#!/usr/bin/env python3
"""GitHub REST helper for the issue registry (see SKILL.md for usage).

One GraphQL operation is supported — `markPullRequestReadyForReview` — because
REST has no draft toggle. Everything else stays on REST.

This is a standard-library script that lives in the skill's `scripts/`
directory, not the `gh` CLI: no `gh` binary is installed for this workflow and
none is assumed. Invoke it by path with `python3`; it is not executable and not
on `PATH`.
"""

from __future__ import annotations

import argparse
import http.client
import json
import os
import random
import re
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

API_BASE = "https://api.github.com"
API_VERSION = "2022-11-28"
USER_AGENT = "github-issue-skill/github-rest.py"
REQUEST_TIMEOUT = 30.0
RETRY_ATTEMPTS = 3
RETRY_BASE_DELAY = 0.5
RETRY_JITTER = 0.5
RETRY_MAX_DELAY = 8.0
RETRY_MAX_AFTER = 60.0
GRAPHQL_METHOD = "GRAPHQL"
GRAPHQL_PATH = "/graphql"
METHODS = ("GET", "POST", "PATCH", "PUT", "DELETE", GRAPHQL_METHOD)
IDEMPOTENT_METHODS = ("GET", "PUT", "PATCH", "DELETE")


class HelperError(RuntimeError):
    """A terminal, user-facing failure; `main` reports it and exits nonzero."""


@dataclass(frozen=True)
class Response:
    """One successful response: the body to print and the headers to inspect."""

    body: bytes
    headers: dict[str, str]


def token_file() -> Path:
    """Operator token file, resolved per call so XDG_CONFIG_HOME stays overridable."""
    base = os.environ.get("XDG_CONFIG_HOME")
    config_dir = Path(base) if base else Path.home() / ".config"
    return config_dir / "issue-pipeline" / "github-token"


def _read_token_file(path: Path) -> str:
    """Read a plain token or an `AGENT_GH_TOKEN=...` line, without echoing it."""
    try:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
    except OSError:
        return ""
    lines = [
        line.strip()
        for line in text.splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    for line in lines:
        if line.startswith("AGENT_GH_TOKEN="):
            return line.split("=", 1)[1].strip().strip("\"'")
    return lines[0] if len(lines) == 1 else ""


def resolve_token(slug: str) -> str:
    """Resolve the token without ever putting it in an error or a log line."""
    tok = os.environ.get("AGENT_GH_TOKEN")
    if tok:
        return tok
    tok = _read_token_file(token_file())
    if tok:
        return tok
    var = re.sub(r"[-.]", "_", slug.split("/")[-1]).upper() + "_GH_TOKEN"
    for name in (var, "GH_TOKEN", "GITHUB_TOKEN"):
        tok = os.environ.get(name)
        if tok:
            return tok
    return ""


def _repo_slug() -> str:
    """Derive owner/repo from `git remote get-url origin`."""
    try:
        completed = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            capture_output=True,
            text=True,
        )
    except OSError as err:
        raise HelperError(f"cannot run git: {err.strerror}") from err
    url = completed.stdout.strip()
    slug = re.sub(r"\.git$", "", re.sub(r"^(git@github\.com:|https://github\.com/)", "", url))
    if "/" not in slug:
        raise HelperError("cannot derive owner/repo from git remote origin")
    return slug


def _parse_args(argv: Sequence[str] | None) -> argparse.Namespace:
    """Parse the command line; `--help` explains the method and payload rules."""
    parser = argparse.ArgumentParser(
        prog="github-rest.py",
        description=(
            "Call the GitHub REST API for the repository containing the current "
            "directory, or the one GraphQL draft-ready mutation. This is a "
            "script from the github-issue skill, not the `gh` CLI."
        ),
    )
    parser.add_argument(
        "--paginate",
        action="store_true",
        help="follow Link rel=next on a GET list endpoint and print one JSON array",
    )
    parser.add_argument(
        "method",
        choices=METHODS,
        help="HTTP method, or GRAPHQL for the draft-ready mutation",
    )
    parser.add_argument(
        "path",
        help="API path starting with /, for example /issues/42; GRAPHQL uses /graphql",
    )
    parser.add_argument(
        "payload",
        nargs="?",
        help='JSON file sent verbatim, or a text file wrapped as {"body": <text>}; '
        'GRAPHQL requires a JSON file with a "query"',
    )
    return parser.parse_args(argv)


def _load_payload(path_text: str | None, method: str) -> str | None:
    """Build the request body from an optional JSON or text file.

    REST writes keep the text-file shorthand; a GraphQL payload is always a
    JSON object with a string `query` and optional object `variables`.
    """
    if path_text is None:
        return None
    path = Path(path_text)
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as err:
        raise HelperError(f"cannot read payload {path}: {err.strerror}") from err
    if method != GRAPHQL_METHOD:
        return text if path.suffix == ".json" else json.dumps({"body": text})
    if path.suffix != ".json":
        raise HelperError('GRAPHQL payload must be a .json file with a "query"')
    try:
        data = json.loads(text)
    except json.JSONDecodeError as err:
        raise HelperError(f"GRAPHQL payload {path} is not valid JSON: {err.msg}") from err
    if not isinstance(data, dict) or not isinstance(data.get("query"), str):
        raise HelperError('GRAPHQL payload must be a JSON object with a string "query"')
    variables = data.get("variables")
    if variables is not None and not isinstance(variables, dict):
        raise HelperError('GraphQL "variables" must be a JSON object')
    return text


def _retry_after(err: urllib.error.HTTPError) -> int | None:
    """Delay from an integer `Retry-After` header; None when absent or an HTTP-date."""
    raw = err.headers.get("Retry-After") if err.headers is not None else None
    if raw is None:
        return None
    try:
        return max(0, int(raw))
    except ValueError:
        return None


def _is_rate_limited(err: urllib.error.HTTPError) -> bool:
    """A 429, or a 403 that GitHub marks as a rate limit rather than a permission."""
    if err.code == 429:
        return True
    if err.code != 403 or err.headers is None:
        return False
    return (
        err.headers.get("Retry-After") is not None
        or err.headers.get("X-RateLimit-Remaining") == "0"
    )


def _sleep_backoff(attempt: int, retry_after: int | None) -> None:
    """Sleep before the next attempt: honored Retry-After, else jittered backoff."""
    if retry_after is not None:
        time.sleep(min(float(retry_after), RETRY_MAX_AFTER))
        return
    delay = min(RETRY_BASE_DELAY * (2**attempt), RETRY_MAX_DELAY)
    time.sleep(delay + random.uniform(0.0, RETRY_JITTER))


def _http_error_message(err: urllib.error.HTTPError) -> str:
    """One-line error for a terminal HTTP status; the token lives only in headers."""
    try:
        payload = json.loads(err.read().decode("utf-8", errors="replace"))
    except json.JSONDecodeError, UnicodeDecodeError, OSError:
        return f"HTTP {err.code}: {err.reason}"
    message = payload.get("message") if isinstance(payload, dict) else None
    if isinstance(message, str) and message:
        return f"HTTP {err.code}: {message}"
    return f"HTTP {err.code}: {err.reason}"


def _unsafe_retry_message(method: str, reason: str) -> str:
    """Explain why a non-idempotent write is not retried."""
    return (
        f"{reason} during {method}; not retried because a retry could duplicate "
        "the write — check whether it landed before retrying"
    )


def _url(slug: str, path: str) -> str:
    """API URL for one repository-relative path."""
    return API_BASE + "/repos/" + slug + path


def _header(headers: dict[str, str], name: str) -> str | None:
    """Case-insensitive header lookup over a plain dict."""
    wanted = name.lower()
    for key, value in headers.items():
        if key.lower() == wanted:
            return value
    return None


def _next_link(headers: dict[str, str]) -> str | None:
    """Extract the `rel="next"` URL from a Link header, if one is present."""
    link = _header(headers, "Link")
    if not link:
        return None
    for part in link.split(","):
        if 'rel="next"' not in part:
            continue
        match = re.search(r"<([^>]+)>", part)
        if match:
            return match.group(1)
    return None


def _same_api_host(url: str) -> bool:
    """True when a pagination URL points at the configured API host."""
    base = urllib.parse.urlsplit(API_BASE)
    target = urllib.parse.urlsplit(url)
    return target.scheme == base.scheme and target.netloc == base.netloc


def _request_url(url: str, token: str, method: str, payload: str | None) -> Response:
    """Send one request with bounded transient retry; return the response.

    Transient: rate limits (429, or 403 with a rate-limit marker) for every
    method, and 5xx or transport errors for idempotent methods. A POST is never
    retried after a 5xx or a transport error, because the write may have landed;
    retrying it could duplicate an issue or a comment. Every other status —
    including a native 404 and a permission 403 — is terminal.
    """
    headers = {
        "Authorization": "Bearer " + token,
        "Accept": "application/vnd.github+json",
        "User-Agent": USER_AGENT,
        "X-GitHub-Api-Version": API_VERSION,
    }
    if payload is not None:
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(
        url,
        data=payload.encode("utf-8") if payload is not None else None,
        headers=headers,
        method=method,
    )
    idempotent = method in IDEMPOTENT_METHODS
    attempt = 0
    while True:
        try:
            resp = urllib.request.urlopen(req, timeout=REQUEST_TIMEOUT)
            try:
                return Response(resp.read(), dict(resp.headers.items()))
            finally:
                resp.close()
        except urllib.error.HTTPError as err:
            rate_limited = _is_rate_limited(err)
            if not rate_limited and err.code < 500:
                raise HelperError(_http_error_message(err)) from err
            if not rate_limited and not idempotent:
                raise HelperError(_unsafe_retry_message(method, f"HTTP {err.code}")) from err
            if attempt + 1 >= RETRY_ATTEMPTS:
                raise HelperError(
                    f"HTTP {err.code} persists after {RETRY_ATTEMPTS} attempts"
                ) from err
            _sleep_backoff(attempt, _retry_after(err))
            attempt += 1
        except (OSError, http.client.HTTPException) as err:
            if not idempotent:
                raise HelperError(_unsafe_retry_message(method, f"network error: {err}")) from err
            if attempt + 1 >= RETRY_ATTEMPTS:
                raise HelperError(
                    f"network error persists after {RETRY_ATTEMPTS} attempts: {err}"
                ) from err
            _sleep_backoff(attempt, None)
            attempt += 1


def _request(
    slug: str,
    token: str,
    method: str,
    path: str,
    payload: str | None,
) -> Response:
    """Send one repository-relative REST request, or the GraphQL call."""
    if method == GRAPHQL_METHOD:
        if path.rstrip("/") != GRAPHQL_PATH:
            raise HelperError(f"GRAPHQL uses path {GRAPHQL_PATH}")
        return _request_url(API_BASE + GRAPHQL_PATH, token, "POST", payload)
    return _request_url(_url(slug, path), token, method, payload)


def _paginate(slug: str, token: str, path: str) -> bytes:
    """Follow Link rel=next on a GET list endpoint and return one JSON array."""
    url = _url(slug, path)
    items: list[object] = []
    while True:
        response = _request_url(url, token, "GET", None)
        data = _decode_json(response.body)
        if not isinstance(data, list):
            return response.body
        items.extend(data)
        next_url = _next_link(response.headers)
        if next_url is None:
            return json.dumps(items).encode("utf-8")
        if not _same_api_host(next_url):
            raise HelperError(f"refusing to follow a pagination link outside {API_BASE}")
        url = next_url


def _decode_json(body: bytes) -> object | None:
    """Decode a response body; an empty body decodes to None."""
    if not body:
        return None
    try:
        return json.loads(body)
    except json.JSONDecodeError as err:
        raise HelperError(f"invalid JSON response: {err.msg}") from err


def _check_graphql_response(body: bytes) -> None:
    """Fail on a GraphQL `errors` array even when HTTP returned 200."""
    data = _decode_json(body)
    if not isinstance(data, dict):
        raise HelperError("GraphQL response is not a JSON object")
    errors = data.get("errors")
    if errors:
        messages: list[str] = []
        if isinstance(errors, list):
            for item in errors:
                if isinstance(item, dict) and isinstance(item.get("message"), str):
                    messages.append(item["message"])
        detail = "; ".join(messages) if messages else "unknown error"
        raise HelperError(f"GraphQL error: {detail}")
    if data.get("data") is None:
        raise HelperError("GraphQL response has no data")


def _print_result(method: str, body: bytes) -> None:
    """Print the JSON response body; `done` when a write returns no body.

    Writes — including the GraphQL mutation — print the created or updated
    resource, so the caller can confirm the result without a follow-up GET. A
    write has already landed by the time its response is printed, so an empty
    or unparseable body prints `done` instead of failing the call. A GET keeps
    the strict JSON contract.
    """
    if method == "DELETE":
        print("done")
        return
    try:
        data = _decode_json(body)
    except HelperError:
        if method == "GET":
            raise
        data = None
    if data is not None:
        print(json.dumps(data, indent=2))
    elif method != "GET":
        print("done")


def main(argv: Sequence[str] | None = None) -> int:
    """Run one GitHub call; return 0 on success and 1 on a terminal failure."""
    args = _parse_args(argv)
    try:
        if args.method == GRAPHQL_METHOD and args.payload is None:
            raise HelperError('GRAPHQL requires a JSON payload with a "query"')
        slug = _repo_slug()
        token = resolve_token(slug)
        if not token:
            raise HelperError(
                f"no token: set AGENT_GH_TOKEN, write {token_file()} (one line, "
                "chmod 600), set the per-repository <REPO>_GH_TOKEN, or set "
                "GH_TOKEN/GITHUB_TOKEN"
            )
        if args.paginate:
            if args.method != "GET":
                raise HelperError("--paginate is only valid for GET")
            body = _paginate(slug, token, args.path)
        else:
            payload = _load_payload(args.payload, args.method)
            body = _request(slug, token, args.method, args.path, payload).body
            if args.method == GRAPHQL_METHOD:
                _check_graphql_response(body)
        _print_result(args.method, body)
    except HelperError as err:
        print(err, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
