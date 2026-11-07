---
name: issue-pipeline-github-issue
description: Use when filing, drafting, or reformatting a GitHub issue — turning notes or discussion into an issue, filing follow-up work from a solved issue, or checking an issue body against house style — and when maintaining an issue's body or labels, posting or editing the post-solve implementation comment, or applying the signature and traceability policy to agent-authored GitHub content. Carries the self-contained GitHub helper (`scripts/github-rest.py`: REST plus one GraphQL draft-ready mutation — a skill script, not the `gh` CLI); label resolution and read-only triage signals live in `references/labels.md`.
metadata:
  author: Stefan Fleck
  version: "9.0.0+autocode.1"
  variant: autocode
  upstream: github-issue@9.0.0
  upstream_commit: 39b1ce32923a7f22cfca8ba82274e42a2107b728
  local_changes: "Namespace prefix and bundle-relative paths only"
---

# GitHub Issue

Compose and maintain issues in the project's GitHub issue registry, and post the comments that close the loop. AGENTS.md rules apply (registry policy wins); this skill is the procedure. Read `references/labels.md` before applying, changing, or checking any label — it owns the label resolution order (defaults, AGENTS.md, repository labels), the project's write and read deny rules, and the read-only triage signals (`wontfix`/`invalid`/`duplicate`/`question`, project blockers, ready gates, and classification hints) that decide whether work may start.

Read `references/style-guide.md` before composing or editing an issue or comment. It holds the stable form contract — title grammar, body skeleton, comment template, signature policy, and reference notation. Label semantics live in `references/labels.md`. Do not restate either here.

## File an Issue

1. Search the open issues first (`scripts/github-rest.py --paginate GET "/issues?state=open&per_page=100"`, ignoring entries with a `pull_request` field): if the same scope and topic exists, refine that issue or link it (`Refines #N`, `Supersedes #N`) instead of filing a duplicate.
2. Compose the title and body to the style guide.
3. Verify the body's load-bearing claims against the current codebase before filing:
   - Refs must resolve; re-check old refs when touching an existing issue.
   - Verify behavior empirically where cheap, then stamp the Problem per the style guide.
   - State the measured numbers, not the expected direction: a mechanism can move a figure up or down, so a claim like "the figures dip" needs its sign checked against the real data.
   - A term's footprint is a whole-surface measurement, not a file list: sweep the source tree, tests, and binding docs with a grep and quote the measured hits — a term that lives in a fixture, a schema, or a helper script is part of the footprint.
4. Resolve the labels per `references/labels.md` before composing the final output, and apply the applicable write-role labels when filing (`draft` is human-applied, never self-applied); never apply a read-only triage signal. If a needed label lacks metadata, ask now. Do not choose names from memory.
5. When composing in chat, output the issue markdown only — no commentary around it. The label question round happens before this output, not inside it.

## Maintain an Issue

- `PATCH /issues/<N>` replaces the whole body: re-fetch immediately before editing and keep everything unrelated verbatim — never PATCH from a cached body.
- Rulings (⚖) are decided by editing the issue's Rulings section; follow-up work becomes new issues.
- Remove the resolved needs-ruling label (default `needs-ruling`) once every ruling is decided; resolve names via `references/labels.md`.
- Never add, change, or remove a read-only triage label (`wontfix`, `invalid`, `duplicate`, `question`, `stale`, project blockers/ready gates, or classification hints) on your own initiative. Read them as disposition signals per `references/labels.md`; an explicit user instruction to change one is a one-off, not a role.

## Post the Post-Solve Comment

One comment per solved issue. Post after implementation and checks pass, before archiving/closing. Use the template and signature rules in the style guide.

If implementation settled any ⚖ decisions, record them in the issue's Rulings section first — decisions are not buried in the comment.

Skip the comment when it would add nothing beyond the issue text and the diff: mechanical change, spec followed exactly, no rulings, deviations, or follow-ups.

## GitHub Access: `scripts/github-rest.py`, not the `gh` CLI

`scripts/github-rest.py` (in this skill's `scripts/` directory) is the project's only GitHub client. There is no `gh` CLI dependency: no `gh` binary is installed for this workflow and none is needed. The helper is a standard-library Python script, not executable and not on `PATH` — always call it by path with `python3`; `--help` lists the methods, `--paginate`, and the payload rules. The paths below show a vendored copy; from the canonical repo use `skills/github-issue/scripts/github-rest.py`. It uses REST for everything except the one draft-ready GraphQL mutation below, because REST has no draft toggle.

It derives `owner/repo` from `git remote get-url origin` and resolves the token from `AGENT_GH_TOKEN`, then `$XDG_CONFIG_HOME/issue-pipeline/github-token` (one line, chmod 600), then a per-repository `<REPO>_GH_TOKEN`, then `GH_TOKEN` and `GITHUB_TOKEN`; the token is never printed or logged. Call it directly — no heredoc:

```bash
python3 .opencode/skills/github-issue/scripts/github-rest.py GET /issues/42
python3 .opencode/skills/github-issue/scripts/github-rest.py --paginate GET "/issues?state=open&per_page=100"
python3 .opencode/skills/github-issue/scripts/github-rest.py POST /issues /tmp/opencode/issue.json                      # create: {"title", "body", "labels"}
python3 .opencode/skills/github-issue/scripts/github-rest.py PATCH /issues/42 /tmp/opencode/issue-42-body.md            # rulings, `Change:` link
python3 .opencode/skills/github-issue/scripts/github-rest.py POST /issues/42/comments /tmp/opencode/issue-42-comment.md
python3 .opencode/skills/github-issue/scripts/github-rest.py DELETE /issues/42/labels/needs-ruling         # resolved needs-ruling label
python3 .opencode/skills/github-issue/scripts/github-rest.py GRAPHQL /graphql /tmp/opencode/pr-ready.json  # {"query", "variables"}; mark a draft PR ready
```

- `--paginate` follows `Link rel=next` for a `GET` list endpoint and prints one JSON array; it refuses a next link outside the API host. Without it, one page is returned.
- Every successful call prints its JSON response; writes print the created or updated resource, so confirm `number`, `html_url`, `state` or the label list from that output instead of issuing a confirmation GET. A write with an empty response prints `done`.
- A file argument ending in `.json` is sent as the raw payload (e.g. creating an issue); any other file is wrapped as `{"body": <text>}` (comments, body updates).
- `GRAPHQL /graphql <payload.json>` is the one exception to REST: it sends `{"query": ..., "variables": ...}` to the GraphQL endpoint, used for `markPullRequestReadyForReview` (a draft PR cannot be merged). The payload must be a JSON object with a string `query` and, when used, an object `variables`. A 200 response containing an `errors` array is a failure, and the mutation is not retried after a 5xx or a network error because it may have landed.
- Transient failures retry automatically; a POST is not retried after a 5xx or a network error because the write may have landed, so check before retrying.
- Token hygiene: the operator token file is `$XDG_CONFIG_HOME/issue-pipeline/github-token` (one line, file 600, directory 700); a project may also provide `<REPO>_GH_TOKEN` in its environment — never write the token value into the repo, workspace files, or logs.

## Lifecycle

- One issue = one shippable unit of work: an agent session or a focused pass can finish it; if not, split it. Issues carry context, not designs — when work on a cluster starts, create the project's change artifact (for OpenSpec, `Change: <id>`), link it in the body, and let the design live there.
- Post-solve summary comments follow **Post the Post-Solve Comment** above.
- Signature and traceability for all agent-authored GitHub content are specified once in the style guide.
- Close with `Fixes #<N>` in the archiving/closing commit.
