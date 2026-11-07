# GitHub Issue Style Guide

Stable form rules for issues and comments in the project's GitHub issue tracker. The parent `SKILL.md` is the procedure; read this guide before composing or editing an issue or comment. AGENTS.md wins where it defines project-specific policy. Label resolution and semantics live in `references/labels.md`.

## Notation and Hygiene

- `file:line` plus the enclosing symbol for code (lines rot; symbols are greppable — e.g. `path/to/file` line 138, symbol `parse_amount`); doc name + `§` for binding docs.
- No token values; no personal data beyond what the fix needs.

## Issue Title

`<scope>: <what>` — a scope, a colon, one space, then the what.

- `<scope>` names where the work lands: the project's own module, layer, surface, document or skill. One lowercase word, hyphenated if compound (`cash-ledger`); no underscore, no code formatting. Reuse a scope the open issues already use; a new one takes the project's own word for that area (the code layout or the binding vocabulary), not an invented synonym. Cross-cutting work gets its own short scope (`chore`, `hygiene`, `tooling`); the generic word `area` is never the scope.
- The what front-loads the point and starts lower case unless its first word is a proper noun or acronym. Use an imperative for action work (`run: add preflight`), a declarative statement when the title pins required behavior (`money: parse_amount must accept the documented format`), and a question mark only for an open ruling question; a ruling still takes the affected scope and the resolved needs-ruling label (default `needs-ruling`; `Ruling:` is not a scope), and the title has no trailing period.
- The scope is separated by exactly `: `. An em dash, ` - ` or a second colon never replaces it, and the scope is never wrapped in backticks: `area:run — add the preflight check` is wrong; `run: add the preflight check` is right. A detail that would otherwise add a second colon goes in parentheses: `run: implement landing (push, PR, required checks, merge)`.
- The scope is mandatory on every agent-authored issue; only a human-written draft may lack one, and rewriting it to house style adds it.

## Issue Body

Use the project's section skeleton (default: `**Abstract**` / `**Problem**` / `**Fix**` / `**Rulings**` / `**Done when**` / `**Out of scope**`). Delete sections that stay empty before filing. Length is a symptom, not a rule: keep the body as long as the evidence requires for one coherent unit, and use [Issue Sizing](#issue-sizing) to decide whether to split.

### Abstract

Two or three plain-language sentences for a human skimming the issue: what is wrong or missing, why it matters, and what this issue changes. No `file:line` references, checklists, test commands or verification stamp. Title + abstract must make sense on their own; `Done when` must make sense with the abstract.

For a deliberate batch, state the shared acceptance path in one sentence: "This batch fixes six blockers that share one acceptance path: `doctor` and `preview` succeed against a provisioned target." Then use a short table in `Fix` when the parts are independent enough to list.

### Problem

The detail layer below the abstract: what is wrong or missing, why it matters, and the evidence a reviewer needs. Reference code and docs per **Notation and Hygiene**. Close the Problem with the verification stamp `verified YYYY-MM-DD (model: <model_id>)` — use `repro` instead of `verified` when a run backs the claim. The stamp is the issue's signature and is required on every agent-authored issue, including a feature issue that only inspected the code; there is no end-of-body signature line.

### Fix

A concrete task list (`- [ ]`); name the required spec delta inline (project notation, e.g. `Δspec: <capability>`). `Fix` is the approach; `Done when` is the acceptance. Do not repeat the same bullets in both. For a batch, group the tasks or use a table instead of splitting the issue by line count.

### Rulings

⚖ only decisions that need the owner: options + one recommendation; the resolved needs-ruling label (default `needs-ruling`) while undecided. A naming or vocabulary ruling states its constraint as every settled sense the new word must not collide with — the code-level senses and the project's binding vocabulary: a word that is free in the code can still contradict the binding table, and the owner must decide against the full set.

### Done when

Machine-checkable acceptance: tests that must exist/pass, commands to run, doc bumps. A bug names the regression test that fails before the fix.

### Out of scope

Only when a reader (human or agent) might plausibly wander there.

## Issue Sizing

The unit is the largest coherent change one pipeline cycle can deliver: one `propose → review → apply` pass, one spec delta, one PR and one verification path. A new issue costs a full cycle, so do not split for line count.

Keep together when:

- the parts share one outcome or root cause;
- they need one spec delta or capability change;
- they share one verification path;
- one PR can land them and one review can judge them;
- the batch fits the pipeline's issue timeout and context budget.

Split when:

- the parts can land independently;
- they need separate decisions or spec deltas;
- their checks, rollback boundaries or risk differ;
- the batch cannot finish in one cycle.

A batch issue must state the shared acceptance path in the abstract; use a table or grouped bullets in `Fix` for the parts. A separate issue should reduce review risk or pipeline cost. If it only adds a cycle, fold it back into the parent. The pipeline's issue timeout is the practical upper bound: split when the batch cannot finish in one cycle, not when the body is long.

## Before Filing

- Title + abstract + `Done when` stand alone; the reader does not need `Problem` to understand the unit.
- The abstract has no `file:line` references, checklist, command or verification stamp.
- A batch names its shared acceptance path; a split is justified by independent outcomes, separate decisions, different risk boundaries or cycle overflow — not line count.
- `Fix` and `Done when` do not repeat each other.

## Labels

Label resolution and semantics live in `references/labels.md`. Read it before applying, changing, or validating a label: it defines the default set, how the project's AGENTS.md overrides or forbids labels, and how repository label descriptions extend the set. Never invent label semantics here.

## Comment

The post-solve comment reports verification and deltas only. It is terse and scannable, written for both humans and AI coding agents. Do not repeat the issue text, the spec, or the task list. Use the notation above; omit sections that have nothing to say. Decisions are recorded per the project's conventions (e.g. the issue's Rulings section) — not buried in the comment.

### Template

Implemented in change `<id>`.

- Shipped: <1–3 bullets — observable behavior + key files>
- Checks: <test/lint command(s) + result>
- Rulings: <decisions settled during implementation, recorded in the body — omit if none>
- Deviations: <done differently from the spec/tasks and why — omit if none>
- Follow-ups: #<n>, #<n> — omit if none (file follow-up work as new issues, never leave TODOs)

Always end with the traceability line (never omitted for this comment): `_<model_name> · <model_version>_` — see **Signature and Traceability**.

## Signature and Traceability

- **Substantive** content — comments with findings, review results, implementation plans, or implementation reports — ends with one plain-text line: `_<model_name> · <model_version>_` (only metadata actually known — never guessed). Take the pair from the session environment (the model name and exact model ID given to you); never infer it or copy it from prior commits, logs, or another session. Detailed execution metadata stays in agent logs, not GitHub.
- **Routine** posts (progress pings, clarification questions, CI/test status) stay unsigned.
- **Issues** are signed by the Problem verification stamp (model + date) — no separate end-signature.
- **Archiving commits** keep the trailer `model: …; version: …`.
- Never edit a human-authored comment without being asked — exception: draft issues. Never double-sign when editing or retrying a comment.
