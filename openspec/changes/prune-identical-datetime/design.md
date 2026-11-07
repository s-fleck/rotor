## Context

`BackupQueue` (`R/BackupQueue.R`) is an R6 hierarchy for locating, rotating and pruning backups. `BackupQueueIndex` handles numbered backups (`foo.1.ext`, `foo.2.ext`) and implements `prune_identical()` by MD5 comparison plus renumbering. `BackupQueueDateTime` handles timestamped backups and `BackupQueueDate` inherits it for datestamped ones; both leave `prune_identical()` abstract, so the inherited method calls `NotImplementedError()`.

`prune_identical_backups()` (`R/rotate.R`) builds a `BackupQueueIndex` and falls back to `BackupQueueDateTime` when no numbered backups exist. Because datestamps parse as datetimes, the fallback serves both timestamped and datestamped backups. That fallback currently fails because `BackupQueueDateTime` does not implement `prune_identical()`.

## Goals / Non-Goals

**Goals:**

- Implement `prune_identical()` for `BackupQueueDateTime`, inherited by `BackupQueueDate`.
- Keep the newest backup of each distinct content, remove older duplicates through the dry-run-aware `file_remove()`, refresh the cache and return `self`.
- Make `prune_identical_backups()` succeed for timestamped and datestamped queues, including dry runs.
- Cover the behavior with class-level and end-to-end tests, a `NEWS.md` entry and regenerated roxygen docs.

**Non-Goals:**

- Changing `BackupQueueIndex$prune_identical()` or its renumbering behavior.
- Implementing a generic `BackupQueue$prune_identical()` for direct use of the abstract class.
- Fixing the pre-existing dry-run defect in `BackupQueueIndex$prune_identical()`.
- Fixing the duplicate `NotImplementedError()` definition in `R/conditions.R`.

## Decisions

### Decision 1: NEWS and version convention

- **Scenario:** The issue asks for a `NEWS.md` bullet, but the file starts at the released `# rotor 0.3.7` heading and has no development section.
- **Options:** (a) append the bullet under the released 0.3.7 heading; (b) open a development section and bump the patch-dev version.
- **Choice:** (b) — set `Version: 0.3.7.9000` in `DESCRIPTION` and prepend `# rotor (dev)` with the bullet.
- **Discarded:** (a) would attribute an unreleased change to the 0.3.7 release.
- **Evidence:** commit `33a69c3` did exactly this after 0.3.5 (`Version: 0.3.5` -> `0.3.5.9000`, new `# rotor (dev)` section); `f6a2b5b` added bullets under that section; the release commit later bumped the version and renamed the heading.

### Decision 2: Roxygen regeneration

- **Scenario:** The new method needs generated docs; installed roxygen2 is 8.0.0 while `DESCRIPTION` records `RoxygenNote: 7.2.1.9000`.
- **Options:** (a) regenerate with the installed 8.0.0; (b) pin 7.2.1.9000; (c) hand-edit the `.Rd` files.
- **Choice:** (a) — regenerate with 8.0.0 and include the resulting `man/*.Rd` rewrite plus the metadata change to `Config/roxygen2/version: 8.0.0`.
- **Discarded:** (b) that version is not installed/not in the support matrix; (c) `man/*.Rd` carry the "do not edit by hand" header.
- **Evidence:** `roxygen2::roxygenise()` on a workspace copy rewrote eight man pages, changed `RoxygenNote` to `Config/roxygen2/version: 8.0.0`, left `NAMESPACE` unchanged, and exited 0; the generated-file headers forbid manual edits.

### Decision 3: Method placement

- **Scenario:** Which class should own the new implementation.
- **Options:** (a) `BackupQueueDateTime` only, inherited by `BackupQueueDate`; (b) implement in the abstract `BackupQueue`; (c) override separately in both DateTime and Date.
- **Choice:** (a).
- **Discarded:** (b) the issue explicitly keeps the abstract method as `NotImplementedError`; (c) duplicate code for identical timestamp semantics.
- **Evidence:** the issue's Fix bullets; `BackupQueueDate <- R6::R6Class(inherit = BackupQueueDateTime, ...)` at `R/BackupQueue.R:768-770`; `prune_identical_backups()` falls back to `BackupQueueDateTime` when `BackupQueueIndex$has_backups` is false (`R/rotate.R:344-347`). The default timestamp (`%Y-%m-%d--%H-%M-%S`) and datestamp (`%Y-%m-%d`) suffixes contain dashes, so `BackupQueueIndex$files` does not claim them (`R/BackupQueue.R:464`) and the fallback reaches both. All-digit formats such as `%Y%m%d` are claimed by the index and are out of scope.

### Decision 4: Deduplication algorithm

- **Scenario:** How to identify and delete older identical backups without renumbering.
- **Options:** (a) newest-first `$files` + `tools::md5sum()` + `duplicated()` + `file_remove()` + cache refresh + return `self`; (b) reuse `BackupQueueIndex`'s renumbering path; (c) pairwise `identical()` comparisons.
- **Choice:** (a).
- **Discarded:** (b) would rename timestamped files and is out of scope; (c) MD5 comparison is the documented contract and handles compressed backups.
- **Evidence:** `BackupQueueDateTime$update_backups_cache()` orders `timestamp` decreasing (`R/BackupQueue.R:651-666`), so the first occurrence of each MD5 is the newest copy and `duplicated()` selects older duplicates; the reference `BackupQueueIndex$prune_identical()` uses `tools::md5sum()` and `duplicated()` and returns `self` (339-366); `file_remove()` is dry-run aware (`R/utils-fs.R:55-67`); a workspace proof-of-concept kept the newest duplicate for both DateTime and Date and left files in place under `DRY_RUN`.

### Decision 5: Test placement

- **Scenario:** Where the new tests belong.
- **Options:** (a) class-level tests in `test_BackupQueue.R` plus end-to-end tests in `test_rotate.R`; (b) all tests in `test_rotate_date.R`.
- **Choice:** (a).
- **Discarded:** (b) direct class behavior belongs with the existing BackupQueue class tests, while the functional helper already has its own prune test in `test_rotate.R`.
- **Evidence:** existing `$prune_identical` tests live at `tests/testthat/test_BackupQueue.R:551` and `tests/testthat/test_rotate.R:129`; DateTime/Date push/prune tests already live in `test_BackupQueue.R`.

## Risks / Trade-offs

- [MD5 comparison reads every backup file] -> Acceptable: the existing `BackupQueueIndex` contract already uses `tools::md5sum()`, and backups are bounded by `max_backups`.
- [Cache refresh before reading may mask externally added backups] -> Intentional: it mirrors `BackupQueue$prune()`'s "to be safe" refresh and `BackupQueueIndex`'s live read; the cache is refreshed again after removals.
- [Empty-queue removals could emit an empty message] -> Mitigated by returning early when fewer than two backups exist.
- [Regenerating docs with roxygen 8 rewrites unrelated man pages] -> Accepted by Decision 2; `NAMESPACE` is unchanged and the pre-existing `Cache` documentation warnings are out of scope.

## Migration Plan

No migration is required. `prune_identical_backups()` keeps its signature and behavior for indexed backups; the change only adds behavior for timestamped and datestamped queues.

## Open Questions

None.

## Finding Classification

| Finding | Classification | Disposition |
| --- | --- | --- |
| `R/conditions.R` defines `NotImplementedError()` twice (1-8, 59-67); the later definition requires `dir`, so the abstract method raises `argument "dir" is missing` rather than the intended message. | Out of scope | Do not edit `conditions.R` in this unit. Pre-archive triage groups it as a follow-up. |
| `BackupQueueIndex$prune_identical()` uses `unlink()`/`file.rename()` and is not dry-run aware (`R/BackupQueue.R:350`, `:363-364`); `prune_identical_backups(..., dry_run = TRUE)` can still delete indexed backups. | Out of scope | Different method/class and explicitly out of scope; pre-archive triage groups it as a follow-up. |
| The Done-when bullet "Dry-run leaves all backups in place" reads as covering the indexed path too. | Auto-fixable (wording) | Narrow the interpretation to the DateTime/Date path (the Fix section already specifies `file_remove()` for the new method); recorded in the specs and tasks. No code or issue-body edit. |
| `prune_identical_backups()` roxygen description is a copy of `prune_backups()`'s text and mentions `max_backups`. | Auto-fixable (docs/wording) | Fixed in `R/rotate.R` during `apply` and regenerated into `man/rotate.Rd`; no runtime change. Task 3.1. |
| roxygen 8 emits pre-existing `Cache`/`DirectoryQueue` documentation warnings. | Out of scope (tooling noise) | Expected when regenerating with the installed roxygen2; not fixed in this unit. |
| `prune_identical_backups()` treats all-digit date/timestamp formats (`%Y%m%d`, `%Y%m`, `%Y`) as indexed backups and routes them to `BackupQueueIndex`, whose renumbering path is not dry-run aware. | Out of scope | The routing rule is pre-existing Index behavior and changing it is an explicit non-goal; the spec and tasks cover the default `%Y-%m-%d--%H-%M-%S` and `%Y-%m-%d` formats. Pre-archive triage groups it with the Index dry-run follow-up. |

## Spec Review

**Status:** Approved (second review)

**Reviews run:** two. The first review returned Issues Found and the second returned Approved.

**Applied fixes:**

- Narrowed the `prune_identical_backups()` requirement and scenarios to the formats that actually reach the `BackupQueueDateTime` fallback (default `%Y-%m-%d--%H-%M-%S` and `%Y-%m-%d`), stated that all-digit formats are claimed by `BackupQueueIndex` and out of scope, and recorded that routing as an out-of-scope finding.
- Corrected Decision 3's evidence: fallback selection is based on `!BackupQueueIndex$has_backups` (`R/rotate.R:344-347`) and the dash-containing default suffixes (`R/BackupQueue.R:464`), not on `is_parsable_datetime()` alone.
- Added a roxygen `#' @description` for the new public method to task 1.1 and noted the inherited-method link move in `man/BackupQueueDate.Rd`.
- Changed task 2.1 to assert on `unname(tools::md5sum(...))`, matching the existing test style.
- Corrected the Index dry-run citation to `R/BackupQueue.R:350` and `:363-364`.
- Added tasks 2.6 and 2.7 for the zero/one-backup and all-distinct scenarios and for cache refresh with `cache_backups = TRUE`.
- Added the `assert_pure_BackupQueue()` precondition to the helper requirement.

**Leftover advisory items:** none.

**Blocking findings:** none. Every finding from the review marker comment and the refine brief is recorded in `## Finding Classification` above.

**Docs sync:** `AGENTS.md` defines only the verification command (`Rscript -e 'testthat::test_local()'`); it defines neither a docs-sync review nor a doc checker, so neither applies. The change's own docs work (roxygen regeneration, `NEWS.md`) is covered by tasks 3.1-3.3.
