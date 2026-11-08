## Why

`BackupQueueDateTime` and `BackupQueueDate` cannot remove identical backups: `BackupQueue$prune_identical()` is abstract and unimplemented, and `prune_identical_backups()` falls back to `BackupQueueDateTime` whenever the index queue has no numbered backups. Calling `prune_identical_backups()` on timestamped or datestamped backups that reach that fallback (for example the default `%Y-%m-%d--%H-%M-%S` and `%Y-%m-%d` formats) therefore fails instead of deduplicating them.

## What Changes

- Implement `prune_identical()` on `BackupQueueDateTime`, inherited by `BackupQueueDate`. It keeps the newest backup of each distinct file content, removes older duplicates with the dry-run-aware `file_remove()`, refreshes the backups cache and returns `self`, without renumbering timestamped filenames.
- Keep `BackupQueue$prune_identical()` abstract as `NotImplementedError` and leave `BackupQueueIndex$prune_identical()`'s renumbering behavior untouched.
- Add class-level and end-to-end testthat coverage, including newest-copy retention, distinct content, dry-run behavior and `prune_identical_backups()` for both queue types.
- Add a `NEWS.md` bullet, bump the development version, fix the copied `prune_identical_backups()` roxygen description, and regenerate the roxygen docs.

## Capabilities

### New Capabilities

- `backup-pruning`: behavior of pruning identical timestamped and datestamped backups, including newest-copy retention, dry-run semantics and the `prune_identical_backups()` entry point.

### Modified Capabilities

<!-- No existing specs in openspec/specs/; no requirement changes to existing capabilities. -->

## Impact

- `R/BackupQueue.R`: new `BackupQueueDateTime$prune_identical()` (inherited by `BackupQueueDate`).
- `R/rotate.R`: corrected roxygen description for `prune_identical_backups()`.
- `tests/testthat/test_BackupQueue.R` and `tests/testthat/test_rotate.R`: new tests.
- `DESCRIPTION`, `NEWS.md`, `man/*.Rd`: development version, changelog entry and regenerated documentation.
- No change to `BackupQueueIndex$prune_identical()` or to the abstract `BackupQueue$prune_identical()`.
