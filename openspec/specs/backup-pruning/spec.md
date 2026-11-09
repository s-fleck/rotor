# backup-pruning Specification

## Purpose

Behavior of pruning identical timestamped and datestamped backups, including newest-copy retention, dry-run semantics and the `prune_identical_backups()` entry point.

## Requirements

### Requirement: Timestamped identical backups are pruned

`BackupQueueDateTime$prune_identical()` SHALL remove older backups whose file content is identical to a newer backup, keep exactly one (the newest) backup per distinct file content, refresh the backups cache, and return `self`. It MUST compare content with `tools::md5sum()` and MUST NOT renumber surviving timestamped filenames.

#### Scenario: Mixed identical and distinct backups

- **WHEN** a `BackupQueueDateTime` holds timestamped backups with contents `iris`, `iris`, `cars`, ordered newest first
- **THEN** `$prune_identical()` keeps the newest `iris` backup and the `cars` backup, removes the older `iris` backup, and returns the queue object

#### Scenario: Fewer than two backups

- **WHEN** a `BackupQueueDateTime` holds zero or one backup
- **THEN** `$prune_identical()` removes nothing, emits no removal message, and returns the queue object

#### Scenario: Distinct backups are preserved

- **WHEN** every backup has distinct content
- **THEN** `$prune_identical()` removes no backups and keeps all timestamped filenames unchanged

#### Scenario: Backups cache is refreshed

- **WHEN** the queue keeps a backups cache and `$prune_identical()` removes a duplicate
- **THEN** `$files` reflects only the surviving backups without an explicit refresh

### Requirement: Datestamped queues inherit identical pruning

`BackupQueueDate` SHALL inherit `prune_identical()` from `BackupQueueDateTime` so datestamped backups are deduplicated with the same newest-copy retention and no renumbering.

#### Scenario: Datestamped duplicates

- **WHEN** a `BackupQueueDate` holds datestamped backups with contents `iris`, `iris`, `cars`
- **THEN** the inherited `$prune_identical()` keeps the newest `iris` backup and the `cars` backup and removes the older `iris` backup

### Requirement: Identical pruning is dry-run aware

`BackupQueueDateTime$prune_identical()` SHALL remove files through the dry-run-aware `file_remove()` helper so a dry run reports the intended removals and leaves every backup on disk.

#### Scenario: Dry run leaves backups in place

- **WHEN** `DRY_RUN` is active and duplicates exist
- **THEN** `$prune_identical()` emits the intended-removal message, deletes no files, and leaves the backups unchanged on disk

### Requirement: prune_identical_backups supports timestamped and datestamped queues

`prune_identical_backups()` SHALL deduplicate timestamped and datestamped backups through its `BackupQueueDateTime` fallback, including when `dry_run = TRUE`, for formats whose suffix is not claimed by `BackupQueueIndex` (such as the default `%Y-%m-%d--%H-%M-%S` and `%Y-%m-%d`). All-digit date/timestamp formats (for example `%Y%m%d`) are claimed by `BackupQueueIndex` and are out of scope for this change. The helper SHALL continue to stop when indexed and timestamped backups coexist, as required by `assert_pure_BackupQueue()`.

#### Scenario: Default timestamped queue through the helper

- **WHEN** `prune_identical_backups()` is called on a file whose backups use the default timestamp format and contain duplicate content
- **THEN** the helper keeps the newest copy of each distinct content and returns the file path invisibly

#### Scenario: Default datestamped queue through the helper

- **WHEN** `prune_identical_backups()` is called on a file whose backups use the default datestamp format and contain duplicate content
- **THEN** the helper keeps the newest copy of each distinct content and returns the file path invisibly

#### Scenario: Dry-run through the helper

- **WHEN** `prune_identical_backups(..., dry_run = TRUE)` is called on a default timestamped queue with duplicates
- **THEN** the intended removals are reported and all backup files remain on disk
