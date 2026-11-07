## 1. Implement the method

- [ ] 1.1 Add `prune_identical()` to the `BackupQueueDateTime` public list in `R/BackupQueue.R` after `$prune()` and before `should_rotate()`: refresh the cache, return `self` when fewer than two backups exist, compute `tools::md5sum()` over `$files$path`, select older duplicates with `duplicated()`, remove them with `file_remove()`, refresh the cache and return `self`. Give it a roxygen `#' @description` so the generated method appears on `BackupQueueDateTime` and the inherited-method link in `man/BackupQueueDate.Rd` moves from `BackupQueue$prune_identical` to `BackupQueueDateTime$prune_identical`
- [ ] 1.2 Verify `BackupQueueDate` inherits the method and requires no override; leave `BackupQueue$prune_identical()` as `NotImplementedError()` and `BackupQueueIndex$prune_identical()` unchanged

## 2. Tests

- [ ] 2.1 Add `BackupQueueDateTime: $prune_identical keeps newest duplicate` to `tests/testthat/test_BackupQueue.R`: push `iris` at `2020-01-01`, `iris` at `2020-01-02`, `cars` at `2020-01-03`; assert the return value is the queue, two backups remain, `unname(tools::md5sum($files$path))` is `c(cars_md5, iris_md5)` (newest first), and `$files$sfx` is `c("2020-01-03--00-00-00", "2020-01-02--00-00-00")`
- [ ] 2.2 Add `BackupQueueDate: $prune_identical keeps newest duplicate` using `%Y-%m-%d` stamps; assert `2020-01-03` and `2020-01-02` survive and `2020-01-01` is removed
- [ ] 2.3 Add `BackupQueueDateTime: $prune_identical dry run leaves files`: snapshot with `utils::fileSnapshot(td, md5sum = TRUE)`, activate `DRY_RUN`, expect the `dry_run` message, assert the snapshot is unchanged, then deactivate
- [ ] 2.4 Add `prune_identical_backups works for default timestamped and datestamped backups` to `tests/testthat/test_rotate.R`; assert the surviving MD5s for both queue types through the `BackupQueueIndex` -> `BackupQueueDateTime` fallback
- [ ] 2.5 Add the dry-run variant for `prune_identical_backups(tf, dry_run = TRUE)`: snapshot, expect the `dry_run` message, assert the snapshot is unchanged
- [ ] 2.6 Add tests for the edge scenarios: a `BackupQueueDateTime` with zero or one backup removes nothing and returns the queue silently, and a queue where every backup has distinct content keeps all backups and filenames unchanged
- [ ] 2.7 Add `BackupQueueDateTime: $prune_identical refreshes the backups cache` with `cache_backups = TRUE`: after pruning, assert `$files` reflects the surviving backups without an intervening explicit refresh

## 3. Documentation

- [ ] 3.1 Fix the copied roxygen description on `prune_identical_backups()` in `R/rotate.R` so it describes keeping the newest backup per distinct content instead of `prune_backups()`/`max_backups` (auto-fixable finding from review/refine)
- [ ] 3.2 Set `Version: 0.3.7.9000` in `DESCRIPTION` and prepend `# rotor (dev)` to `NEWS.md` with the `BackupQueueDateTime$prune_identical()` bullet
- [ ] 3.3 Regenerate docs with `Rscript -e 'roxygen2::roxygenise()'`; confirm `NAMESPACE` is unchanged and only the expected `man/*.Rd` files and roxygen metadata change

## 4. Verification

- [ ] 4.1 Run `Rscript -e 'testthat::test_local()'` and confirm all tests pass, including the new cases
- [ ] 4.2 Run `openspec validate --changes` and `openspec status --change prune-identical-datetime`
