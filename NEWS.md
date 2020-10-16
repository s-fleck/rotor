# rotor 0.3.0

* Improved some error messages
* Added `rotate_rds()`, `rotate_rds_time()`, and `rotate_rds_date()` as a
  replacement for `base::saveRDS()` that supports creating backups instead of
  just overwriting the destination file.
* added `Cache`, an R6 class for managing cache directories. The `Cache` API
  is still experimental and might change.
* **breaking** R6 API: renamed some methods and active fields of BackupQueue to
  more universal names:
   - `$push_backup()` -> `$push()`
   - `$backup_dir` -> `dir()`
   - `$backups` -> `$files`
   - `$file` -> `$origin`
* `BackupQueue$prune_identical()` removes identical backups for a BackupQueue
   
  
# rotor 0.2.4

* Fixes unit tests sensitive to year change.


# rotor 0.2.3

* Changed default behavior of `rotate_date()`, `rotate_time()`, etc...: If
  no backups exist of target file, use the "created" timestamp 
  to determine whether rotation should take place or not. 
* `verbose == TRUE` now also displays information on why rotation was NOT 
  triggered.
* added `backup_info()` which is similar to `file.info()` but with additional
  backup related infos.
* removed `"dir"` column from `$backups`/`backup_info()`
  

# rotor 0.2.2

* Reordered the arguments of `rotate_*()` and `backup_*()` for more consistency 
* default `size` for all all `rotate_*()` and `backup_*()` functions is now
  `1` (Byte). This means empty files are never rotated by default.
* added support for `Inf` `size` and `age` (= never rotate)
* More robust regex for discovering backups of files
* R6 API: BackupQueue subclasses gain a `should_rotate(...)` method that 
  determines whether rotation/backup should take place. 
* R6 API: BackupQueueDate and BackupQueueDateTime now have a caching mechanism 
  for backups (defaults to `FALSE`). 
* R6 API: BackupQueue* now use setters/getters for all fields. 


# rotor 0.2.1

* added examples to `rotate()` documentation
* `dry_run` status is now tracked internally instead of a potentially user 
  modifiable `option()` (it was never designed to be user modifiable anyways). 
* Track test coverage with covr
* Added a `NEWS.md` file to track changes to the package.
