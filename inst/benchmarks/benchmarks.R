
library(rotor)
library(bench)

tf <- tempfile()
file.create(tf)

backup_date(tf, now = "2019-01-01")
backup_date(tf, now = "2019-01-02")
backup_date(tf, now = "2019-01-03")
backup_date(tf, now = "2019-01-04")
backup_date(tf, now = "2019-01-05")


bq_cached <- BackupQueueDate$new(tf, cache_backups = TRUE)
bq <- BackupQueueDate$new(tf, cache_backups = FALSE)

res <- bench::mark(
  bq$n,
  bq$files,
  bq$should_rotate(age = "1 year", size = -1),
  bq_cached$n,
  bq_cached$files,
  bq_cached$should_rotate(age = "1 year", size = -1),
  check = FALSE
)

plot(res)
