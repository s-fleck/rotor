
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rotor

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/s-fleck/rotor.svg?branch=master)](https://travis-ci.com/s-fleck/rotor)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/s-fleck/rotor/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/rotor?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/rotor)](https://cran.r-project.org/package=rotor)
<!-- badges: end -->

**rotor** provides a cross platform R reimagination of
[logrotate](https://linux.die.net/man/8/logrotate). It is a companion
package to the logging package [lgr](https://github.com/s-fleck/lgr). In
contrast to logrotate, rotor relies solely on information encoded in a
suffixes of file names for conditionally creating backups (i.e. a
timestamp or index). It therefore also works with backups created by
other tools, as long as the filename has a format that rotor can
understand.

`rotate()`, `rotate_date()`, and `rotate_time()` move a file and insert
a suffix (either an integer or a timestamp) into the filename. In
addition, they create an empty file in place of the original one. This
is useful for log rotation. `backup()`, `backup_date()` and
`backup_time()` do the same but keep the original file.

rotor also includes utility functions for finding and examining the
backups of a file: `list_backups()`, `backup_info()`, `n_backups`,
`newest_backup()`, `oldest_backup()`. See the [function
reference](https://s-fleck.github.io/rotor/reference/index.html) for
details.

## Installation

You can install the released version of rotor from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rotor")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("s-fleck/rotor")
```

## Example

First we create a temporary directory for the files created by the code
examples

``` r
library(rotor)

# create a directory
td <- file.path(tempdir(), "rotor")
dir.create(td, recursive = TRUE)

# create an example logfile
tf <- file.path(td, "mylogfile.log")
writeLines("An important message", tf)
```

### Indexed backups

`backup()` makes a copy of a file and inserts an index between the
filename and the file extension. The file with the index `1` is always
the most recently made backup.

``` r
backup(tf)

# backup and rotate also support compression
backup(tf, compression = TRUE) 

# display backups of a file
list_backups(tf)  
#> [1] "/tmp/Rtmpw73XUg/rotor/mylogfile.1.log.zip"
#> [2] "/tmp/Rtmpw73XUg/rotor/mylogfile.2.log"
```

`rotate()` also backs up a file, but replaces the original file with an
empty one.

``` r
rotate(tf)
list_backups(tf)
#> [1] "/tmp/Rtmpw73XUg/rotor/mylogfile.1.log"    
#> [2] "/tmp/Rtmpw73XUg/rotor/mylogfile.2.log.zip"
#> [3] "/tmp/Rtmpw73XUg/rotor/mylogfile.3.log"

# the original file is now empty
readLines(tf)
#> character(0)

# its content was moved to the first backup
readLines(list_backups(tf)[[1]])
#> [1] "An important message"

# we can now safely write to the original file
writeLines("another important message", tf)
```

The `max_backups` parameter limits the maximum number of backups rotor
will keep of a file. Notice how the zipped backup we created above moves
to index 4 as we create two new backups.

``` r
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)

list_backups(tf)
#> [1] "/tmp/Rtmpw73XUg/rotor/mylogfile.1.log"    
#> [2] "/tmp/Rtmpw73XUg/rotor/mylogfile.2.log"    
#> [3] "/tmp/Rtmpw73XUg/rotor/mylogfile.3.log"    
#> [4] "/tmp/Rtmpw73XUg/rotor/mylogfile.4.log.zip"
```

We can also use `prune_backups()` to delete old backups. Other than
ensuring that no new backups is created, it works identically to using
`backup()` with the `max_backups` parameter. By setting it to `0`, we
delete all backups.

``` r
prune_backups(tf, max_backups = 0)
```

## Timestamped backups

**rotor** can also create timestamped backups. `backup_date()` creates
uses a Date (`yyyy-mm-dd`) timestamp, `backup_time()` uses a full
datetime-stamp by default (`yyyy-mm-dd--hh-mm-ss`). The format of the
timestamp can be modified with a subset of the formatting tokens
understood by `strftime()` (within certain restrictions). Backups
created with both functions are compatible with each other (but not with
those created with `backup_index()`).

``` r
# be default backup_date() only makes a backup if the last backups is younger
# than 1 day, so we set `age` to -1 for this example
backup_date(tf, age = -1)  
backup_date(tf, format = "%Y-%m", age = -1)
backup_time(tf)
backup_time(tf, format = "%Y-%m-%d_%H-%M-%S")  # Python logging
backup_time(tf, format = "%Y%m%dT%H%M%S")  # ISO 8601 compatible

backup_info(tf)
#>                                                       path      name
#> 1  /tmp/Rtmpw73XUg/rotor/mylogfile.2020-07-24_10-54-30.log mylogfile
#> 2 /tmp/Rtmpw73XUg/rotor/mylogfile.2020-07-24--10-54-30.log mylogfile
#> 5      /tmp/Rtmpw73XUg/rotor/mylogfile.20200724T105430.log mylogfile
#> 3           /tmp/Rtmpw73XUg/rotor/mylogfile.2020-07-24.log mylogfile
#> 4              /tmp/Rtmpw73XUg/rotor/mylogfile.2020-07.log mylogfile
#>                    sfx ext size isdir mode               mtime
#> 1  2020-07-24_10-54-30 log   26 FALSE  664 2020-07-24 10:54:30
#> 2 2020-07-24--10-54-30 log   26 FALSE  664 2020-07-24 10:54:30
#> 5      20200724T105430 log   26 FALSE  664 2020-07-24 10:54:30
#> 3           2020-07-24 log   26 FALSE  664 2020-07-24 10:54:30
#> 4              2020-07 log   26 FALSE  664 2020-07-24 10:54:30
#>                 ctime               atime   uid   gid uname grname
#> 1 2020-07-24 10:54:30 2020-07-24 10:54:30 11861 11861 fleck  fleck
#> 2 2020-07-24 10:54:30 2020-07-24 10:54:30 11861 11861 fleck  fleck
#> 5 2020-07-24 10:54:30 2020-07-24 10:54:30 11861 11861 fleck  fleck
#> 3 2020-07-24 10:54:30 2020-07-24 10:54:30 11861 11861 fleck  fleck
#> 4 2020-07-24 10:54:30 2020-07-24 10:54:30 11861 11861 fleck  fleck
#>             timestamp
#> 1 2020-07-24 10:54:30
#> 2 2020-07-24 10:54:30
#> 5 2020-07-24 10:54:30
#> 3 2020-07-24 00:00:00
#> 4 2020-07-01 00:00:00
```

If we examine the “timestamp” column in the example above, we see that
missing date information is always interpreted as the start of the
period; i.e. so `"2019-01"` is equivalent to `"2019-01-01--00--00--00"`
for all intents and purposes.

``` r
prune_backups(tf, max_backups = 0)  # cleanup
list_backups(tf)
#> character(0)
```

Besides passing a total number of backups to keep, `max_backups` can
also be a period or a date / datetime for timestamped backups.

``` r
# keep all backups younger than one year
prune_backups(tf, "1 year") 
  
# keep all backups from April 4th, 2018 and onwards
prune_backups(tf, "2018-04-01")  
```

## Cache

rotor also provides a simple on-disk key-value store that can be used as
a persistent cache.

``` r
cache <- Cache$new(file.path(tempdir(), "cache-test"), hashfun = digest::digest)
#> creating directory '/tmp/Rtmpw73XUg/cache-test'
key1 <- cache$push(iris)
key2 <- cache$push(cars)
key3 <- cache$push(mtcars)

cache$files$path
#> [1] "/tmp/Rtmpw73XUg/cache-test/d3c5d071001b61a9f6131d3004fd0988"
#> [2] "/tmp/Rtmpw73XUg/cache-test/f98a59010652c8e1ee062ed4c43f648e"
#> [3] "/tmp/Rtmpw73XUg/cache-test/a63c70e73b58d0823ab3bcbd3b543d6f"

head(cache$read(key1))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

cache$prune(max_files = 1)
cache$files$path
#> [1] "/tmp/Rtmpw73XUg/cache-test/a63c70e73b58d0823ab3bcbd3b543d6f"
cache$purge()  # deletes all cached files
cache$destroy()  # deletes the cache directory
```

# Dependencies

**rotor**’s dependencies are intentionally kept slim. It only comes with
two non-base dependencies:

  - [R6](https://github.com/r-lib/R6): A light weight system for
    encapsulated object-oriented programming.
  - [dint](https://github.com/s-fleck/dint): A toolkit for working
    year-quarter and year-month dates that I am also the author of. It
    is used by `rotate_date()` and `rotate_time()` to deal with calendar
    periods (such as weeks or months).

Both packages have no transitive dependencies (i.e they do not depend on
anything outside of base R)

Optional dependencies:

  - [digest](https://github.com/eddelbuettel/digest) or
    [uuid](https://CRAN.R-project.org/package=uuid) for generating
    hashes or UIDs when using Cache. Storage keys for cache files can
    also be set manually, in which case no external dependencies are
    required.
  - [zip](https://CRAN.R-project.org/package=zip) is supported as an
    alternative to the integrated zip function in R. Might work better
    on some systems and worse on others.
