
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rotor

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/s-fleck/rotor.svg?branch=master)](https://travis-ci.org/s-fleck/rotor)
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

rotor also includes a few utility functions for examining backups of a
file: `list_backups()`, `backup_info()`, `n_backups`, `newest_backup()`,
`oldest_backup()`. See the [function
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
#> [1] "/tmp/RtmppxEUB8/rotor/mylogfile.1.log.zip"
#> [2] "/tmp/RtmppxEUB8/rotor/mylogfile.2.log"
```

`rotate()` also backs up a file, but replaces the original file with an
empty one.

``` r
rotate(tf)
list_backups(tf)
#> [1] "/tmp/RtmppxEUB8/rotor/mylogfile.1.log"    
#> [2] "/tmp/RtmppxEUB8/rotor/mylogfile.2.log.zip"
#> [3] "/tmp/RtmppxEUB8/rotor/mylogfile.3.log"

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
#> [1] "/tmp/RtmppxEUB8/rotor/mylogfile.1.log"    
#> [2] "/tmp/RtmppxEUB8/rotor/mylogfile.2.log"    
#> [3] "/tmp/RtmppxEUB8/rotor/mylogfile.3.log"    
#> [4] "/tmp/RtmppxEUB8/rotor/mylogfile.4.log.zip"
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
those created with
`backup_index()`).

``` r
# be default backup_date() only makes a backup if the last backups is younger
# than 1 day, so we set `age` to -1 for this example
backup_date(tf, age = -1)  
backup_date(tf, format = "%Y-%m", age = -1)
backup_time(tf)
backup_time(tf, format = "%Y-%m-%d_%H-%M-%S")  # Python logging
backup_time(tf, format = "%Y%m%dT%H%M%S")  # ISO 8601 compatible

backup_info(tf)
#>                                                       path
#> 1  /tmp/RtmppxEUB8/rotor/mylogfile.2019-05-30_23-15-33.log
#> 2 /tmp/RtmppxEUB8/rotor/mylogfile.2019-05-30--23-15-33.log
#> 5      /tmp/RtmppxEUB8/rotor/mylogfile.20190530T231533.log
#> 3           /tmp/RtmppxEUB8/rotor/mylogfile.2019-05-30.log
#> 4              /tmp/RtmppxEUB8/rotor/mylogfile.2019-05.log
#>                     dir      name                  sfx ext size isdir mode
#> 1 /tmp/RtmppxEUB8/rotor mylogfile  2019-05-30_23-15-33 log   26 FALSE  664
#> 2 /tmp/RtmppxEUB8/rotor mylogfile 2019-05-30--23-15-33 log   26 FALSE  664
#> 5 /tmp/RtmppxEUB8/rotor mylogfile      20190530T231533 log   26 FALSE  664
#> 3 /tmp/RtmppxEUB8/rotor mylogfile           2019-05-30 log   26 FALSE  664
#> 4 /tmp/RtmppxEUB8/rotor mylogfile              2019-05 log   26 FALSE  664
#>                 mtime               ctime               atime  uid  gid
#> 1 2019-05-30 23:15:33 2019-05-30 23:15:33 2019-05-30 23:15:33 1000 1000
#> 2 2019-05-30 23:15:33 2019-05-30 23:15:33 2019-05-30 23:15:33 1000 1000
#> 5 2019-05-30 23:15:33 2019-05-30 23:15:33 2019-05-30 23:15:33 1000 1000
#> 3 2019-05-30 23:15:33 2019-05-30 23:15:33 2019-05-30 23:15:33 1000 1000
#> 4 2019-05-30 23:15:33 2019-05-30 23:15:33 2019-05-30 23:15:33 1000 1000
#>   uname grname           timestamp
#> 1 hoelk  hoelk 2019-05-30 23:15:33
#> 2 hoelk  hoelk 2019-05-30 23:15:33
#> 5 hoelk  hoelk 2019-05-30 23:15:33
#> 3 hoelk  hoelk 2019-05-30 00:00:00
#> 4 hoelk  hoelk 2019-05-01 00:00:00
```

If we examine the “timestamp” column in the example above, we see that
missing date information is always interpreted as the start of the
period; i.e. so `"2019-01"` is equivalent to `"2019-01-01--00--00--00"`
for all intentds and purposes.

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

# Dependencies

**rotor**’s dependencies are intentionally kept slim. It only comes with
two non-base dependencies:

  - [R6](https://github.com/r-lib/R6): A light weight system for
    encapsulated object-oriented programming.
  - [dint](https://github.com/s-fleck/dint): A toolkit for working
    year-quarter and year-month dates that I am also the author of. It
    is used by `rotate_date()` and `rotate_time()` to deal with calendar
    periods.

Both packages have no transitive dependencies (i.e they do not depend on
anything outside of base R)
