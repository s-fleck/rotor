
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rotor

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/s-fleck/rotor.svg?branch=master)](https://travis-ci.org/s-fleck/rotor)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

rotor is aimed to provide a cross platform R reimagination of
[logrotate](https://linux.die.net/man/8/logrotate) as a companion
package to <https://github.com/s-fleck/lgr>. In addition to rotating log
files, it can also be used as a (very primtivie) backup tool.

`rotate()`, `rotate_date()`, and `rotate_time()` move a file and insert
a suffix (either an integer or a timestamp) into the filename. In
addition, they create an empty file in place of the original one. This
is useful for log rotation.

`backup()`, `backup_date()` and `backup_time()` do the same but keep the
original file.

See the [function
reference](https://s-fleck.github.io/rotor/reference/index.html) for
more
details

## Installation

<!-- You can install the released version of rotor from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("rotor") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/rotor")
```

## Example

The examples below all use `backup()` because it’s a bit more convenient
to write examples for than `rotate()`, but the later works mostly the
same.

``` r
library(rotor)
```

First we must create a temporary directory and an example log file.

``` r
# setup test file for examples
dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
tf <- file.path(td, "test.log")
saveRDS(cars, tf)
```

`backup()` makes a copy of a file and inserts an index between the
filename and the file extension. The file with the index `1` is always
the most recently made backup.

``` r
backup(tf)
backup(tf)
backup(tf, compression = TRUE)  # backup and rotate also support compression

list_backups(tf)  # returns all backups of a file
#> [1] "/tmp/RtmpHcL8u7/rotor/test.1.log.zip"
#> [2] "/tmp/RtmpHcL8u7/rotor/test.2.log"    
#> [3] "/tmp/RtmpHcL8u7/rotor/test.3.log"
```

You can also set a maximum number of backups to be kept

``` r
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)

list_backups(tf)
#> [1] "/tmp/RtmpHcL8u7/rotor/test.1.log"    
#> [2] "/tmp/RtmpHcL8u7/rotor/test.2.log"    
#> [3] "/tmp/RtmpHcL8u7/rotor/test.3.log"    
#> [4] "/tmp/RtmpHcL8u7/rotor/test.4.log.zip"
```

`prune_backups()` deletes all backups of a file except for
`max_backups`. If we set `max_backups` to `0`, we can clean up all
backups.

``` r
prune_backups(tf, max_backups = 0)
```

Instead of backup up with an index, you can also add a timestamp.

``` r
backup_date(tf)
backup_time(tf)
list_backups(tf)
#> [1] "/tmp/RtmpHcL8u7/rotor/test.2019-04-28--18-46-14.log"
#> [2] "/tmp/RtmpHcL8u7/rotor/test.2019-04-28.log"
```

``` r
prune_backups(tf, max_backups = 0)  # cleanup
list_backups(tf)
#> character(0)
```
