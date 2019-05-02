
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

**rotor** relies solely on information encoded in the filename suffix of
the backups (i.e. the timestamp or index). It therefore also works with
backups created by other tools, as long as the filename has a format
that rotor can deal with.

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

# backup and rotate also support compression
backup(tf, compression = TRUE) 

# returns all backups of a file
list_backups(tf)  
#> [1] "/tmp/RtmpRrR1ZJ/rotor/test.1.log.zip"
#> [2] "/tmp/RtmpRrR1ZJ/rotor/test.2.log"    
#> [3] "/tmp/RtmpRrR1ZJ/rotor/test.3.log"
```

You can also set a maximum number of backups to be kept. Notice how the
zipped backup we created above moves to index 4 as we create three new
backups.

``` r
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)

list_backups(tf)
#> [1] "/tmp/RtmpRrR1ZJ/rotor/test.1.log"    
#> [2] "/tmp/RtmpRrR1ZJ/rotor/test.2.log"    
#> [3] "/tmp/RtmpRrR1ZJ/rotor/test.3.log"    
#> [4] "/tmp/RtmpRrR1ZJ/rotor/test.4.log.zip"
```

We can also use `prune_backups()` to delete old backups. Other than
ensuring that no new backups is created, it works equivalent to using
`backup()` with the `max_backups` paramter. If we set `max_backups` to
`0`, we can clean up all backups.

``` r
prune_backups(tf, max_backups = 0)
```

Besides creating backup up with an index, **rotor** can also create
timestamped backups.

``` r
backup_date(tf)
backup_time(tf)
list_backups(tf)
#> [1] "/tmp/RtmpRrR1ZJ/rotor/test.2019-05-02--11-17-22.log"
#> [2] "/tmp/RtmpRrR1ZJ/rotor/test.2019-05-02.log"
```

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
  
# keep all backups after April 4th, 2018
prune_backups(tf, "2018-04-01")  
```
