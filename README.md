
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rotor

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/s-fleck/rotor.svg?branch=master)](https://travis-ci.org/s-fleck/rotor)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/s-fleck/rotor/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/rotor?branch=master)
<!-- badges: end -->

**rotor** provides a cross platform R reimagination of
[logrotate](https://linux.die.net/man/8/logrotate) as a companion
package to the logging package [lgr](https://github.com/s-fleck/lgr). In
addition to rotating log files, it can also be used as a (primitive)
backup tool. For conditionally creating and deleting backups, rotor
relies solely on information encoded in a suffix of the backup file
names (i.e. a timestamp or index). It therefore also works with backups
created by other tools, as long as the filename has a format that rotor
can deal with.

`rotate()`, `rotate_date()`, and `rotate_time()` move a file and insert
a suffix (either an integer or a timestamp) into the filename. In
addition, they create an empty file in place of the original one. This
is useful for log rotation. `backup()`, `backup_date()` and
`backup_time()` do the same but keep the original file.

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

Before we run the examples for rotor, we have to setup a temporary
directory and an example log file.

``` r
library(rotor)

# setup test file for examples
dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
tf <- file.path(td, "mylogfile.log")
writeLines("An important message", tf)
```

`backup()` makes a copy of a file and inserts an index between the
filename and the file extension. The file with the index `1` is always
the most recently made backup.

``` r
backup(tf)

# backup and rotate also support compression
backup(tf, compression = TRUE) 

# display backups of a file
list_backups(tf)  
#> [1] "/tmp/RtmpZygTcn/rotor/mylogfile.1.log.zip"
#> [2] "/tmp/RtmpZygTcn/rotor/mylogfile.2.log"
```

`rotate()` also makes a copy, but replaces the original file with an
empty one.

``` r
rotate(tf)
list_backups(tf)
#> [1] "/tmp/RtmpZygTcn/rotor/mylogfile.1.log"    
#> [2] "/tmp/RtmpZygTcn/rotor/mylogfile.2.log.zip"
#> [3] "/tmp/RtmpZygTcn/rotor/mylogfile.3.log"

# the original file is now empty
readLines(tf)
#> character(0)

# its content was moved to the first backup
readLines(list_backups(tf)[[1]])
#> [1] "An important message"

writeLines("another important message", tf)
```

The `max_backups` parameter limits the maximum number of backups rotor
will keep of a file. Notice how the zipped backup we created above moves
to index 4 as we create two new backups.

``` r
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)

list_backups(tf)
#> [1] "/tmp/RtmpZygTcn/rotor/mylogfile.1.log"    
#> [2] "/tmp/RtmpZygTcn/rotor/mylogfile.2.log"    
#> [3] "/tmp/RtmpZygTcn/rotor/mylogfile.3.log"    
#> [4] "/tmp/RtmpZygTcn/rotor/mylogfile.4.log.zip"
```

We can also use `prune_backups()` to delete old backups. Other than
ensuring that no new backups is created, it works equivalent to using
`backup()` with the `max_backups` parameter. By setting it to `0`, we
delete all backups.

``` r
prune_backups(tf, max_backups = 0)
```

Besides creating backup up with an index, **rotor** can also create
timestamped backups.

``` r
backup_date(tf)
rotate_time(tf)
list_backups(tf)
#> [1] "/tmp/RtmpZygTcn/rotor/mylogfile.2019-05-15--14-01-51.log"
#> [2] "/tmp/RtmpZygTcn/rotor/mylogfile.2019-05-15.log"
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
