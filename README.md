
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rotor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/s-fleck/rotor.svg?branch=master)](https://travis-ci.org/s-fleck/rotor)
<!-- badges: end -->

rotor is aimed to provide a cross platform R reimagination of
[logrotate](https://linux.die.net/man/8/logrotate) as a companion
package to
<https://github.com/s-fleck/lgr>.

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

`rotate()` and `rotate_date()` rename a file and insert a suffix (either
an integer or a timestamp) into the filename. This is useful for log
rotation.

`backup()` and `backup_date()` do the same but keep the original file,
thus creting a backup. The examples below use `backup*()` because it’s a
bit more convenient to write examples for, but `rotate()` works mostly
the same.

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
file.create(tf)
```

`backup()` makes a copy of a file and inserts an index between the
filename and the file extension. The file with the index `1` is always
the most recently made backup.

``` r
backup(tf)
backup(tf)
backup(tf, compression = "zip")  # backup and rotate also support compression

find_backups(tf)  # returns all backups of a file
#> [1] "/tmp/RtmpbVXsKq/rotor/test.1.log.zip"
#> [2] "/tmp/RtmpbVXsKq/rotor/test.2.log"    
#> [3] "/tmp/RtmpbVXsKq/rotor/test.3.log"
```

You can also set a maximum number of backups to be kept

``` r
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)
backup(tf, max_backups = 4)

find_backups(tf)
#> [1] "/tmp/RtmpbVXsKq/rotor/test.1.log"    
#> [2] "/tmp/RtmpbVXsKq/rotor/test.2.log"    
#> [3] "/tmp/RtmpbVXsKq/rotor/test.3.log"    
#> [4] "/tmp/RtmpbVXsKq/rotor/test.4.log.zip"
```

``` r
invisible(file.remove(find_backups(tf)))  # cleanup
```

Instead of adding an index, you can also add a timestamp.

``` r
backup_date(tf)
find_backups(tf)
#> [1] "/tmp/RtmpbVXsKq/rotor/test.2019-03-31.log"
```

``` r
invisible(file.remove(find_backups(tf)))  # cleanup
```
