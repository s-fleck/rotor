## Test environments
* ubuntu 20.04, R 4.0.2
* ubuntu 18.04 (via RStudio Server), R 4.0.2
* ubuntu 16.04 (via travis), R 3.6.1
* win-builder, R devel
* macOS 10.13.6 High Sierra, R-release, brew (via Rhub) 
* rhub::check_for_cran()

## R CMD check results

0 errors | 0 warnings | 0 notes

Another - hopefully final attempt - to fix a hard to debug issue where some
tests can fail on systems with low-precission filesystem-timestamps 
(such as ext3 and old Windows filesystems).

Added a test that mocks low-precision filesystem-timestamps to be on the safe
side.
