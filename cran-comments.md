## Test environments
* ubuntu 20.04, R 4.0.2
* ubuntu 18.04 (via RStudio Server), R 4.0.2
* ubuntu 16.04 (via travis), R 3.6.1
* win-builder, R devel
* macOS 10.13.6 High Sierra, R-release, brew (via Rhub) 
* rhub::check_for_cran()

## R CMD check results

0 errors | 0 warnings | 0 notes

* fixes time zone related issue in `Cache$prune()` that causes macOS tests to fail
* more robust cleanup of temporary files between some tests
* resubmission that improves one test where 
  `https://cran.r-project.org/web/checks/check_results_rotor.html` shows an
  error that I could not reproduce on any machine available on rhub. 
  Hopefully the test succeedes now, but if not it will give me more useful 
  debug information for another resubmission.


Notes:

There was a timezone related issue that only occured on macos. I fixed it and
was able to test it on macOS 10.13.6 High Sierra with R-release (but not devel)
on Rhub.

I could not reproduce the other errors; however, it looks as if they were caused
by improperly cleaned-up temporary files between failing tests. I made the
cleanup code a bit more robust and hope the problem is fixed now.
