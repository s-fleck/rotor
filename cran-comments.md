## Test environments
* ubuntu 20.04, R 4.0.2
* ubuntu 18.04 (via RStudio Server), R 4.0.2
* ubuntu 16.04 (via travis), R 3.6.1
* win-builder, R devel
* macOS 10.13.6 High Sierra, R-release, brew (via Rhub) 
* rhub::check_for_cran()

## R CMD check results

0 errors | 0 warnings | 0 notes

Sorry for the many resubmissions. This package currently has an issue that
only to occur on macOS. It does not occur on the same macOS version available
on Rhub. 

My current theory is that the error is caused by the accuracy of the
'mtime' filestamp, which is linked to the file system the machine uses. 
I implemented a workaround under that assumption, but I cannot be 100% sure 
that this fixes that error. If the error still persists... could you please
inform me which file system you use on r-release-macos-x86_64?
