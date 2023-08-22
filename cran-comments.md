## Test environments

* local OS X install, R 4.3.1
* Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.2.3
* Fedora Linux (on R-hub) R-devel
* Windows (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Adam Loy <aloy@carleton.edu>’

New maintainer:
  Adam Loy <aloy@carleton.edu>
Old maintainer(s):
  Laura Chihara <lchihara@carleton.edu>

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

#> Skipping checking math rendering: package 'V8' unavailable
621#> * checking for non-standard things in the check directory ... NOTE
622#> Found the following files/directories:
623#> ''NULL''
624#> * checking for detritus in the temp directory ... NOTE
625#> Found the following files/directories:
626#> 'lastMiKTeXException'
627#> * DONE
  
These are not problems with the package, but on the R-hub end.