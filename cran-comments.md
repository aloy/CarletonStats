## Test environments

* local OS X install, R 4.3.0
* Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.2.3
* Fedora Linux (on R-hub) R-devel
* Windows (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Adam Loy <aloy@carleton.edu>’

New maintainer:
  Adam Loy <aloy@carleton.edu>
Old maintainer(s):
  Laura Chihara <lchihara@carleton.edu>

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
This is not a problem with the package, but on the R-hub end.