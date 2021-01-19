# boxr 0.3.6

## Test environments

* Local OS X install, R 4.0.3
* Fedora Linux, R-devel, clang, gfortran
* ubuntu 18.04 (on travis-ci), R (oldrel, release, and devel)
* win-builder (devel)

## R CMD check results

NOTE: change of email for maintainer

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ian Lyttle <ian.lyttle@se.com>'

New maintainer:
  Ian Lyttle <ian.lyttle@se.com>
Old maintainer(s):
  Ian Lyttle <ian.lyttle@schneider-electric.com>
```

## Current CRAN check results 

<https://cran.rstudio.org/web/checks/check_results_boxr.html> as of 2021-01-18

```
Version: 0.3.5
Check: Rd cross-references
Result: NOTE
    Undeclared packages ‘data.table’, ‘readxl’, ‘openxlsx’ in Rd xrefs
Flavor: r-devel-linux-x86_64-fedora-clang
```

These are references to dependencies of a suggested package (rio)