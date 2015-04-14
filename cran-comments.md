## Test environments
* Ubuntu 12.04 (on travis-ci), R 3.1.2
* Windows 7, 3.1.3

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Brendan Rocks <rocks.brendan@gmail.com>'
New submission
Components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Brendan Rocks

# Resubmission Note
> I get
>
> Possibly mis-spelled words in DESCRIPTION:
>   boxr (10:62)
>   setwd (12:27)
>
> For the first, pls simply drop the package name from the Description.
> For the second, pls write box_setwd() etc.
>
> Best

Apologies - fixed!