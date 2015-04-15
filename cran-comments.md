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
As requested, the packaged description has been updated.

> >>>>> brendan  writes:
> 
> > Thanks Kurt,
> > This submission is taking quite a few 'rounds' so I'd like to make sure the
> > next one is up to scratch! 
> 
> > Two questions:
> 
> > 1. Is the following description acceptable? The first line is new.
> 
> >       An R interface for the remote file hosting service 'Box' (https://www.box.com/).
> >       In addition to uploading and downloading files, this package
> >       includes functions which mirror base R operations for local files, (e.g. 
> >       box_load(), box_save(), box_read(), box_setwd(), etc.), as well as 'git' style 
> >       functions for entire directories (e.g. box_fetch(), box_push()).
> 
> Looks fine to me, thanks.
> 
> >    The HTTP status  of the URL is 200 on a headless Linux server, so hopefully
> >    it should be compliant with the CRAN URL policy.
> 
> >    I do not seem able to check for possibly misspelled words, but hopefully
> >    none have been introduced.
> 
> > 2. Aside from this change, is the package acceptable to CRAN, or are
> > there any other changes I should make before re-submission?
> 
> If the above is all I reported, then no further changes should be
> necessary (hopefully).
> 
> Best
> 
