## Test environments
* Ubuntu 15.10 R 3.2.3
* Ubuntu 12.04 (on travis-ci), R 3.2.3
* Windows Server 2012 R2 (x64) (on appveyor)
* CRAN winbuilder

## R CMD check results
There were no ERRORs, WARNINGs. There was one NOTE:

```
* checking dependencies in R code ... NOTE
Unexported object imported by a ':::' call: ‘httr:::request’
  See the note in ?`:::` about the use of this operator.
```

This is a work around for a bug in the current CRAN version of the package `httr`. Without it users are 'spammed' with a great many supurious error messages, when nothing is actually wrong (see https://github.com/hadley/httr/pull/252 and https://github.com/jeroenooms/curl/issues/30).

I am very concious that this is bad practice, and will be able to remove the offending piece of code shortly after `httr` is updated on CRAN: This is not long-term functionality built on top of another package's internals.
