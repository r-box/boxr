## Test environments
* Ubuntu 14.04, R 3.1.3
* Ubuntu 12.04 (on travis-ci), R 3.1.2
* Windows 7, 3.1.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* `Namespace in Imports field not imported from: 'httpuv'`
  `All declared Imports should be used.`

  `httpuv` is Suggested by `httr`, however, it is required to perform OAuth2.0 authentication, which is necessary to use the `boxr` package.

## Downstream dependencies
None.

## Tests
OAuth2.0 requires credentials which are excluded from the package. Tests have been run locally prior to submission, but should not run on CRAN.
