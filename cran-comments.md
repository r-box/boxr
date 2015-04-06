## Test environments
* Ubuntu 14.04, R 3.1.3
* Ubuntu 12.04 (on travis-ci), R 3.1.2
* Windows 7, 3.1.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Brendan Rocks <rocks.brendan@gmail.com>'
New submission
Components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Brendan Rocks
```
Hopefully this is alright.

```
Namespace in Imports field not imported from: 'httpuv'
All declared Imports should be used.
```

`httpuv` is Suggested by `httr`, however, it is required to perform OAuth2.0 authentication, which is necessary to use the `boxr` package.

## Resubmission note
```
Found the following
  (possibly) invalid URLs:
  URL:
  https://app.box.com/developers/services
    From:
  man/box_auth.Rd
    Status: 403
    Message:
  Forbidden
```

In a browser, this takes you to a login screen.

Unfortunately, it is necessary to login via this URL to make any use of the package. 

As per the CRAN URL check policy (http://cran.r-project.org/web/packages/URL_checks.html), I have explained how this URL is problematic in the files where it appears, and in this note. Apologies!

```
The Title field should be in title case, current version then in title case:
‘R Interface for the box.com API’
‘R Interface for the Box.com API’

and in fact you missed in the manual:

The mandatory ‘Title’ field should give a short description of the package. ... Refer to other packages and external software in single quotes, and to book titles (and similar) in double quotes.

and 'R' here is redundant for an R package.
```

Fixed, I hope! 

The title of my package is now: Interface for the 'Box.com API'.

```
Quoting is also need in the Description field (see the manual).
```
The only violation I could spot was writing "'git style'" as opposed to "'git' style". Hopefully this is a fix! 


## Downstream dependencies
None.

## Tests
OAuth2.0 requires credentials which are excluded from the package. Tests have been run locally prior to submission, but have been excluded from the package build.
