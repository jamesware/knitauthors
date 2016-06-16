<!-- README.md is generated from README.Rmd. Please edit that file -->
**knitauthors**: an `R` package to support reproducible manuscripts
===================================================================

This repository is the home of **knitauthors**, an `R` package to support reproducible manuscripts.

In particular, `knitauthors` takes a table of authors & affiliations, and lists authors with numbered affiliations in the style typically expected for publication. The addition of a new middle author need not lead to extensive re-numbering by hand: knitauthors will take care of this for you.

The latest development version can be installed from GitHub using devtools:

``` r
## Check whether devtools is installed
"devtools" %in% installed.packages()

## install devtools if required
install.packages("devtools")

## install denovolyzeR
library(devtools)
install_github("jamesware/knitauthors")
```

[![Travis-CI Build Status](https://travis-ci.org/jamesware/knitauthors.png?branch=master)](https://travis-ci.org/jamesware/knitauthors)
