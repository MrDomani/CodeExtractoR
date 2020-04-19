<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/MrDomani/CodeExtractoR.svg?branch=master)](https://travis-ci.org/MrDomani/CodeExtractoR)
[![Codecov test coverage](https://codecov.io/gh/MrDomani/CodeExtractoR/branch/master/graph/badge.svg)](https://codecov.io/gh/MrDomani/CodeExtractoR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MrDomani/CodeExtractoR?branch=master&svg=true)](https://ci.appveyor.com/project/MrDomani/CodeExtractoR)
<!-- badges: end -->

# CodeExtractoR
A package to extract code snippets from PDF files.

Created with extracting code snippets from articles from [RJournal](https://journal.r-project.org/) and other similar sources.

Mostly for purposes related to Research Workshops subject on Warsaw Uniwersity of Technology.

## Install
Be aware, that it is still in development.
``` r
# install.packages("devtools")
devtools::install_github("MrDomani/CodeExtractoR")
```

## Usage
Ever wanted to extract code from PDF files (posted, for example, at [RJournal](https://journal.r-project.org/)), but Ctrl+C Ctrl+V is too exhausting / boring / humiliating? No more:

``` r
library(CodeExtractoR)
my_url <- "https://journal.r-project.org/archive/2014/RJ-2014-011/RJ-2014-011.pdf"
api_key <- 'My-API-key'
extract_code_from_pdf(my_url, 'output.R', api_key = api_key)
```

Boom!

## Important notes

 * Package uses API to [cloudconvert](https://cloudconvert.com/pdf-to-html). Obtaining your own API key is necessary.
 * Alternatively, you may convert the file(s) by hand at [cloudconvert](https://cloudconvert.com/pdf-to-html) and use `extract_code_from_html()` function.
 * PDF file **must** be supplied as URL link, **not** path to file stored in your filesystem.


