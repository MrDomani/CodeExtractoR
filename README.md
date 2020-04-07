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
extract_code_from_pdf(my_url, 'output.R')
```

Boom!

## Important notes

 * Package uses API to [cloudconvert](https://cloudconvert.com/pdf-to-html). Obtaining your own API key may be convenient.
 * PDF file **must** be supplied as URL link, **not** path to file stored in your filesystem.


