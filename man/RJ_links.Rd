% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{RJ_links}
\alias{RJ_links}
\title{Articles from RJournal}
\format{
a \code{data.frame} with 433 rows and 6 following columns:
\describe{
\item{year}{character, in which year the issue with article was published}
\item{issue_nr}{integer, in which issue in specified year article was published}
\item{title}{character, title of the article}
\item{pdf_ref}{character, link to article in .pdf}
\item{site_href}{character, link to site on which the article was published}
\item{has_supp}{logical. Did the authors provide any supplementary material?}
}
}
\source{
\url{https://journal.r-project.org/archive/}
}
\usage{
RJ_links
}
\description{
A dataset containing link and basic data about articles from RJournal, till 2019.
}
\keyword{datasets}
