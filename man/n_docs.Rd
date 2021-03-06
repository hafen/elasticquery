% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{n_docs}
\alias{n_docs}
\title{Get the total number of documents that match query parameters}
\usage{
n_docs(query)
}
\arguments{
\item{query}{a \code{\link[=query_agg]{query_agg()}}, \code{\link[=query_fetch]{query_fetch()}}, or \code{\link[=query_str]{query_str()}} object}
}
\description{
Get the total number of documents that match query parameters
}
\details{
Note: the number returned is not necessarily the same number of
documents that a fetch query will return. That depends on the value set for
max.
}
