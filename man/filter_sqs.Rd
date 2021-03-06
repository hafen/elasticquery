% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_sqs}
\alias{filter_sqs}
\title{Specify a "simple query string" filter}
\usage{
filter_sqs(
  query,
  fields,
  string,
  bool = c("must", "filter", "must_not", "should")
)
}
\arguments{
\item{query}{a \code{\link[=query_agg]{query_agg()}} or \code{\link[=query_fetch]{query_fetch()}} object}

\item{fields}{field names (see \code{\link[=queryable_fields]{queryable_fields()}} for all possibilities)}

\item{string}{a string or vector of strings to exact match}

\item{bool}{one of "must", "filter", "must_not", "should" (see \href{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html}{here})}
}
\description{
Specify a "simple query string" filter
}
\details{
See \href{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html}{here} for more.
More of the options will be implemented in the future.
Adding more options and testing them could be an easy exercise for a
contributor's pull request.
}
