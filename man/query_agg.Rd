% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_agg.R
\name{query_agg}
\alias{query_agg}
\title{Initialize an aggregation query}
\usage{
query_agg(con, index = NULL, size = 1000, check_fields = TRUE)
}
\arguments{
\item{con}{an \code{\link[=es_connect]{es_connect()}} object}

\item{index}{the name of the index to query (if not specified, will fall back on 'primary_index' provided to \code{\link[=es_connect]{es_connect()}})}

\item{size}{(TODO)}

\item{check_fields}{should field names be checked when the query is
augmented?}
}
\description{
Initialize an aggregation query
}
