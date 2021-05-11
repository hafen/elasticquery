# filters can be applied to both fetch and aggregate queries

# https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-range-query.html

#' Specify a range filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param from the lower value of the range
#' @param to the upper value of the range
#' @param bool one of "must", "filter", "must_not", "should" (see [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html))
#' @export
filter_range <- function(
  query, field, from = NULL, to = NULL,
    bool = c("must", "filter", "must_not", "should")
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_range")
  if (query$check_fields)
    check_fields(query, field, "filter_range")
  bool <- match.arg(bool)
  # TODO: make sure variable is numeric or date
  # long, integer, short, byte, double, float, half_float, scaled_float

  res <- list(
    range = list()
  )
  res$range[[field]] <- list()
  if (!is.null(from))
    res$range[[field]]$gte <- from
  if (!is.null(to))
    res$range[[field]]$lte <- to

  query$filters[[bool]] <- c(query$filters[[bool]], list(res))
  query
}
# { "range": { "processedOnDate": { "gte": "2020-02-01" }}}

#' Specify a match filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param match a string to match
#' @param bool one of "must", "filter", "must_not", "should" (see [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html))
#' @export
filter_match <- function(
  query, field, match,
  bool = c("must", "filter", "must_not", "should")
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_match")
  if (query$check_fields)
    check_fields(query, field, "filter_match")
  bool <- match.arg(bool)
  res <- list(
    match = list()
  )
  res$match[[field]] <- match

  query$filters[[bool]] <- c(query$filters[[bool]], list(res))
  query
}
# { "match": { "fullText": "and" }},

#' Specify a regexp filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param regexp a regular expression string to match
#' @param bool one of "must", "filter", "must_not", "should" (see [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html))
#' @export
filter_regexp <- function(
  query, field, regexp,
  bool = c("must", "filter", "must_not", "should")
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_match")
  if (query$check_fields)
    check_fields(query, field, "filter_regexp")
  bool <- match.arg(bool)

  res <- list(
    regexp = list()
  )
  res$regexp[[field]] <- regexp

  query$filters[[bool]] <- c(query$filters[[bool]], list(res))
  query
}

#' Specify a terms filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param terms a string or vector of strings to exact match
#' @importFrom jsonlite toJSON
#' @param bool one of "must", "filter", "must_not", "should" (see [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html))
#' @export
filter_terms <- function(
  query, field, terms,
  bool = c("must", "filter", "must_not", "should")
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_terms")
  if (query$check_fields)
    check_fields(query, field, "filter_terms")
  bool <- match.arg(bool)

  tm <- ifelse(length(terms) == 1, "term", "terms")
  res <- list()
  res[[tm]] <- list()
  res[[tm]][[field]] <- terms

  query$filters[[bool]] <- c(query$filters[[bool]], list(res))
  query
}


#' Specify a "simple query string" filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param fields field names (see [queryable_fields()] for all possibilities)
#' @param string a string or vector of strings to exact match
#' @param bool one of "must", "filter", "must_not", "should" (see [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html))
#' @details See [here](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html) for more.
#' More of the options will be implemented in the future.
#' Adding more options and testing them could be an easy exercise for a
#' contributor's pull request.
#' @export
filter_sqs <- function(
  query, fields, string,
  bool = c("must", "filter", "must_not", "should")
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_sqs")
  if (query$check_fields)
    check_fields(query, fields, "filter_sqs")
  bool <- match.arg(bool)

  # if (!is.null(query$sqs))
  #   message("Simple query string has already been specified. Overwriting...")

  res <- list(simple_query_string = list(
    fields = as.list(fields),
    query = string
  ))

  query$filters[[bool]] <- c(query$filters[[bool]], res)

  query
}
