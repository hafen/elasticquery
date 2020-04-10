# filters can be applied to both fetch and aggregate queries

# https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-range-query.html

#' Specify a range filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param from the lower value of the range
#' @param to the upper value of the range
#' @export
filter_range <- function(
  query, field, from = NULL, to = NULL
) {
  check_class(query, c("query_agg", "query_fetch"), "filter_range")
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

  query$filters <- c(query$filters, list(res))
  query
}
# { "range": { "processedOnDate": { "gte": "2020-02-01" }}}

#' Specify a match filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param match a string to match
#' @export
filter_match <- function(query, field, match) {
  check_class(query, c("query_agg", "query_fetch"), "filter_match")
  res <- list(
    match = list()
  )
  res$match[[field]] <- match

  query$filters <- c(query$filters, list(res))
  query
}
# { "match": { "fullText": "and" }},

#' Specify a terms filter
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param terms a string or vector of strings to exact match
#' @importFrom jsonlite toJSON
filter_terms <- function(query, field, terms) {
  check_class(query, c("query_agg", "query_fetch"), "filter_terms")

  tm <- ifelse(length(terms) == 1, "term", "terms")
  res <- list()
  res[[tm]] <- list()
  res[[tm]][[field]] <- terms

  query$filters <- c(query$filters, list(res))
  query
}
# { "term":  { "languageCode": "en" }},
# { "terms":  { "languageCode": ["en", "au"] }},

# logic = c("and", "or", "not")
