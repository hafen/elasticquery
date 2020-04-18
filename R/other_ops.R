#' Specify fields to return
#'
#' @param query a [query_fetch()] object
#' @param fields field name or vector of field names (see [selectable_fields()] for all possibilities)
#' @export
select_fields <- function(query, fields) {
  check_class(query, c("query_fetch"), "select_fields")
  check_fields(query, fields, "select_fields", nested_okay = TRUE)
  query$select <- fields
  query
}

#' Specify sort order of returned documents
#'
#' @param query a [query_fetch()] object
#' @param fields field name or vector of field names (see [queryable_fields()] for all possibilities)
#' @export
sort_docs <- function(query, fields) {
  check_class(query, c("query_fetch"), "sort_docs")
  check_fields(query, fields, "sort_docs")
  query$sort <- fields
  query
}
