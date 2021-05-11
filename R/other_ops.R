#' Specify fields to return
#'
#' @param query a [query_fetch()] object
#' @param fields field name or vector of field names (see [selectable_fields()] for all possibilities)
#' @export
select_fields <- function(query, fields) {
  check_class(query, c("query_fetch"), "select_fields")
  if (query$check_fields)
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
  if (query$check_fields)
    check_fields(query, fields, "sort_docs")
  fields <- lapply(fields, function(x) {
    res <- list()
    if (!is.null(attr(x, "direction")) && attr(x, "direction") == "desc") {
      res[[1]] <- list(order = "desc")
    } else {
      res[[1]] <- list(order = "asc")
    }
    names(res) <- x
    res
  })
  query$sort <- fields
  query
}

#' Specify ascending sort order
#' @param x field name to sort by
#' @export
asc <- function(x) {
  x <- list(x)
  attr(x, "direction") <- "asc"
  x
}

#' Specify descending sort order
#' @param x field name to sort by
#' @export
desc <- function(x) {
  x <- list(x)
  attr(x[[1]], "direction") <- "desc"
  x
}
