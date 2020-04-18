#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

check_class <- function(obj, class_name, fn_name) {
  if (length(class_name) == 1) {
    cls <- paste0("'", class_name, "'")
  } else {
    cls <- paste0("'", class_name, "'", collapse = " or ")
  }
  if (!inherits(obj, class_name))
    stop(fn_name, "() expects an object of class ", cls, call. = FALSE)
}

check_fields <- function(query, fields, func, nested_okay = FALSE) {
  types <- query$con$types
  if (!nested_okay)
    types <- types[types$type != "nested", ]

  idx <- which(!fields %in% types$field)
  if (length(idx) > 0) {
    flds <- paste0(fields[idx], collapse = "', '")
    plural <- ifelse(length(idx) > 1, "s", "")
    plural2 <- ifelse(length(idx) > 1, "are", "is")
    checkfn <- "queryable_fields()"
    if (nested_okay)
      checkfn <- "selectable_fields()"
    warning("Note: field", plural, " '", flds, "' specifed in ",
      func, "() ", plural2, " not valid. See ", checkfn, "...", call. = FALSE)
  }
}

#' Get list of all queryable fields
#'
#' @param con an [es_connect()] object
#' @export
queryable_fields <- function(con) {
  tibble::as_tibble(con$types[con$types$type != "nested", ])
}

#' Get list of all selectable fields
#'
#' @param con an [es_connect()] object
#' @export
selectable_fields <- function(con) {
  tibble::as_tibble(con$types)
}

get_index <- function(con, index) {
  if (is.null(index))
    index <- con$primary_index
  if (is.null(index))
    stop("Must provide an index.\n",
      "A primary index can be specified in es_connect()\n",
      "Or a per-query index can be specified in query_agg() or query_fetch()")
  index
}
