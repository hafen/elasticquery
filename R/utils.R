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

#' Get list of all queryable fields
#'
#' @param con an [es_connect()] object
#' @export
queryable_fields <- function(con) {
  tibble::as_tibble(con$types[con$types$type != "nested", ])
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
