#' Initialize an document fetch query
#'
#' @param con an [es_connect()] object
#' @param index the name of the index to query (if not specified, will fall back on 'primary_index' provided to [es_connect()])
#' @param path an optional directory to write docuents to as they are fetched
#' @param size the number of documents to fetch in each batch (max is 10000)
#' @param time_scroll specify how long a consistent view of the index should be maintained for scrolled search, e.g. "1m"
#' @param max maximum number of documents to fetch
#' @export
query_fetch <- function(
  con, index = NULL, path = NULL, size = 10000, time_scroll = "5m", max = 0
) {
  if (size > 10000)
    size <- 10000

  if (!is.null(path)) {
    if (!dir.exists(path))
      stop("Path '", path, "' must exist and be a directory",
        call. = FALSE)

    if (length(list.files(path)) > 0)
      message("Note: files already exist in directory '", path, "'")
  }

  structure(list(
    con = con,
    index = get_index(con, index),
    size = size,
    path = path,
    time_scroll = time_scroll,
    max = max,
    filters = list()
  ), class = c("es_query", "query_fetch"))
}
