#' Initialize a query with a specified query string
#'
#' @param con an [es_connect()] object
#' @param index the name of the index to query (if not specified, will
#'   fall back on 'primary_index' provided to [es_connect()])
#' @param str query string
#' @param path an optional directory to write documents to as they are fetched
#' @param time_scroll specify how long a consistent view of the index should
#'   be maintained for scrolled search, e.g. "1m" (used when type=="fetch"
#' @param max maximum number of documents to fetch (used when type=="fetch")
#' @param type the type of query defined in the string - one of "unknown",
#'   "fetch", or "agg"
#' @param format format of returned output - either "list" or "json"
#' @export
query_str <- function(
  con, index = NULL, str, path = NULL, time_scroll = "5m", max = 0,
  type = c("unknown", "fetch", "agg"), format = c("list", "json")
) {
  format <- match.arg(format)
  con$raw <- format == "json"

  type <- match.arg(type)

  if (is.null(path) && format == "file")
    stop("Must specify a path if format='file'", call. = FALSE)

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
    str = str,
    path = path,
    time_scroll = time_scroll,
    max = max,
    format = format,
    type = type
  ), class = c("es_query", "query_str"))
}
