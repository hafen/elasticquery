#' Set connection details to Elasticsearch engine
#'
#' @inheritParams elastic::connect
#' @param primary_index the name of the primary index to use for querying (will be used as a fallback if index is not explicitly specified in the query)
#' @details See [elastic::connect()] for more details.
#' @importFrom elastic connect index_get
#' @examples
#' \dontrun{
#' con <- es_connect(host = "localhost", port = 9200)
#' }
#' @export
es_connect <- function(
  host = "127.0.0.1", port = 9200, primary_index = NULL,
  path = NULL, transport_schema = "http", user = NULL, pwd = NULL,
  headers = NULL, cainfo = NULL, force = FALSE, errors = "simple", warn = TRUE,
  ...
) {
  pars <- c(
    list(
      host = host, port = port, path = path,
      transport_schema = transport_schema, user = user, pwd = pwd,
      headers = headers, cainfo = cainfo, force = force,
      errors = errors, warn = warn),
    list(...)
  )

  con <- do.call(elastic::connect, pars)
  con$ping()
  index <- elastic::index_get(con, primary_index, raw = TRUE)[[1]]

  # easily queryable
  get_types <- function(x, prefix = "") {
    types <- lapply(x, function(a) a$type)
    tmp <- unlist(types)
    nms <- names(tmp)
    if (prefix != "")
      nms <- paste0(prefix, ".", nms)
    data.frame(field = nms, type = unname(tmp), stringsAsFactors = FALSE)
  }
  get_null_types <- function(x)
    names(which(unlist(lapply(x, function(a) is.null(a$type)))))

  types <- list(get_types(index$mappings$properties))
  for (fld in get_null_types(index$mappings$properties)) {
    types <- c(types, list(
      get_types(index$mappings$properties[[fld]]$properties, prefix = fld)
    ))
  }
  types <- do.call(rbind, types)
  # idx <- which(sapply(types, is.null))

  structure(list(
    con = con,
    primary_index = primary_index,
    types = types,
    index = index
  ), class = "es_connection")
}

#' @export
print.es_connection <- function(x, ...) {
  # str(x[-1], 1)
  "es_connection object"
}
