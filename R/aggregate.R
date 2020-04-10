#' Initialize an aggregation query
#'
#' @param con an [es_connect()] object
#' @param index the name of the index to query (if not specified, will fall back on 'primary_index' provided to [es_connect()])
#' @param size (TODO)
#' @export
query_agg <- function(con, index = NULL, size = 1000) {
  structure(list(
    con = con,
    index = get_index(con, index),
    size = size,
    aggs = list(),
    filters = list()
  ), class = c("es_query", "query_agg"))
}

#' Specify a field to aggregate by
#'
#' @param query a [query_agg()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @export
agg_by_field <- function(query, field) {
  check_class(query, "query_agg", "agg_by_field")
  res <- list()
  res[[field]] <- list(
    terms = list(field = field)
  )
  query$aggs <- c(query$aggs, list(res))
  query
}

# milliseconds (ms), seconds (s), minutes (m), hours (h), days (d)

#' Specify a date binning for aggregation
#'
#' @param query a [query_agg()] object
#' @param field field name (see [queryable_fields()] for all possibilities)
#' @param interval binning interval (see [calendar_interval()] or [fixed_interval()]) - default is 1-day calendar interval
#' @export
agg_by_date <- function(query, field, interval = calendar_interval("1d")) {
  check_class(query, "query_agg", "agg_by_date")

  res <- list()
  res[[field]] <- list(
    date_histogram = list(
      field = field
    )
  )
  res[[field]][["date_histogram"]][[interval$type]] <- interval$unit
  query$aggs <- c(query$aggs, list(res))
  query
}

# https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-datehistogram-aggregation.html

# more parameters will be added (time zone, etc.)...

#' Specify a calendar interval for date binning aggregation
#'
#' @param unit one of "1m" (minute), "1h" (hour), "1d" (day), "1w" (week), "1M" (month), "1q" (quarter), "1y" (year)
#' @export
calendar_interval <- function(unit = "1d") {
  valid_ints <- c("1m", "1h", "1d", "1w", "1M", "1q", "1y")

  if (!unit %in% valid_ints)
    stop("Calendar interval '", unit, "' is not valid. Must be one of ",
      paste(valid_ints, collapse = ", "), call. = FALSE)

  list(
    unit = unit,
    type = "calendar_interval"
  )
}

#' Specify a fixed time interval for date binning aggregation
#'
#' @param unit a positive integer followed by one of "ms" (millisecond), "s" (second), "m" (minute), "h" (hour), "d" (day)
#' @export
fixed_interval <- function(unit = "1d") {
  valid_suff <- c("ms", "s", "m", "h", "d")

  num <- as.integer(gsub("([0-9]+).*", "\\1", unit))
  if (num < 1)
    stop("Fixed interval must be a positive integer", call. = FALSE)

  suff <- gsub("[0-9]+(.*)", "\\1", unit)
  if (!suff %in% valid_suff)
    stop("Fixed interval '", unit, "' is not valid. Suffix must be one of ",
      paste(valid_suff, collapse = ", "), call. = FALSE)

  list(
    unit = paste0(num, suff),
    type = "fixed_interval"
  )
}
