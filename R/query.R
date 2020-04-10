get_query <- function(query, after = NULL) {
  check_class(query, c("query_agg", "query_fetch"), "get_query")

  if (inherits(query, "query_agg")) {
    qry <- get_query_agg(query, after = after)
  } else {
    qry <- get_query_fetch(query)
  }

  jsonlite::toJSON(qry, auto_unbox = TRUE, null = "null", pretty = TRUE)
}

#' @export
print.es_query <- function(x, ...) {
  print(get_query(x))
}

get_query_agg <- function(query, after = NULL) {
  if (length(query$aggs) == 0)
    return(structure(list(), names = character(0)))
  res <- list(
    size = 0,
    aggs = list(
      agg_results = list(
        composite = list(
          size = query$size,
          sources = query$aggs
        )
      )
    )
  )
  if (!is.null(after))
    res$aggs$agg_results$composite$after <- after

  if (length(query$filters) != 0)
    res$query <- get_filter_list(query)

  res
}

# "sort": ["processedOnDate"]

get_query_fetch <- function(query) {
  res <- list(
    size = query$size
  )

  if (length(query$filters) != 0)
    res$query <- get_filter_list(query)

  res
}

get_filter_list <- function(query) {
  list(
    bool = list(
      filter = query$filters
    )
  )
}

#' Execute a query
#'
#' @param query a [query_agg()] or [query_fetch()] object
#' @export
run <- function(query) {
  check_class(query, c("query_agg", "query_fetch"), "run")

  if (inherits(query, "query_agg")) {
    run_query_agg(query)
  } else {
    run_query_fetch(query)
  }
}

agg_build <- function(res, r) {
  dat <- r$aggregations$agg_results$buckets
  df <- NULL
  if (length(dat) > 0) {
    keys <- names(dat[[1]]$key)
    # instead of depending on dplyr's bind_rows, extract each from list
    df <- lapply(keys, function(k) {
      unlist(lapply(dat, function(a) a$key[[k]]))
    })
    names(df) <- keys
    df$count <- unlist(lapply(dat, function(x) x$doc_count))
    df$stringsAsFactors <- FALSE
    df <- do.call(data.frame, df)
  }

  rbind(
    res,
    df
  )
}

# r$hits$total$value

get_date <- function(x)
  as.POSIXct((x + 0.1) / 1000, origin = "1970-01-01", tz = "UTC")

#' @importFrom elastic Search
run_query_agg <- function(query) {
  qry <- get_query(query)
  r <- elastic::Search(query$con$con, index = query$index, body = qry)
  after_key <- r$aggregations$agg_results$after_key
  res <- agg_build(NULL, r)

  while (!is.null(after_key)) {
    qry <- get_query(query, after = after_key)
    r <- elastic::Search(query$con$con, index = query$index, body = qry)
    after_key <- r$aggregations$agg_results$after_key
    res <- agg_build(res, r)
  }

  # fix dates
  nms <- names(res)
  flds <- queryable_fields(query$con)
  dts <- flds$field[flds$type == "date"]
  dt_nms <- nms[nms %in% dts]
  for (nm in dt_nms)
    res[[nm]] <- get_date(res[[nm]])

  tibble::as_tibble(res)
}

#' @importFrom elastic scroll scroll_clear
run_query_fetch <- function(query) {
  qry <- get_query(query)
  r <- elastic::Search(query$con$con, index = query$index, time_scroll = query$time_scroll, body = qry)

  tot_hits <- cur_hits <- r$hits$total$value
  message("Fetching ", tot_hits, " total documents...")
  if (tot_hits > 10000 && is.null(query$path))
    message("This is a large query. Beginning to fetch to memory, but consider aborting and using the 'path' argument with query_fetch() to the write results to disk.")

  out <- r$hits$hits
  counter <- 1
  cum_hits <- length(out)
  if (!is.null(query$path)) {
    jsonlite::write_json(out,
      path = sprintf("%s/out%04d.json", query$path, counter),
      auto_unbox = TRUE, null = "null")
  }
  message(min(query$size, length(out)), " documents fetched (",
        round(100 * cum_hits / tot_hits), "%)...")

  while (cur_hits != 0) {
    counter <- counter + 1
    r <- elastic::scroll(query$con$con, r$`_scroll_id`, time_scroll = query$time_scroll)
    cur_hits <- length(r$hits$hits)
    if (cur_hits > 0) {
      if (!is.null(query$path)) {
        jsonlite::write_json(out,
          path = sprintf("%s/out%04d.json", query$path, counter),
          auto_unbox = TRUE, null = "null")
      } else {
          out <- c(out, r$hits$hits)
      }
      cum_hits <- cum_hits + length(r$hits$hits)
      message(cum_hits, " documents fetched (",
        round(100 * cum_hits / tot_hits), "%)...")
    }
  }
  elastic::scroll_clear(query$con$con, r$`_scroll_id`)
  if (!is.null(query$path))
    return(query$path)
  out
}
