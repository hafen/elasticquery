#' @importFrom jsonlite fromJSON prettify toJSON
get_query <- function(query, after = NULL) {
  check_class(query, c("query_agg", "query_fetch", "query_str"), "get_query")

  if (inherits(query, "query_agg")) {
    qry <- get_query_agg(query, after = after)
  } else if (inherits(query, "query_fetch")) {
    qry <- get_query_fetch(query)
  } else if (inherits(query, "query_str")) {
    if (query$type == "agg") {
      qry <- jsonlite::fromJSON(query$str, simplifyVector = FALSE)
      if (!is.null(after))
        qry$aggs$agg_results$composite$after <- after
    } else {
      return(jsonlite::prettify(query$str))
    }
  } else {
    stop("Not a valid query type")
  }

  jsonlite::toJSON(qry, auto_unbox = TRUE, null = "null", pretty = TRUE,
    digits = NA)
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
  res[["_source"]] <- query$select
  res$sort <- query$sort

  if (length(query$filters) != 0)
    res$query <- get_filter_list(query)

  res
}

get_filter_list <- function(query) {
  list(bool = query$filters)
}

#' Execute a query
#'
#' @param query a [query_agg()], [query_fetch()], or [query_str()] object
#' @export
run <- function(query) {
  check_class(query, c("query_agg", "query_fetch", "query_str"), "run")

  if (inherits(query, "query_agg")) {
    run_query_agg(query)
  } else if (inherits(query, "query_fetch")) {
    run_query_fetch(query)
  } else if (inherits(query, "query_str")) {
    run_query_str(query)
  } else {
    stop("Not a valid query type")
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

run_query_str <- function(query) {
  if (query$type == "agg") {
    run_query_agg(query)
  } else if (query$type == "fetch") {
    run_query_fetch(query)
  } else {
    r <- elastic::Search(query$con$con, index = query$index,
      body = query$str)
  }
}

#' @importFrom elastic scroll scroll_clear
run_query_fetch <- function(query) {
  qry <- get_query(query)
  r <- elastic::Search(query$con$con, index = query$index,
    time_scroll = query$time_scroll, body = qry)

  tot_hits <- cur_hits <- r$hits$total$value
  if (tot_hits == 0) {
    message("No documents found...")
    return(list())
  }

  tot_hits_str <- tot_hits
  if (query$max > 0) {
    tot_hits <- query$max
    if (query$max < query$size) {
      tot_hits_str <- paste0("first ", query$max, " of ", tot_hits)
    } else {
      tot_hits_str <- tot_hits
    }
  }
  message("Fetching ", tot_hits_str, " total documents...")
  if (tot_hits > 10000 && is.null(query$path))
    message("This is a large query. Beginning to fetch to memory, but consider aborting and using the 'path' argument with query_fetch() to the write results to disk.")

  out <- r$hits$hits
  counter <- 1
  cum_hits <- length(out)
  if (!is.null(query$path)) {
    jsonlite::write_json(out,
      path = sprintf("%s/out%04d.json", query$path, counter),
      auto_unbox = TRUE, null = "null", digits = NA)
  }
  sz_str <- min(query$size, length(out))
  denom <- tot_hits
  cum_str <- cum_hits
  if (query$max > 0) {
    sz_str <- min(sz_str, query$max)
    denom <- min(query$max, tot_hits)
    cum_str <- min(cum_hits, query$max)
  }
  message(sz_str, " documents fetched (",
        round(100 * cum_str / denom), "%)...")

  while (cur_hits != 0 && (query$max <= 0 ||
    (query$max > 0 && cum_hits < query$max))) {

    counter <- counter + 1
    r <- elastic::scroll(query$con$con, r$`_scroll_id`,
      time_scroll = query$time_scroll)
    cur_hits <- length(r$hits$hits)
    if (cur_hits > 0) {
      if (!is.null(query$path)) {
        jsonlite::write_json(r$hits$hits,
          path = sprintf("%s/out%04d.json", query$path, counter),
          auto_unbox = TRUE, null = "null", digits = NA)
      } else {
          out <- c(out, r$hits$hits)
      }
      cum_hits <- cum_hits + length(r$hits$hits)
      denom <- tot_hits
      cum_str <- cum_hits
      if (query$max > 0) {
        cum_hits <- min(cum_hits, query$max)
        denom <- min(query$max, tot_hits)
        cum_str <- min(cum_hits, query$max)
      }
      message(cum_hits, " documents fetched (",
        round(100 * cum_str / denom), "%)...")
    }
  }
  elastic::scroll_clear(query$con$con, r$`_scroll_id`)
  if (!is.null(query$path))
    return(query$path)
  if (query$max > 0)
    return(out[1:query$max])
  out
}
