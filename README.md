# elasticquery

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/hafen/elasticquery.svg?branch=master)](https://travis-ci.org/hafen/elasticquery)
<!-- badges: end -->

The elasticquery package provides utilities for building Elasticsearch queries in a simple and intuitive way.

## Installation

You can install the package from Github with:

```r
# install.packages("remotes")
remotes::install_github("hafen/elasticquery")
```

## Examples

```r
# connect and specify a primary index for queries
con <- es_connect(
  host = "127.0.0.1", port = 9200,
  primary_index = "eios-items")

# initialize a query
query <- query_agg(con)

# view the query (empty)
query

# run the query
run(query)

# see what what can we aggregate on?
queryable_fields(con)

# aggregate "tags" field
query <- query_agg(con) %>%
  agg_by_field("tags")
query
run(query)

# aggregate on multiple fields
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_field("affectedCountriesIso") %>%
  run()

# aggregate by date (default binning is daily, UTC)
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate") %>%
  run()

# aggregate by a date using a weekly calendar interval
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate", calendar_interval("1w")) %>%
  run()

# aggregate by a date using a 10-day fixed interval
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate", fixed_interval("10d")) %>%
  run()

# add a date filter range to aggregation
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate", calendar_interval("1w")) %>%
  filter_range("processedOnDate", from = "2018-01-01") %>%
  run()

# add a terms filter to aggregation
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate", calendar_interval("1w")) %>%
  filter_range("processedOnDate", from = "2018-01-01") %>%
  filter_terms("affectedCountriesIso", c("US", "CA")) %>%
  run()

# add a match filter to aggregation
query_agg(con) %>%
  agg_by_field("tags") %>%
  agg_by_date("processedOnDate", calendar_interval("1w")) %>%
  filter_range("processedOnDate", from = "2018-01-01") %>%
  filter_terms("affectedCountriesIso", c("US", "CA")) %>%
  filter_match("fullText", "disease") %>%
  run()

# fetch all documents
docs <- query_fetch(con) %>%
  run()

# fetch documents using filters
docs <- query_fetch(con) %>%
  filter_range("processedOnDate", from = "2018-01-01") %>%
  filter_terms("affectedCountriesIso", c("US", "CA")) %>%
  filter_match("fullText", "disease") %>%
  run()

# fetch and save to disk
tf <- tempfile()
dir.create(tf)
docs <- query_fetch(con, path = tf, size = 10) %>%
  filter_range("processedOnDate", from = "2018-01-01") %>%
  filter_terms("affectedCountriesIso", c("US", "CA")) %>%
  filter_match("fullText", "disease") %>%
  run()

list.files(docs)
```

