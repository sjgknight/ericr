# =============================================================================
# CORE API CALL
# =============================================================================

#' Perform a single ERIC API request (internal)
#'
#' @param search_string `<string>` The assembled search string.
#' @param rows `<integer>` Results per page (max 2000 per API docs).
#' @param start `<integer>` Pagination offset.
#' @param fields `<character>` Fields to return.
#' @param base_url `<string>` API base URL.
#' @param timeout `<integer>` Timeout in seconds.
#'
#' @return A list: `meta` (numFound, start, numFoundExact), `content` (tibble).
#'   Named `content` to match the original ericr package conventions.
#' @noRd
.eric_request <- function(search_string,
                          rows     = 100L,
                          start    = 0L,
                          fields   = NULL,
                          base_url = "https://api.ies.ed.gov/eric/",
                          timeout  = 30L) {

  # format=json is REQUIRED -- without it the API returns XML and
  # resp_body_json() will fail silently or error.
  query_params <- list(
    search = search_string,
    format = "json",
    rows   = as.integer(min(rows, 2000L)),  # API max is 2000
    start  = as.integer(start)
  )
  if (!is.null(fields) && length(fields) > 0L) {
    query_params$fields <- paste(fields, collapse = ",")
  }

  resp <- tryCatch(
    httr2::request(base_url) |>
      httr2::req_url_query(!!!query_params) |>
      httr2::req_timeout(as.integer(timeout)) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_abort("Network error: {e$message}")
    }
  )

  status <- httr2::resp_status(resp)
  if (status != 200L) {
    cli::cli_abort("ERIC API returned HTTP {status}.")
  }

  # ERIC always returns Content-Type: text/plain even for valid JSON, so
  # resp_body_json() (which checks content-type) will always error.
  # Use resp_body_string() + jsonlite::fromJSON() instead.
  raw_string <- httr2::resp_body_string(resp)

  body <- tryCatch(
    jsonlite::fromJSON(raw_string, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse ERIC API response as JSON.",
        "i" = "Content-Type: {httr2::resp_header(resp, 'Content-Type') %||% 'unknown'}",
        "i" = "Preview: {substr(raw_string, 1L, 300L)}"
      ))
    }
  )

  # ERIC returns Solr error objects when the query is malformed
  if (!is.null(body[["error"]])) {
    err_msg <- body[["error"]][["msg"]] %||% "Unknown Solr error"
    cli::cli_abort(c(
      "ERIC API query error: {err_msg}",
      "i" = "Check your query syntax. Phrase search across core fields uses",
      "i" = "  Blended AND Learning  (not quoted strings).",
      "i" = "  To phrase-search a specific field use  title:\"blended learning\"",
      "i" = "  Use eric_build_query() to construct queries safely."
    ))
  }

  # Response is nested under body$response (Solr format)
  resp_block <- body[["response"]] %||% list()

  numFound     <- .int(resp_block[["numFound"]])
  start_at     <- .int(resp_block[["start"]])
  numFoundExact <- resp_block[["numFoundExact"]] %||% NA

  docs <- resp_block[["docs"]] %||% list()

  # With simplifyVector=TRUE, docs is already a data.frame when results exist
  content <- if (is.data.frame(docs) && nrow(docs) > 0L) {
    tibble::as_tibble(docs)
  } else if (is.list(docs) && length(docs) > 0L) {
    # Fallback: list of lists (simplifyVector didn't fully flatten)
    dplyr::bind_rows(purrr::map(docs, \(d) tibble::as_tibble(as.list(d))))
  } else {
    tibble::tibble()
  }

  cli::cli_alert_info(
    "numFound: {numFound} | start: {start_at} | returned: {nrow(content)}"
  )

  # Return list with same names as original ericr package (meta + content)
  list(
    meta = tibble::tibble(
      numFound      = numFound,
      start         = start_at,
      FoundExact    = numFoundExact
    ),
    content = content
  )
}

# =============================================================================
# MAIN SEARCH FUNCTION
# =============================================================================

#' Search the ERIC API
#'
#' Queries the ERIC education research database and returns results as a tibble.
#' Handles pagination automatically up to `max_results`.
#'
#' ERIC includes both peer-reviewed journal articles and grey literature
#' (policy documents, reports, conference papers, etc.). Use `peerreviewed`
#' to filter, or `has_policy_law` to restrict to documents with law/policy
#' identifiers. See [eric_build_query()] for query construction details.
#'
#' @param search `<string>` Free-text search term. Phrase-quote multi-word
#'   terms: `'"artificial intelligence"'`.
#' @param title,author,source,subject,description `<string>` Optional
#'   field-scoped search clauses.
#' @param peerreviewed `<logical>` `TRUE` = peer-reviewed only;
#'   `FALSE` = grey literature only; `NULL` (default) = all.
#' @param has_policy_law `<logical>` If `TRUE`, restricts to documents with
#'   any law/policy identifier in `identifierslaw`. Default `FALSE`.
#' @param rows `<integer>` Results per API page (max 1000). Default `100L`.
#' @param max_results `<integer>` Maximum total results to return across all
#'   pages. Set to `Inf` to retrieve everything (can be slow for large result
#'   sets -- ERIC has rate limits). Default `100L`.
#' @param fields `<character>` Fields to return. Defaults to the most useful
#'   set. Pass `NULL` to get all available fields.
#' @param pause `<numeric>` Seconds to wait between paginated requests.
#'   Default `1`. Increase if you hit rate limits.
#' @param base_url `<string>` API base URL. Unlikely to need changing.
#' @param timeout `<integer>` Request timeout in seconds. Default `30L`.
#' @param .query `<string>` Optional pre-built query string (overrides all
#'   other search arguments). Useful for complex queries built manually or
#'   with [eric_build_query()].
#'
#' @return A [tibble::tibble()] of results. Common columns include:
#'   `id`, `title`, `author`, `source`, `publicationdateyear`,
#'   `publicationtype`, `peerreviewed`, `subject`, `description`,
#'   `url`, `publisher`, `language`, `isbn`, `issn`.
#'   Run `eric_fields()` to see the full field list.
#'
#' @examples
#' \dontrun{
#' # Peer-reviewed AI education research
#' eric_search(
#'   search       = '"artificial intelligence"',
#'   peerreviewed = TRUE,
#'   rows         = 200L
#' )
#'
#' # Grey literature from policy sources only
#' eric_search(
#'   search       = '"artificial intelligence"',
#'   source       = "policy",
#'   peerreviewed = FALSE
#' )
#'
#' # Documents with any law/policy identifier
#' eric_search(
#'   search         = '"climate adaptation"',
#'   has_policy_law = TRUE
#' )
#'
#' # Title + abstract + keyword (TAK) search via pre-built query
#' tak_query <- eric_tak(
#'   terms = c("artificial intelligence", "machine learning"),
#'   extra = "peerreviewed:F"
#' )
#' eric_search(.query = tak_query, max_results = 500L)
#' }
#'
#' @export
eric_search <- function(search        = NULL,
                        title         = NULL,
                        author        = NULL,
                        source        = NULL,
                        subject       = NULL,
                        description   = NULL,
                        peerreviewed  = NULL,
                        has_policy_law = FALSE,
                        rows          = 100L,
                        max_results   = 100L,
                        fields        = c("id", "title", "author", "source",
                                          "publicationdateyear", "publicationtype",
                                          "peerreviewed", "subject", "description",
                                          "url", "publisher", "language",
                                          "isbn", "issn", "identifierslaw"),
                        pause         = 1,
                        base_url      = "https://api.ies.ed.gov/eric/",
                        timeout       = 30L,
                        .query        = NULL) {

  .check_internet()

  # Build or use pre-supplied query string
  search_string <- .query %||% eric_build_query(
    search        = search,
    title         = title,
    author        = author,
    source        = source,
    subject       = subject,
    description   = description,
    peerreviewed  = peerreviewed,
    has_policy_law = has_policy_law
  )

  rows <- min(as.integer(rows), 2000L)  # API max is 2000

  # -- First page -------------------------------------------------------------
  first <- .eric_request(search_string,
                         rows     = rows,
                         start    = 0L,
                         fields   = fields,
                         base_url = base_url,
                         timeout  = timeout)

  # meta is a tibble (matching original ericr); extract scalar values
  total <- first$meta$numFound
  cli::cli_alert_success("Total results available: {total %||% 'unknown'}")

  if (is.na(total) || total == 0L) {
    cli::cli_alert_warning("No results found.")
    return(tibble::tibble())
  }

  # Use `content` to match original ericr package naming convention
  all_content <- list(first$content)

  # -- Subsequent pages -------------------------------------------------------
  retrieved <- nrow(first$content)
  start     <- rows

  while (retrieved < min(total, max_results) && start < total) {
    Sys.sleep(pause)

    batch_rows <- min(rows, min(total, max_results) - retrieved)

    page <- .eric_request(search_string,
                          rows     = batch_rows,
                          start    = start,
                          fields   = fields,
                          base_url = base_url,
                          timeout  = timeout)

    if (nrow(page$content) == 0L) break

    all_content <- c(all_content, list(page$content))
    retrieved   <- retrieved + nrow(page$content)
    start       <- start + batch_rows
  }

  out <- dplyr::bind_rows(all_content)
  cli::cli_alert_success("Retrieved {nrow(out)} of {total} total results.")
  out
}
