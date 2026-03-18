# eric.R
# Functions for searching the ERIC (Education Resources Information Center) API.
#
# ERIC API:     https://eric.ed.gov/?api
# Swagger docs: https://api.ies.ed.gov/eric/swagger-ui/
#
# SETUP:
#   install.packages(c("httr2", "tibble", "dplyr", "purrr", "cli", "curl"))
#
# BACKGROUND:
#   ERIC is a comprehensive bibliographic database of education research,
#   maintained by the US Institute of Education Sciences (IES). It includes
#   both peer-reviewed journal articles and grey literature (reports, policy
#   documents, conference papers, etc.).
#
#   ERIC nearly lost funding in 2025 -- consider acknowledging it in citations.
#
# QUERY SYNTAX:
#   The ERIC API uses a Solr-style query syntax in the `search` parameter.
#   Fields are scoped with field:value syntax, e.g. title:"AI" author:"Knight"
#   Boolean operators: AND OR NOT (uppercase required)
#   Phrase search: "artificial intelligence"
#
# KEY FILTER PARAMETERS (NOT part of the search string):
#   rows         : number of results per page (max 1000)
#   start        : offset for pagination
#   fields       : comma-separated list of fields to return
#   peerreviewed : TRUE / FALSE  (API: "T" / "F")
#                  NOTE: in the API, peerreviewed is a *search clause*, not
#                  a query parameter. Use eric_add_filter() or include
#                  "peerreviewed:T" / "peerreviewed:F" in the search string.
#
# USEFUL SEARCH PATTERNS:
#   Grey lit only:    peerreviewed:F
#   Peer reviewed:    peerreviewed:T
#   Has law/policy:   identifierslaw:[* TO *]   (any value in that field)
#   Specific source:  source:"Consortium for Policy Research in Education"
#   Policy sources:   source:policy
#
# EXAMPLE:
#   results <- eric_search(
#     search       = '"artificial intelligence"',
#     source       = "policy",
#     peerreviewed = FALSE,
#     rows         = 100
#   )

# =============================================================================
# QUERY BUILDER
# =============================================================================

#' Build an ERIC search string from named components
#'
#' Constructs the `search` parameter for the ERIC API from a free-text search
#' term and optional field-scoped clauses. Handles the distinction between
#' the main search string (which takes phrase-quoted terms) and field-scoped
#' filters (`title:`, `author:`, `source:`, `peerreviewed:`, etc.).
#'
#' In the ERIC API, `peerreviewed`, `source`, and other field filters are
#' included *within* the `search` parameter as `field:value` clauses, not
#' as separate query parameters. This function assembles them correctly.
#'
#' @param search `<string>` Free-text search term. Phrase-quote multi-word
#'   terms yourself, e.g. `'"artificial intelligence"'`.
#' @param title `<string>` Scope search to title field.
#' @param author `<string>` Scope search to author field.
#' @param source `<string>` Scope search to source/journal field.
#'   Use `"policy"` to match ERIC's policy document sources.
#' @param subject `<string>` Scope search to subject field.
#' @param description `<string>` Scope search to description/abstract field.
#' @param peerreviewed `<logical>` `TRUE` to restrict to peer-reviewed items,
#'   `FALSE` to restrict to non-peer-reviewed (grey literature) only,
#'   `NULL` (default) to include both.
#' @param has_policy_law `<logical>` If `TRUE`, adds an `identifierslaw:[* TO *]`
#'   clause to restrict results to documents with any law/policy/legislation
#'   identifier. Default `FALSE`.
#' @param ... Additional named field:value clauses, e.g.
#'   `educationlevel = "Higher Education"`.
#'
#' @return A single search string suitable for the ERIC API `search` parameter.
#'
#' @section ERIC query syntax:
#' The ERIC API uses Solr under the hood with some non-standard constraints:
#'
#' **Free-text `search`** (unscoped, searches title/author/source/subject/description):
#' - Multi-word phrases use `AND` between words: `artificial AND intelligence`
#' - Do NOT quote the free-text search -- `"artificial intelligence"` triggers
#'   a Solr PhraseQuery on a field without position data and returns an error.
#' - Boolean: `reading AND (comprehension OR fluency)`
#'
#' **Field-scoped searches** (via `title`, `author`, etc. arguments, or `.query`):
#' - Single words: `title:literacy`
#' - Phrases: `title:"artificial intelligence"` (quotes work here)
#' - These are safe to quote because named fields have position data.
#'
#' **Special filters** (always clause-style, never quoted):
#' - `peerreviewed:T` / `peerreviewed:F`
#' - `source:policy`
#' - `identifierslaw:[* TO *]`
#'
#' @examples
#' # Multi-word free-text: use AND, not quotes
#' eric_build_query(search = "artificial AND intelligence")
#'
#' # Single word is fine unquoted
#' eric_build_query(search = "literacy", source = "policy", peerreviewed = FALSE)
#'
#' # Field-scoped phrase search is safe with quotes
#' eric_build_query(title = '"artificial intelligence"', peerreviewed = TRUE)
#'
#' # Policy/law filter
#' eric_build_query(search = "climate AND adaptation", has_policy_law = TRUE)
#'
#' @export
eric_build_query <- function(search       = NULL,
                             title        = NULL,
                             author       = NULL,
                             source       = NULL,
                             subject      = NULL,
                             description  = NULL,
                             peerreviewed = NULL,
                             has_policy_law = FALSE,
                             ...) {
  clauses <- character(0)

  # Free-text search: warn if user has passed a quoted phrase, which causes
  # a Solr PhraseQuery error on ERIC's unpositioned default search field.
  if (!is.null(search) && nzchar(search)) {
    if (grepl('^\".*\"$', trimws(search))) {
      # Strip outer quotes and rewrite as AND-joined words
      inner <- gsub('^\"|\"$', "", trimws(search))
      words <- strsplit(inner, "\\s+")[[1L]]
      if (length(words) > 1L) {
        search_safe <- paste(words, collapse = " AND ")
        cli::cli_warn(c(
          "!" = "Quoted phrase search is not supported in ERIC's free-text field.",
          "i" = "Rewrote {.val {search}} as {.val {search_safe}}.",
          "i" = 'To phrase-search a specific field use e.g. title:\\"artificial intelligence\\"'
        ))
        search <- search_safe
      }
    }
    clauses <- c(clauses, search)
  }

  # Named field-scoped clauses (phrases with quotes are fine here)
  field_args <- list(
    title       = title,
    author      = author,
    source      = source,
    subject     = subject,
    description = description
  )
  extra_args <- list(...)

  all_fields <- c(field_args, extra_args)
  for (field in names(all_fields)) {
    val <- all_fields[[field]]
    if (!is.null(val) && nzchar(val)) {
      clauses <- c(clauses, sprintf("%s:%s", field, val))
    }
  }

  # peerreviewed is a search clause in ERIC, not a separate param
  if (isTRUE(peerreviewed))  clauses <- c(clauses, "peerreviewed:T")
  if (isFALSE(peerreviewed)) clauses <- c(clauses, "peerreviewed:F")

  # identifierslaw:[* TO *] matches any document with a law/policy identifier
  if (isTRUE(has_policy_law)) {
    clauses <- c(clauses, "identifierslaw:[* TO *]")
  }

  if (length(clauses) == 0L) {
    cli::cli_abort("At least one search term or field filter must be provided.")
  }

  query <- paste(clauses, collapse = " AND ")
  cli::cli_alert_info("ERIC query: {query}")
  query
}


# =============================================================================
# QUERY HELPERS
# =============================================================================

#' Build a Title-Abstract-Keyword (TAK) search for ERIC
#'
#' ERIC has no combined TAK field, but you can approximate it by ORing
#' `title:`, `description:` (abstract), and `subject:` (keyword) clauses
#' for each term. This function builds that expanded query.
#'
#' @param terms `<character>` One or more search phrases. Each is expanded
#'   across title, description, and subject fields.
#' @param extra `<string>` Optional additional clause(s) to AND onto the
#'   end of the query (e.g. `"peerreviewed:F"`, `"source:policy"`).
#'
#' @return A search string ready for [eric_search()] via `.query =`.
#'
#' @examples
#' eric_tak(
#'   terms = c("artificial intelligence", "machine learning"),
#'   extra = "peerreviewed:F"
#' )
#'
#' @export
eric_tak <- function(terms, extra = NULL) {
  stopifnot(is.character(terms), length(terms) >= 1L)

  tak_clauses <- purrr::map_chr(terms, function(term) {
    # Field-scoped searches (title:, description:, subject:) support quoted
    # phrases because those fields are indexed with position data.
    fields <- c("title", "description", "subject")
    inner  <- paste(sprintf('%s:"%s"', fields, term), collapse = " OR ")
    paste0("(", inner, ")")
  })

  query <- paste(tak_clauses, collapse = " AND ")

  if (!is.null(extra) && nzchar(extra)) {
    query <- paste(query, "AND", extra)
  }

  cli::cli_alert_info("TAK query: {query}")
  query
}

#' List all available ERIC API fields
#'
#' Returns a tibble describing every field available from the ERIC API,
#' split into default fields (returned unless you specify `fields`) and
#' extended fields.
#'
#' @return A [tibble::tibble()] with columns `field` and `type`.
#'
#' @export
eric_fields <- function() {
  tibble::tibble(
    field = c(
      # Default fields
      "author", "description", "id", "isbn", "issn", "language",
      "peerreviewed", "publicationdateyear", "publicationtype",
      "publisher", "subject", "title", "url",
      # Extended fields
      "abstractor", "audience", "authorxlink", "e_datemodified",
      "e_fulltextauth", "e_yearadded", "educationlevel",
      "identifiersgeo", "identifierslaw", "identifierstest",
      "iescited", "iesfunded", "iesgrantconstractnum",
      "iesgrantcontractnumxlink", "ieslinkdatasource",
      "ieslinkpublication", "ieslinkwwcreviewguide",
      "ieswwcreviewed", "institution", "sponsor", "sourceid",
      "source"
    ),
    type = c(
      rep("default", 13L),
      rep("extended", 19L)
    )
  )
}

