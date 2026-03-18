#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function() {
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Please check your internet connection")
}

#' @param res
#'
#' @importFrom httr status_code
check_status <- function(res) {
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = "The API returned an error"
  )
}

# Follows guidance at https://colinfay.me/build-api-wrapper-package-r/

# searches of form httr::GET(base_url, query = list(search="test"))
# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# redundant null coalescing operator, available in R since 4.4.0, Claude consistently adds it.
`%||%` <- function(x, y) if (!is.null(x)) x else y

.chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  as.character(x[[1L]])
}

.int <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_integer_)
  as.integer(x[[1L]])
}

#' Check internet connectivity (internal)
#' @noRd
.check_internet <- function() {
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection detected.")
  }
}
