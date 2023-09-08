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

