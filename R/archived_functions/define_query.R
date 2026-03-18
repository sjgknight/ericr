#' Define ERIC query
#'
#' @param args takes args, normally passed from search_eric
#'
#' @return q_string
#' @export
#'
#' @examples
#' define_query(list(search = "epistemic", author = "Knight"))
define_query <- function(args) {
  # create the query string.
  # Remove NULL elements
  # If exists, remove 'search' name from 'search' element
  # If 'search' is the only element i.e. length(q_string) == 1, that's the whole query.
  # collapse the string so search is first, and then any fields indicated by field:query, separated by AND
  q_string <- args[!is.null(args)]

  if (!is.null(q_string[["search"]])) {
    q_string <-
      c(unname(q_string["search"]), q_string[grep("search", names(q_string), invert =
                                                    T)])

    if (length(q_string) == 1) {
      q_string <- list(search = paste(q_string[1]))
    } else {
      q_string <- list(search =
                         paste0(
                           q_string[1],
                           " AND ",
                           paste0(names(q_string)[2:length(q_string)], ":", q_string[2:length(q_string)], collapse = " AND "),
                           collapse = " AND "
                         ))
    }
  } else {
    q_string <-
      list(search = paste0(names(q_string), ":", q_string, collapse = " AND "))

  }
  cat("searching for:", paste0(q_string))
  return(q_string)
}
