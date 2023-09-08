search_eric_base <- function(q_string, q, base_url = base_url) {
  # Set remainder of query
  q <- q[!is.null(q)]

  res <- httr::GET(base_url, query = c(q_string, q))

  # Check the result
  check_status(res)

  # get the content and return it as a data.frame

  content <- jsonlite::fromJSON(rawToChar(res$content))

  meta <- as.data.frame(
    list(
      numFound = content$response$numFound,
      start = content$response$start,
      FoundExact = content$response$numFoundExact
    )
  )
  content <- content$response$docs

  cat(
    "\nnumFound is:",
    meta$numFound,
    " start is:",
    meta$start,
    " FoundExact is:",
    meta$FoundExact
  )
  out <- list(meta = meta, content = content)
  return(out)
}
