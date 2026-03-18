# Create a function for the pagination used below
get_pages <- function(meta,
                      content,
                      q_string,
                      fields,
                      rows,
                      base_url,
                      ...) {
  content <- content

  remaining <- max((meta$numFound - meta$start - rows), 0)
  n_pages <- floor(remaining / 1000)

  attempt::stop_if(n_pages, ~ . == 0, "no more pages to retrieve")

  initial_start <- meta$start + rows
  start_points <- if (n_pages == 1) {
    initial_start
  } else {
    results <- vapply(c(1:n_pages), function(page) {
      page * 1000 + initial_start + page
    })
    c(initial_start, results)
  }

  cat("retrieving: ", n_pages, " pages from ", start_points)

  # if (exists("fields")){
  #   fields <- fields
  # } else {
  #   fields <- NULL
  # }

  call_search <- function(start_point, ...) {
    params <- c(fields = fields,
                rows = 1000,
                start = start_point)

    pages <- search_eric_base(q_string = q_string,
                              q = params,
                              base_url = "https://api.ies.ed.gov/eric/")
    return(as.data.frame(pages$content))

  }

  content_pages <- purrr::map(start_points, call_search)


  output <- content_pages#do.call(rbind, content_pages)
  output <- do.call(rbind, output)

  return(output)
}
