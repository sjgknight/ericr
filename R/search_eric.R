#' Main function to search eric and return results
#'
#' @param search general search, can use fields
#'        (title, author, source, subject, or description).
#'        Function assumes that AND is used to combine search fields where multiple are specified;
#'        If AND is not the intended operator, manually include the fields in the 'search' variable
#' @param title search-field
#' @param author search-field
#' @param source search-field
#' @param subject search-field
#' @param description search-field
#' @param start the record number to start from. Default 0
#' @param rows the number of records to return, default 100, max 1000, set to 0 for just meta
#' @param check whether to check before returning results
#' @param fields the fields to return.
#'        If set to NULL it returns c(author, description, id, isbn, issn, language, peerreviewed, publicationdateyear, publicationtype, publisher, subject, title).
#'        Full list is c(abstractor, audience, authorxlink, e_datemodified, e_fulltextauth, e_yearadded, educationlevel, identifiersgeo, identifierslaw, identifierstest, iescited, iesfunded, iesgrantconstractnum, iesgrantcontractnumxlink, ieslinkdatasource, ieslinkpublication, ieslinkwwcreviewguide, ieswwcreviewed, institution, sponsor, sourceid, source, url)
#' @param base_url unlikely to change, default is valid.
#' @description
#' Simple wrapper for the ERIC search API, main function, returns a dataframe and
#' some metadata to console. Documentation is largely restating the ERIC API materials.
#' Education Resources Information Center,
#' "ERIC is a comprehensive, easy-to-use, searchable,
#' Internet-based bibliographic and full-text database of education research and information"
#'
#' @return content_new
#' @export
#' @importFrom glue glue
#' @import httr
#' @import attempt
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' search_eric(search = "epistemic cognition", author = "Knight, S")
search_eric <- function(search = NULL,
                        title = NULL,
                        author = NULL,
                        source = NULL,
                        subject = NULL,
                        description = NULL,
                        start = 0,
                        rows = 100,
                        check = FALSE,
                        fields = c(
                          "author",
                          "description",
                          "id",
                          "isbn",
                          "issn",
                          "language",
                          "peerreviewed",
                          "publicationdateyear",
                          "publicationtype",
                          "publisher",
                          "subject",
                          "title",
                          "url"
                        ),
                        base_url = "https://api.ies.ed.gov/eric/") {
  # Dummy
  # args <- c(search = "hello world", title = "test title", author = "Knight Simon", source = "source", description = "description")
  # args <- list(search = "epistemic", author = "Knight Simon")
  # args <- list(search = "epistemic")
  # q <- c(fields = NULL, rows = 100, start = 0)

  # start <- ifelse(is.null(start), 0, start)
  # rows <- ifelse(is.null(rows), 100, rows)


  # get the base parameters
  q <- c(fields = fields,
         rows = rows,
         start = start)

  # Check there's something in the search
  args <-
    list(
      search = search,
      title = title,
      author = author,
      source = source,
      subject = subject,
      description = description
    )

  attempt::stop_if_all(args,
                       is.null,
                       glue::glue("You need to specify at least one item to search in {args}"))

  # Check there's internet
  check_internet()

  #write query
  q_string <- define_query(unlist(args))

  out <-
    search_eric_base(q_string = q_string,
                     q = q,
                     base_url = "https://api.ies.ed.gov/eric/")

  attempt::stop_if(out$meta$numFound, ~ . == 0, msg = "That search doesn't return any results")
  ##############################################################
  ##############################################################
  # If we returned fewer rows than rows queried, then just return
  meta <- out$meta
  content_first <- out$content

  cat("\nthis is the start of pagination\n")
  #cat(paste0(meta), "\n")
  #cat(paste0(head(content)))

  cat(meta$numFound, "and start ", meta$start, " and rows", rows)

  if ((meta$numFound - meta$start) <= rows) {
    content_new <- content_first
  }

  # If we found more rows than rows queried, then...
  if ((meta$numFound - meta$start) > rows) {
    if (exists("fields")) {
      fields <- fields
    } else {
      fields <- NULL
    }

    cat("retrieving: ", n_pages, " pages from ", start_points)


    args_to_pass <<- list(
      meta = meta,
      content = content_first,
      q_string = q_string,
      fields = fields,
      rows = rows,
      base_url = "https://api.ies.ed.gov/eric/"
    )

    # If we want to interactively check, then ask for input before getting all
    if (check == TRUE) {
      cat(
        "There are ",
        meta$numFound,
        " results. Do you want to get them all? (Y for yes, N for no)"
      )
      resp <- tolower(readline(""))
      if (resp == "y") {
        content_new <- rlang::exec(get_pages,!!!args_to_pass)
        content_new <-
          list(first = as.data.frame(content_first),
               second = as.data.frame(content_new))
        content_new <-
          dplyr::bind_rows(content_new$first, content_new$second)
      }
    }
    if (check == FALSE) {
      content_new <- rlang::exec(get_pages,!!!args_to_pass)
      content_new <-
        list(first = as.data.frame(content_first),
             second = as.data.frame(content_new))
      content_new <-
        dplyr::bind_rows(content_new$first, content_new$second)

    }
  }

  return(content_new)
}
