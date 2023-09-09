<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`devtools::install_github("sjgknight/ericr")`

Follows guidance at https://colinfay.me/build-api-wrapper-package-r/
And https://ropensci.org/blog/2022/06/16/publicize-api-client-yes-no/ 
(or, at least, they were very helpful and any errors are my own)


Using ERIC API documented at https://eric.ed.gov/?api#/default/get_eric_ 

For Python check https://eric.ed.gov/pdf/Using_ERIC_API_for_Research_Topics.pdf

See for an alternative approach to the functions https://httr2.r-lib.org/articles/wrapping-apis.html 

A first attempt at a package, it is very rough...

It would be useful to extend by:

1. Adjusting the search approach to a different incremental method (ideally one that would allow updating by date, without adjusting the query)
2. Would facilitate search structures that are irritating to run manually, e.g. ERIC has no 'Title-Abstract-Keyword' search, but this could be done (inefficiently) programmatically and results combined/through the define_query function which could take TAK as a field and correctly create the Boolean (Title:[terms] OR Abstract:[terms] OR Keyword:[terms], for each comma separated term-phrase provided)
3. Modify the functions for specifying Boolean logic between query elements (ERIC 'advanced search' is not particularly intuitive in the UI so this seems like a thing that could be made easier in a package. The proquest UI to ERIC is easier)
4. General tidying up of code...

x <- list(TAK = c("hello world", "second one"))
purrr::map(x$TAK, function(.x){
    mytak <- list(Title = .x, Abstract = .x, Keyword = .x)
    mytak <- paste0(names(mytak), ":", mytak, collapse = " OR ")
    paste0("(", mytak ,")")}) |>
    paste0(collapse = " AND ")

Then build it with:
devtools::check()
devtools::build()

use_testthat()
use_vignette("{your-package-name}")
use_readme_rmd()


