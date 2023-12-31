library(devtools)
library(usethis)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "ericr")

#Set your name
my_desc$set("Authors@R", "person('Simon', 'Knight', email = 'sjgknight@gmail.com', role = c('cre', 'aut'))")

my_desc$set("desc_add_orcid", "('0000-0002-2815-0399',
                     given = 'Simon',
                     family = 'Knight')")

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.0.9000")

# The title of your package
my_desc$set(Title = "Package to connect to ERIC API with R")
# The description of your package
my_desc$set(Description = "Package wraps the Education Resources Information Center (ERIC) API for R.")
# The urls
my_desc$set("URL", "http://github.com/sjgknight/ericr")
my_desc$set("BugReports", "http://github.com/sjgknight/ericr")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, and lifecycle badge
use_mit_license(name = "Simon Knight")
use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()

# Get the dependencies
use_package("httr")
use_package("jsonlite")
use_package("curl")
use_package("attempt")
use_package("purrr")

# Clean your description
use_tidy_description()
