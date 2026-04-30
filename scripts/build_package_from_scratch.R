library(devtools)
library(usethis)
library(pkgload)
library(pkgbuild)
library(pkgdown)

usethis::use_package_doc()

# add to .RBuildIgnore
usethis::use_build_ignore(c("data-raw"))
# add air to GitHub Actions
usethis::use_github_action(
  url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml"
)

# # add the packages requiring importing to package.r
use_tidy_eval()
use_data_table()
use_package("data.table", "Depends")
use_package("dplyr", "Imports")
use_package("ggplot2", "Depends")
use_package('ggiraph', 'Imports')
use_package('glue', 'Imports') # used in one line only - should perhaps replace
use_package('here', 'Imports')
use_package("fs", "Imports", min_version = "1.5.0")
use_package("leaflet", "Imports")
use_package("lubridate", "Imports")
use_package("mgcv", "Imports")
use_package("openair", "Imports")
use_package("openairmaps", "Imports")
use_package("powerjoin", "Imports")
use_package("readxl", "Imports")
use_package("shiny", "Imports")
use_package('shinycssloaders', 'Imports')
use_package('shinydashboard', 'Imports')
use_package('shinyjs', 'Imports')
use_package('shinyvalidate', 'Imports')
use_package("utils", "Imports")


document()
pkgload::load_all()
devtools::test()
build(vignettes = FALSE)
check(vignettes = FALSE)
install(".")
pkgload::load_all()
detach("package:metamet", unload = TRUE)
pkgdown::build_site()
