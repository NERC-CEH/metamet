# This uses renv to check the package dependencies
# It seems to mostly work, but not completely

source("R/utils.R")
df <- renv::dependencies("./R")
v_pkgs <- unique(df$Package)
# generate code to modify DESCRIPTION
paste0("use_package('", v_pkgs, "', 'Imports')")

use_package('data.table', 'Imports')
use_package('fs', 'Imports', min_version = "1.5.0")
use_package('mgcv', 'Imports')
use_package('openair', 'Imports')
use_package('powerjoin', 'Imports')
use_package('readxl', 'Imports')
use_package('dplyr', 'Imports')
use_package('leaflet', 'Imports')
use_package('openairmaps', 'Imports')
use_package('forcats', 'Imports')
use_package('ggExtra', 'Imports')
use_package('lubridate', 'Imports')
use_package('shiny', 'Imports')
use_package('utils', 'Imports')

# additional pakgs in shiny app
df <- renv::dependencies("inst/shinyApp/metqc_app/app.R")
v_pkgs_app <- unique(df$Package)
ind <- v_pkgs_app %!in% v_pkgs
v_pkgs_app <- v_pkgs_app[ind]

# generate code to modify DESCRIPTION
paste0("use_package('", v_pkgs_app, "', 'Imports')")

use_package('here', 'Imports')
use_package('shinycssloaders', 'Imports')
use_package('shinyjs', 'Imports')

# manually added
use_package('shinydashboard', 'Imports')
use_package('shinyvalidate', 'Imports')
use_package('glue', 'Imports') # used in one line only - should perhaps replace

pkg_remove(pkg)

x <- list.functions.in.file("inst/shinyApp/metqc_app/app.R", alphabetic = TRUE)
str(x)

# # previous package list hard-coded
# library(shinyWidgets)
# library(shinyjs)
# library(shinydashboard)
# library(dplyr)
# library(ggplot2)
# library(ggiraph)
# library(mgcv)
# library(DT)
# library(data.table)
# library(lubridate)
# library(ggExtra)
# library(openair)
# library(powerjoin)
# library(pins)
# library(glue)
# library(shinycssloaders)
# library(shinyalert)
# library(stringr)
# library(forcats)
# library(shinyvalidate)
# library(markdown)
