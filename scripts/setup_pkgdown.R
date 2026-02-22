library(here)
library(sloop)
library(devtools)
library(usethis)
library(pkgload)
library(pkgbuild)
library(pkgdown)

# usethis::use_pkgdown_github_pages()
use_readme_rmd()
devtools::build_readme()
use_news_md()
use_vignette("metamet") #substitute with the name of your package
use_github_links()
usethis::use_logo(img = "data-raw/weather_station.jpg")
build_site()

detach("package:metamet", unload = TRUE)
