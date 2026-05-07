pkg_extdata <- function(...) {
  system.file("extdata", ..., package = "metamet", mustWork = TRUE)
}
