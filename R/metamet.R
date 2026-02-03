#' dummy
#'
#' A dummy function for testing
#' @param x_in url to the template repository
#'
#' @details none
#' @importFrom stats median
#' @examples
#' \dontrun{
#' dummy(1:5)
#' }
dummy <- function(x = 1:10) {
  median(x)
}
