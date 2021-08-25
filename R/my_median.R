#' My median
#'
#' @param x Vector of Numeric values
#' @inheritParams stats::median
#'
#' @return
#' Median of vector x
#' @export
#'
#' @examples
#' my_median(1:12)
my_median <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {stop("x should be numeric")}
  stats::median(x, na.rm = na.rm)
}
