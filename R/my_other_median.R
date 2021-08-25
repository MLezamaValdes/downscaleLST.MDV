#' My Other median
#'
#' @param x Vector of Numeric values
#' @inheritParams stats::median
#'
#' @return
#' Median of vector x
#' @export
#'
#' @examples
#' my_other_median(1:12)
my_other_median <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {stop("x should be numeric")}
  sub_median(x, na.rm =na.rm)
}

#' Core of the median not exported
#' @param x Vector of Numeric values
#' @inheritParams stats::median
sub_median <- function(x, na.rm = TRUE) {
  stats::median(x, na.rm)
}
