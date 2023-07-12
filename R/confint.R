#' @title Calculate a CI from a \code{carlboot} object
#'
#' @description
#' Calculate percentile confidence intervals for a \code{carlboot} object.
#'
#' @param x The carlboot object to print.
#' @param level the confidence level
#' @param ... not used
#'
#' @rdname confint
#' @export 
#' @method confint carlboot
confint.carlboot <- function(x, level = 0.95, ...) {
  quantile(x, probs = (1 + c(-level, level)) / 2)
}
