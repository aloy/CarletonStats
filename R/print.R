#' @title Print a summary of an \code{carlboot} object
#'
#' @description
#' Print summary statistics and confidence intervals, if desired, for an \code{lmeresamp} object.
#'
#' @param x The carlboot object to print.
#' @param ... not used
#'
#' @rdname print
#' @export 
#' @method print carlboot
print.carlboot <- function(x, ...) {
  if(is.null(attr(x, "groups"))) {
    if(attr(x, "statistic") %in% c("correlation", "slope")) {
      cat("\n\t** Bootstrap interval of", attr(x, "statistic"), "\n\n")
      cat(" Observed", attr(x, "statistic"), "between", attr(x, "x.name"), "and", attr(x, "y.name"), ":", round(attr(x, "observed"), 5), "\n")
    } else {
      cat("\n\t** Bootstrap interval for", attr(x, "statistic"), "\n\n")
      cat(" Observed ", attr(x, "x.name"), ":", round(attr(x, "observed"), 5), "\n")
    }
  } else {
    cat("\n\t** Bootstrap interval for difference of", attr(x, "statistic"), "\n\n")
    cat(" Observed difference of", attr(x, "statistic"), ":", paste(attr(x, "groups"), collapse = " - "), "=", round(attr(x, "observed"), 5), "\n")
  }
  cat(" Mean of bootstrap distribution:",  round(mean(x), 5),"\n")
  cat(" Standard error of bootstrap distribution:", round(sd(x), 5),"\n\n")
  cat(" Bootstrap percentile interval\n")
  print(confint(x, level = attr(x, "level")))
  cat("\n\t\t*--------------*\n")
}

