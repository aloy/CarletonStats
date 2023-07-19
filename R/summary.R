#' @title Print a summary of an \code{carlboot} object
#'
#' @description
#' Print summary statistics and confidence intervals, if desired, for an \code{lmeresamp} object.
#'
#' @param object The carlboot object to print.
#' @param ... not used
#'
#' @rdname summary
#' @export 
#' @method summary carlboot
summary.carlboot <- function(object, ...) {
  if(is.null(attr(object, "groups"))) {
    if(attr(object, "statistic") %in% c("correlation", "slope")) {
      cat("\n\t** Bootstrap interval of", attr(object, "statistic"), "\n\n")
      cat(" Observed", attr(object, "statistic"), "between", attr(object, "x.name"), "and", attr(object, "y.name"), ":", round(attr(object, "observed"), 5), "\n")
    } else {
      cat("\n\t** Bootstrap interval for", attr(object, "statistic"), "\n\n")
      cat(" Observed ", attr(object, "x.name"), ":", round(attr(object, "observed"), 5), "\n")
    }
  } else {
    cat("\n\t** Bootstrap interval for difference of", attr(object, "statistic"), "\n\n")
    cat(" Observed difference of", attr(object, "statistic"), ":", paste(attr(object, "groups"), collapse = " - "), "=", round(attr(object, "observed"), 5), "\n")
  }
  cat(" Mean of bootstrap distribution:",  round(mean(object), 5),"\n")
  cat(" Standard error of bootstrap distribution:", round(sd(object), 5),"\n\n")
  cat(" Bootstrap percentile interval\n")
  print(confint(object, level = attr(object, "level")))
  cat("\n\t\t*--------------*\n")
}

