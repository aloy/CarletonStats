#' @title Print a summary of an \code{carlboot} object
#'
#' @description
#' Print summary statistics and confidence intervals for an \code{carlboot} object.
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
    if(length(attr(x, "groups")) > 1) {
      cat("\n\t** Bootstrap interval for difference of", attr(x, "statistic"), "\n\n")
      cat(" Observed difference of", attr(x, "statistic"), ":", paste(attr(x, "groups"), collapse = " - "), "=", round(attr(x, "observed"), 5), "\n")
    } else {
      cat("\n\t**Bootstrap interval for mean of paired difference \n\n")
      cat(" Observed mean of ", attr(x, "y.name"), "-", attr(x, "x.name"), ":", round(attr(x, "observed"), 5), "\n")
    } 
  }
  cat(" Mean of bootstrap distribution:",  round(mean(x), 5),"\n")
  cat(" Standard error of bootstrap distribution:", round(sd(x), 5),"\n\n")
  cat(" Bootstrap percentile interval\n")
  print(confint(x, level = attr(x, "level")))
  cat("\n\t\t*--------------*\n")
}


#' @title Print a summary of an \code{carlperm} object
#'
#' @description
#' Print summary statistics, standard error, and p-value for a \code{carlperm} object.
#'
#' @param x The carlperm object to print.
#' @param ... not used
#'
#' @rdname print
#' @export 
#' @method print carlperm
print.carlperm <- function(x, ...) {
  cat("\n\t** Permutation test **\n")
  cat("\n Permutation test with alternative:", attr(x, "alternative"),"\n")
  cat(" Observed statistic\n")
  cat(" ", attr(x, "groups")[1], ": ", attr(x, "group.stats")[1], "\t", attr(x, "groups")[1],": ", attr(x, "group.stats")[2],"\n")
  cat(" Observed difference:", round(attr(x, "observed"), 5), "\n\n")
  cat(" Mean of permutation distribution:", round(mean(x), 5), "\n")
  cat(" Standard error of permutation distribution:", round(mean(x), 5), "\n")
  cat(" P-value: ", round(attr(x, "pval"), 5),"\n")
  cat("\n\t*-------------*\n\n")
  
  
}