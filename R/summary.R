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
  cat("Replications:", length(object), "\n\n")
  
  cat("Summary Statistics of Bootstrap Distribution:", "\n")
  
  print(
    data.frame(
      Observed = attr(object, "observed"),
      Mean = mean(object),
      SE = sd(object)
    )
  )
}

