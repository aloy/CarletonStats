#' @param call the result of `match.call(expand.dots = FALSE)` from the caller
#' @param parent_env the caller's `parent.frame()`
#' @return A list with elements `x`, `y`, `x.name`, `y.name`
#' @noRd
.parse_formula_two_var <- function(call, parent_env) {
  m <- call
  if (is.matrix(eval(m$data, parent_env))) {
    m$data <- as.data.frame(eval(m$data, parent_env))
  }
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent_env)

  if (length(mf) != 2L) {
    stop("Invalid formula")
  }

  nmiss <- length(attr(mf, "na.action"))
  if (nmiss > 0) {
    cat("\n ", nmiss, "observation(s) removed due to missing values.\n")
  }

  varnames <- names(mf)
  response <- attr(attr(mf, "terms"), "response")
  y <- mf[[response]]
  x <- mf[[-response]]

  list(x = x, y = y, x.name = varnames[2], y.name = varnames[1])
}
