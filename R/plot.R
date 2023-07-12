#' @title Plot the bootstrap distribution in \code{carlboot} object
#'
#' @description
#' Plot the bootstrap distribution returned as a \code{carlboot} object.
#'
#' @param x The carlboot object to print.
#' @param ... not used
#'
#' @rdname plot
#' @export 
#' @method plot carlboot
plot.carlboot <- function(x, bins = 30, ...) {
  boot_stats <- as.numeric(x)
  ggplot2::ggplot(data = NULL, ggplot2::aes(x = boot_stats)) +
    ggplot2::geom_histogram(aes(y = after_stat(width*density)), bins = bins) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, .05))) +
    ggplot2::labs(
      title = "Bootstrap distribution",
      x = "statistics",
      y = "Percentage"
    ) +
    ggplot2::theme_classic()
}
