#' @title Plot the bootstrap distribution in \code{carlboot} object
#'
#' @description
#' Plot the bootstrap distribution returned as a \code{carlboot} object.
#'
#' @param x The carlboot object to print.
#' @param bins number of bins in histogram.
#' @param size size of points.
#' @param ... not used
#'
#' @rdname plot
#' @export 
#' @method plot carlboot
#' @importFrom scales label_percent
#' @importFrom stats density
plot.carlboot <- function(x, bins = 15, size = 5,...) {
  boot_stats <- as.numeric(x)
  width <- NULL
  ggplot2::ggplot(data = NULL, ggplot2::aes(x = boot_stats)) +
    ggplot2::geom_histogram(aes(y = after_stat(width*density)), bins = bins) +
    ggplot2::geom_point(aes(x = mean(x), y = 0, color = "Bootstrap", shape = "Bootstrap"), size = size) +
    ggplot2::geom_point(aes(x = attr(x, "observed"), color = "Observed", shape = "Observed", y = 0), size = size) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0.05, .05))) +
    ggplot2::labs(
      title = "Bootstrap distribution",
      x = "statistics",
      y = "Percentage",
      shape = "",
      color = ""
    ) +
    ggplot2::scale_shape_manual(values = c(16, 4)) + 
    ggplot2::scale_color_manual(values = c("darkorange", "skyblue")) + 
    ggplot2::theme_classic()
}
