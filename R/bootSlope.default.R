#' @describeIn bootSlope Bootstrap the slope of a simple linear regression line
#' @export
bootSlope.default <-
  function(x, y, conf.level = 0.95, B = 10000, plot.hist = TRUE, 
           xlab = NULL, ylab = NULL, title = NULL,
           plot.qq = FALSE, x.name=deparse(substitute(x)), y.name=deparse(substitute(y)), ...)
  {
    
    if (B%%1  != 0 || B < 2) stop("B must be a positive integer")
    
    alpha <- 1 - conf.level
    
    
    if (!is.numeric(x) || !is.numeric(y)) stop("Both variables must be numeric.")
    
    comCases <- complete.cases(x, y)
    nmiss <- length(x) - sum(comCases)
    if (nmiss > 0)
      cat("\n ", nmiss, "observation(s) removed due to missing values.\n")
    
    x2 <- x[comCases]
    y2 <- y[comCases]
    
    observed <- round(cor(x2, y2) * sd(y2) / sd(x2), 4)
    n <- length(x2)
    #Boot
    temp <- numeric(B)
    for (i in 1:B)
    {
      index <- sample(1:n, n, replace = TRUE)
      newx <- x2[index]
      newy <- y2[index]
      temp[i] <- cor(newx, newy) * sd(newy) / sd(newx)
    } #end for
    bootstrap.mean <- mean(temp)
  
    
    # invisible(temp)
    class(temp) <- "carlboot"
    attr(temp, "observed")  <- observed
    attr(temp, "statistic") <- "slope"
    attr(temp, "groups")    <- NULL
    attr(temp, "x.name")    <- x.name
    attr(temp, "y.name")    <- y.name
    attr(temp, "level")     <- conf.level
    
    if (plot.hist) {
      phist <- plot(temp, xlab = xlab, ylab = ylab, title = title)
    }
    
    if(plot.qq) {
      pqq <- ggplot(data = NULL, aes(sample = as.numeric(temp))) +
        geom_qq() + 
        geom_qq_line() +
        theme_classic() +
        labs(x = "N(0, 1) Quantiles", y = "Bootstrap Statistics")
    }
    
    which_plot <- c(phist = plot.hist, pqq = plot.qq)
    
    if(sum(which_plot) == 1) {
      print(get(names(which(which_plot))))
    }
    
    if(sum(which_plot) == 2) {
      print(patchwork::wrap_plots(phist, pqq, ncol = 2))
    }
    
    temp
  }
