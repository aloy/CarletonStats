#' @describeIn bootPaired Perform a bootstrap of two paired variables.
#' @export

bootPaired.default <-
  function(x, y, conf.level = 0.95, B = 10000,
           plot.hist = TRUE,  xlab = NULL, ylab = NULL, title = NULL, plot.qq = FALSE, 
           x.name = deparse(substitute(x)), y.name=deparse(substitute(y)), ...)
  {
    
    if (B%%1  != 0 || B < 2) stop("B must be a positive integer")
    
    alpha <- 1 - conf.level
    
    if (!is.numeric(x)) stop("Variable 1 must be numeric.")
    if (!is.numeric(y)) stop("Variable 2 must be numeric.")
    
    comCases <- complete.cases(x, y)
    nmiss <- length(x) - sum(comCases)
    if (nmiss > 0)
      cat("\n ", nmiss, "observation(s) removed due to missing values.\n")
    
    x2 <- x[comCases]
    y2 <- y[comCases]
    
    Diff <- y2 - x2
    n <- length(Diff)
    observed <- mean(Diff)
    
    #Boot
    temp <- numeric(B)
    
    for (i in 1:B)
    {
      bootDiff <- sample(Diff, n, replace = TRUE)
      temp[i] <- mean(bootDiff)
    } #end for
    
    bootstrap.mean <- mean(temp)
    
    class(temp) <- "carlboot"
    attr(temp, "observed")  <- observed
    attr(temp, "statistic") <- as.character(substitute(statistic))
    attr(temp, "groups")    <- "paired"
    attr(temp, "x.name")    <- x.name
    attr(temp, "y.name")    <- y.name
    attr(temp, "level")     <- conf.level
    
    
    # cat("\n\t**Bootstrap interval for mean of paired difference \n\n")
    # cat(" Observed mean of ", y.name, "-", x.name, ":", round(observed, 5), "\n")
    # cat(" Mean of bootstrap distribution:",  round(mean(temp),5),"\n")
    # cat(" Standard error of bootstrap distribution:", round(sd(temp), 5),"\n\n")
    # cat(" Bootstrap percentile interval\n")
    # print(quantile(temp, c(alpha/2, 1-alpha/2)))
    # cat("\n\t\t*--------------*\n\n")
    
    if (plot.hist) {
      if(is.null(title)) {
        title <- "Bootstrap distribution"
      }
      
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
      print(wrap_plots(phist, pqq, ncol = 2))
    }
    
    temp
    
    
  }
