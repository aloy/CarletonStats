#' @describeIn bootSlope Bootstrap the slope of a simple linear regression line
#' @export
bootSlope.default <-
  function(x, y, conf.level = 0.95, B = 10000, plot.hist = TRUE, hist.title = NULL,
           plot.qq = FALSE, legend.loc = "topright", x.name=deparse(substitute(x)), y.name=deparse(substitute(y)), ...)
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
    
    cat("\n\t** Bootstrap of slope **\n\n")
    cat(" Observed slope between ", x.name, " and ", y.name, ":", round(observed, 5), "\n")
    cat(" Mean of bootstrap distribution:",  round(mean(temp),5),"\n")
    cat(" Standard error of bootstrap distribution:", round(sd(temp), 5),"\n\n")
    cat(" Bootstrap percentile interval\n")
    print(round(quantile(temp, c(alpha/2, 1-alpha/2)), 5))
    cat("\n\t\t*--------------*\n\n")
    
    if (plot.hist){
      
      if (is.null(hist.title))
        hist.title <- paste("Bootstrap distribution of slope\n", x.name," and ", y.name, sep=" ")
      #hist in R gives counts
      out <-   hist(temp, plot=FALSE)
      out$density <- 100*(out$counts)/sum(out$counts)
      plot(out, freq = FALSE, ylab = "Percent", main = hist.title, xlab = "Slope", cex.main = .9, cex.lab = .9)
      
      points(observed, 0, pch = 2, col = "red")
      points(bootstrap.mean, 0, pch = 4, col = "blue")
      legend(legend.loc, legend = c("Observed","Bootstrap"),  pch = c(2,4),
             col = c("red", "blue"), cex = 0.8)
      
    } #end if plot.hist
    
    if (plot.qq){
      if (Sys.getenv("RSTUDIO_USER_IDENTITY") == "")
        grDevices::dev.new()
      qqnorm(temp, ylab = "Bootstrap")
      qqline(temp)
    }  #end if plot.qq
    
    invisible(temp)
  }
