#' @describeIn permTestCor Permutation test for the correlation of two variables.
#' @export

permTestCor.default <-
function(x, y,  B = 999,
     alternative = "two.sided",  plot.hist = TRUE, plot.qq = FALSE,
        x.name = deparse(substitute(x)), y.name = deparse(substitute(y)), 
     xlab = NULL, ylab = NULL, title = NULL, 
     seed = NULL, ...) {


   if (!is.numeric(x)) stop("Variable 1 must be numeric")
   if (!is.numeric(y)) stop("Variable 2 must be numeric")

    comCases <- complete.cases(x, y)
    nmiss <- length(x) - sum(comCases)
    if (nmiss > 0)
     cat("\n ", nmiss, "observation(s) removed due to missing values.\n")

    x2 <- x[comCases]
    y2 <- y[comCases]

    n <-length(x2)
    observed <- round(cor(x2, y2), 4)

    result <- numeric(B)
    for (i in 1:B)
    {
       index <- sample(n, n, replace=FALSE)
       newy <- y2[index]
       result[i] <- cor(x2, newy)
     }  #end for

  mean.PermDist <- round(mean(result), 4)
  sd.PermDist <- round(mean(result), 4)

  alt <- pmatch(alternative, c("less", "greater", "two.sided"), nomatch=4)

  P.value <- .calc_pvalue(result, observed, alt)
  if (P.value > 1) P.value <- 1

  # my.title <- paste("Permutation distribution of correlation: " ,x.name, ", ", y.name, sep= " ")
  # out <- hist(result, plot = F)
  # out$density <- 100*out$counts/sum(out$counts)
  # xrange <- range(c(out$breaks, observed))
  # plot(out, freq = FALSE, ,xlim = xrange, main = my.title, ylab = "Percent", cex.main = .9,
  # xlab="Correlation", cex.lab=.9)
  # 
  #  points(observed, 0, pch = 2, col = "red")
  #  points(mean.PermDist, 0, pch = 8, col = "blue")
  # legend(legend.loc, legend = c("Observed", "Permutation"), pch = c(2, 8), col= c("red", "blue"),
  #     cex = .9)


  # cat("\n\t** Permutation test: Correlation **\n")
  # cat("\n Permutation test with alternative:", alternative,"\n")
  # cat(" Observed correlation between", x.name, ", ", y.name, ": ", observed, "\n")
  # cat(" Mean of permutation distribution:", mean.PermDist, "\n")
  # cat(" Standard error of permutation distribution:", sd.PermDist, "\n")
  # cat(" P-value: ", round(P.value, 5),"\n")
  # cat("\n\t*-------------*\n\n")
  
  class(result) <- "carlperm"
  attr(result, "observed")  <- observed
  attr(result, "statistic") <- "correlation"
  attr(result, "alternative") <- alternative
  attr(result, "x.name")    <- x.name
  attr(result, "y.name")    <- y.name
  # attr(result, "groups")    <- levels(group)
  # attr(result, "group.stats") <- c(stat(group1), stat(group2))
  attr(result, "pval")      <- P.value
  attr(result, "xlab")      <- xlab
  attr(result, "ylab")      <- ylab
  attr(result, "title")     <- title
  attr(result, "plot.hist") <- plot.hist
  attr(result, "plot.qq")   <- plot.qq
  
  result
  
}
