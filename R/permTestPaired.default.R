#' @describeIn permTestPaired Permutation test for paired data.
#' @export

permTestPaired.default <-
function(x, y,  B = 9999,
     alternative ="two.sided",  plot.hist = TRUE, plot.qq = FALSE,
     x.name=deparse(substitute(x)), y.name=deparse(substitute(y)), 
     xlab = NULL, ylab = NULL, title = NULL, 
     seed = NULL,...)
    {
     if (!is.numeric(x) || !is.numeric(y)) stop("Both variables must be numeric.")

     comCases <- complete.cases(x, y)
     nmiss <- length(x) - sum(comCases)
      if (nmiss > 0)
        cat("\n ", nmiss, "observation(s) removed due to missing values.\n")

     x2 <- x[comCases]
     y2 <- y[comCases]
     n <- length(x2)
     stat1 <- mean(x2)
     stat2 <- mean(y2)
     Diff <- x2 - y2
     observed <- mean(Diff)

    result <- numeric(B)

     for (i in 1:B)
      {
       Diff2 <- Diff
       Sign <- sample(c(-1, 1), n, replace = TRUE)
       Diff2 <- - Diff*Sign
       result[i] <- mean(Diff2)
       }  #end for

  mean.PermDist <- round(mean(result), 5)
  sd.PermDist <- round(sd(result), 5)

  alt <- pmatch(alternative, c("less", "greater", "two.sided"), nomatch=4)

  P.value <- .calc_pvalue(result, observed, alt)
   if (P.value > 1) P.value <- 1

#   if (plot.hist){
#   my.title <- paste("Permutation distribution for mean of paired difference:" ,"\n", y.name, "-", x.name, sep = " ")
#   out <- hist(result, plot = F)
#   out$density <- 100*out$counts/sum(out$counts)
#   xrange <- range(c(out$breaks, observed))
#   plot(out, freq = FALSE, ,xlim = xrange, main = my.title, ylab = "Percent", cex.main = .9,
#      xlab = "Paired difference", cex.lab = .9)
# 
#    points(observed, 0, pch = 2, col = "red")
#    points(mean.PermDist, 0, pch = 8, col = "blue")
#   legend(legend.loc, legend = c("Observed", "Permutation"), pch = c(2, 8), col = c("red", "blue"), cex = .9)
# }

  # cat("\n\t** Permutation test for mean of paired difference **\n")
  # cat("\n Permutation test with alternative:", alternative,"\n")
  # cat(" Observed mean\n")
  # cat(" ", y.name, ": ",stat2, "\t", x.name,": ", stat1,"\n")
  # cat(" Observed difference ", y.name, "-", x.name, ":",  round(observed, 5), "\n\n")
  # cat(" Mean of permutation distribution:", mean.PermDist, "\n")
  # cat(" Standard error of permutation distribution:", sd.PermDist, "\n")
  # cat(" P-value: ", round(P.value, 5),"\n\n")
  #  cat("\t*-------------*\n\n")

   class(result) <- "carlperm"
   attr(result, "observed")  <- observed
   attr(result, "statistic") <- "paired difference"
   attr(result, "alternative") <- alternative
   attr(result, "x.name")    <- x.name
   attr(result, "y.name")    <- y.name
   attr(result, "var.stats") <- c(stat1, stat2)
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
