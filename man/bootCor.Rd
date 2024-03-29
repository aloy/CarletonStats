% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootCor.R, R/bootCor.default.R,
%   R/bootCor.formula.R
\name{bootCor}
\alias{bootCor}
\alias{bootCor.default}
\alias{bootCor.formula}
\title{Bootstrap the correlation}
\usage{
bootCor(x, ...)

\method{bootCor}{default}(
  x,
  y,
  conf.level = 0.95,
  B = 10000,
  plot.hist = TRUE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  plot.qq = FALSE,
  x.name = deparse(substitute(x)),
  y.name = deparse(substitute(y)),
  seed = NULL,
  ...
)

\method{bootCor}{formula}(formula, data, subset, ...)
}
\arguments{
\item{x}{a numeric vector.}

\item{\dots}{further arguments to be passed to or from methods.}

\item{y}{a numeric vector.}

\item{conf.level}{confidence level for the bootstrap ercentile interval.}

\item{B}{number of times to resample (positive integer greater than 2).}

\item{plot.hist}{a logical value. If \code{TRUE}, plot the bootstrap
distribution of the resampled correlation.}

\item{xlab}{an optional character string for the x-axis label}

\item{ylab}{an optional character string for the y-axis label}

\item{title}{an optional character string giving the plot title}

\item{plot.qq}{a logical value. If \code{TRUE} a normal quantile-quantile
plot of the bootstraped values is created.}

\item{x.name}{Label for variable x}

\item{y.name}{Label for variable y}

\item{seed}{optional argument to \code{\link{set.seed}}}

\item{formula}{a formula of the form lhs ~ rhs where lhs is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.}

\item{data}{an optional data frame containing the variables in the formula formula.
By default the variables are taken from environment(formula).}

\item{subset}{an optional vector specifying a subset of observations to be used.}
}
\value{
The command returns the correlations of the resampled
observations.
}
\description{
Bootstrap the correlation of two numeric variables.
}
\details{
Bootstrap the correlation of two numeric variables. The bootstrap mean and
standard error are printed as well as a bootstrap percentile confidence
interval.

Observations with missing values are removed.
}
\section{Methods (by class)}{
\itemize{
\item \code{bootCor(default)}: Bootstrap the correlation of two numeric variables.

\item \code{bootCor(formula)}: Bootstrap the correlation of two numeric variables.

}}
\examples{


plot(states03$ColGrad, states03$InfMortality)
bootCor(InfMortality ~ ColGrad, data = states03, B = 1000)
bootCor(states03$ColGrad, states03$InfMortality, B = 1000)

}
\references{
Tim Hesterberg's website
\url{https://www.timhesterberg.net/bootstrap-and-resampling}
}
\author{
Laura Chihara
}
\keyword{randomiziation}
\keyword{resampling}
