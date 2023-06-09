% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\docType{data}
\name{Icecream}
\alias{Icecream}
\title{Ice cream data}
\format{
A data frame with 39 observations on the following 7 variables.
\describe{
\item{Brand}{Brand name}
\item{VanillaCalories}{Calories per serving in vanilla}
\item{VanillaFat}{Fat per serving (g) in vanilla}
\item{VanillaSugar}{Sugar per serving (g) in vanilla}
\item{ChocCalories}{Calories per serving in chocolate}
\item{ChocFat}{Fat per serving (g) in chocolate}
\item{ChocSugar}{Sugar per serving (g) in chocolate}
}
}
\source{
Data collected by Carleton student Ann Butkowski (2008).
}
\description{
Nutritional information on vanilla and chocolate ice cream from a sample of
companies.
}
\examples{

head(Icecream)
t.test(Icecream$VanillaCalories, Icecream$ChocCalories, paired = TRUE)

}
\keyword{datasets}
