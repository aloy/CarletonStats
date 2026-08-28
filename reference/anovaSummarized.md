# Anova F test

ANOVA F test when given summarized data (sample sizes, means and
standard deviations).

## Usage

``` r
anovaSummarized(N, mn, stdev)
```

## Arguments

- N:

  a vector with the sample sizes

- mn:

  a vector of means, one for each group in the sample

- stdev:

  a vector of standard deviations, one for each group in the sample

## Value

Returns invisibly a list

- Treatment SS:

  The treatment sum of squares (also called the "between sum of
  squares").

- Residual SS:

  Residual sum of squares (also called the "within sum of squares").

- Degrees of Freedom:

  a vector with the numerator and denominator degrees of freedom.

- Treatment Mean Square:

  Treatment SS/numerator DF

- Residual Mean Square:

  Residual SS/denominator DF

- Residual Standard Error:

  Square root of Residual Mean Square

- F:

  the F statistic

- P-value:

  p-value

...

## Details

Perform an ANOVA F test when presented with summarized data: sample
sizes, sample means and sample standard devations.

## Author

Laura Chihara

## Examples

``` r

#use the data set chickwts from base R
head(chickwts)
#>   weight      feed
#> 1    179 horsebean
#> 2    160 horsebean
#> 3    136 horsebean
#> 4    227 horsebean
#> 5    217 horsebean
#> 6    168 horsebean

N <- table(chickwts$feed)
stdev <- tapply(chickwts$weight, chickwts$feed, sd)
mn <- tapply(chickwts$weight, chickwts$feed, mean)

anovaSummarized(N, mn, stdev)
#>                                     
#> Treatment SS            231129.16210
#> Residual SS             195556.02100
#> numerator DF                 5.00000
#> denominator DF              65.00000
#> Residual standard error     54.85029
#> 
#>      F-stat     P-value 
#> 1.53648e+01 5.93642e-10 
```
