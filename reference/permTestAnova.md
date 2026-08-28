# Permutation test for ANOVA F-test

Permutation test to see if the population mean is the same for two or
more populations. For instance, test \\latex\\ where \\latex\\ denotes
the population mean. The values of the numeric variable are randomly
assigned to the groups and the ANOVA F statistic is calculated. The
command will print the mean and standard error of the distribution of
the test statistic as well as a P-value.

## Usage

``` r
permTestAnova(x, ...)

# Default S3 method
permTestAnova(
  x,
  group,
  B = 9999,
  plot.hist = TRUE,
  plot.qq = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  seed = NULL,
  ...
)

# S3 method for class 'formula'
permTestAnova(formula, data = parent.frame(), subset, ...)
```

## Arguments

- x:

  a numeric vector.

- ...:

  further arguments to be passed to or from methods.

- group:

  a factor variable with two or more levels. If `group` is a numeric
  vector, it will be coerced into a factor variable.

- B:

  the number of resamples (positive integer greater than 2).

- plot.hist:

  a logical value. If `TRUE`, the permutation distribution of the
  statistic is plotted.

- plot.qq:

  a logical value. If `TRUE`, then a normal quantile-quantile plot of
  the resampled test statistic is created.

- xlab:

  an optional character string for the x-axis label

- ylab:

  an optional character string for the y-axis label

- title:

  an optional character string giving the plot title

- seed:

  optional argument to [`set.seed`](https://rdrr.io/r/base/Random.html)

- formula:

  a formula of the form `y ~ group` where `y` is numeric and `group` is
  a factor variable.

- data:

  a data frame with the variables in the formula.

- subset:

  an optional expression specifying which observations to keep.

## Value

Returns invisibly a vector of the replicates of the test statistic.

## Details

Observations with missing values are removed.

## Methods (by class)

- `permTestAnova(default)`: Permutation test for ANOVA F-test

- `permTestAnova(formula)`: Permutation test for ANOVA F-test

## References

Tim Hesteberg's website:
<https://www.timhesterberg.net/bootstrap-and-resampling>

## Author

Adam Loy, Laura Chihara

## Examples

``` r

permTestAnova(states03$ViolentCrime, states03$Region, B = 499)
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: greater 
#>  Observed F statistic: 1.82174 
#>  Mean of permutation distribution: 1.08794 
#>  Standard error of permutation distribution: 0.98715 
#>  P-value:   0.154 
#> 
#>  *-------------*
#> 


#using formula syntax
if (FALSE) { # \dontrun{
permTestAnova(ViolentCrime ~ Region, data = states03, B = 9999)
} # }
```
