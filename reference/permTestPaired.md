# Permutation test for paired data.

Permutation test for paired data.

## Usage

``` r
permTestPaired(x, ...)

# Default S3 method
permTestPaired(
  x,
  y,
  B = 9999,
  alternative = "two.sided",
  plot.hist = TRUE,
  plot.qq = FALSE,
  x.name = deparse(substitute(x)),
  y.name = deparse(substitute(y)),
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  seed = NULL,
  ...
)

# S3 method for class 'formula'
permTestPaired(formula, data, subset, ...)
```

## Arguments

- x:

  a numeric vector.

- ...:

  further arguments to be passed to or from methods.

- y:

  a numeric vector.

- B:

  the number of resamples.

- alternative:

  the alternative hypothesis. Options are `"two.sided"`, `"less"` and
  `"greater"`.

- plot.hist:

  a logical value. If `TRUE`, create a histogram displaying the
  permutation distribution of the statistic.

- plot.qq:

  a logical value. If `TRUE`, include a quantile-normal plot of the
  permuation distribution.

- x.name:

  Label for x variable

- y.name:

  Label for y variable

- xlab:

  an optional character string for the x-axis label

- ylab:

  an optional character string for the y-axis label

- title:

  an optional character string giving the plot title

- seed:

  optional argument to [`set.seed`](https://rdrr.io/r/base/Random.html)

- formula:

  a formula of the form `y ~ x`, where `x, y` are both numeric
  variables.

- data:

  an optional data frame containing the variables in the formula. By
  default the variables are taken from environment(formula).

- subset:

  an optional vector specifying a subset of observations to be used.

## Value

Returns invisibly a vector of the replicates of the test statistic (ex.
mean of the difference of the resampled variables).

## Details

For two paired numeric variables with n rows, randomly select k of the n
rows (k also is randm) and switch the entries \\latex\\ and then compute
the mean of the difference of the two variables (`y-x`).

Observations with missing values are removed.

## Methods (by class)

- `permTestPaired(default)`: Permutation test for paired data.

- `permTestPaired(formula)`: Permutation test for paired data.

## References

Tim Hesterberg's website:
<https://www.timhesterberg.net/bootstrap-and-resampling>

## Author

Laura Chihara

## Examples

``` r

#Does chocolate ice cream have more calories than vanilla ice cream, on average?
#H0: mean number of calories is the same
#HA: mean number of calories is greater in chocolate ice cream

permTestPaired(Icecream$VanillaCalories, Icecream$ChocCalories, alternative = "less")
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: less 
#>  Observed mean
#>   Icecream$VanillaCalories :  191.4103    Icecream$ChocCalories :  198.7436 
#>  Observed difference: 7.33333 
#> 
#>  Mean of permutation distribution: -0.02095 
#>  Standard error of permutation distribution: 2.3687 
#>  P-value:  0.9994 
#> 
#>  *-------------*
#> 

permTestPaired(ChocCalories ~ VanillaCalories, data = Icecream, alternative = "greater")
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: greater 
#>  Observed mean
#>   VanillaCalories :  191.4103     ChocCalories :  198.7436 
#>  Observed difference: 7.33333 
#> 
#>  Mean of permutation distribution: 0.00606 
#>  Standard error of permutation distribution: 2.34752 
#>  P-value:  0.0003 
#> 
#>  *-------------*
#> 

```
