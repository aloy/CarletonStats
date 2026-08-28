# Permutation test for the correlation of two variables.

Hypothesis test for a correlation of two variables. The null hypothesis
is that the population correlation is 0.

## Usage

``` r
permTestCor(x, ...)

# Default S3 method
permTestCor(
  x,
  y,
  B = 999,
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
permTestCor(formula, data, subset, ...)
```

## Arguments

- x:

  a numeric vector.

- ...:

  further arguments to be passed to or from methods.

- y:

  a numeric vector.

- B:

  the number of resamples to draw (positive integer greater than 2).

- alternative:

  alternative hypothesis. Options are `"two.sided"`, `"less"` or
  `"greater"`.

- plot.hist:

  a logical value. If `TRUE`, plot the distribution of the correlations
  obtained from each resample.

- plot.qq:

  a logical value. If `TRUE`, plot the normal quantile-quantile plot of
  the correlations obtained from each resample.

- x.name:

  Label for variable x

- y.name:

  Label for variable y

- xlab:

  an optional character string for the x-axis label

- ylab:

  an optional character string for the y-axis label

- title:

  an optional character string giving the plot title

- seed:

  optional argument to [`set.seed`](https://rdrr.io/r/base/Random.html)

- formula:

  a formula `y ~ x` where `x, y` are numeric vectors.

- data:

  a data frame that contains the variables given in the formula.

- subset:

  an optional expression indicating what observations to use.

## Value

Returns invisibly a vector of the correlations obtained by the
randomization.

## Details

Perform a permutation test to test \\latex\\, where \\latex\\is the
population correlation. The rows of the second variable are permuted and
the correlation is re-computed.

The mean and standard error of the permutation distribution is printed
as well as a P-value.

Observations with missing values are removed.

## Methods (by class)

- `permTestCor(default)`: Permutation test for the correlation of two
  variables.

- `permTestCor(formula)`: Permutation test for the correlation of two
  variables.

## References

Tim Hesterberg's website:
<https://www.timhesterberg.net/bootstrap-and-resampling>

## Author

Laura Chihara

## Examples

``` r

plot(states03$HSGrad, states03$TeenBirths)

cor(states03$HSGrad, states03$TeenBirths)
#> [1] -0.6761514

permTestCor(states03$HSGrad, states03$TeenBirths)
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: two.sided 
#>  Observed correlation between states03$HSGrad ,  states03$TeenBirths : -0.6762 
#>  Mean of permutation distribution: -0.00135 
#>  Standard error of permutation distribution: 0.14573 
#>  P-value:   0.001 
#> 
#>  *-------------*
#> 

permTestCor(TeenBirths ~ HSGrad, data = states03)
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: two.sided 
#>  Observed correlation between HSGrad ,  TeenBirths : -0.6762 
#>  Mean of permutation distribution: -0.00553 
#>  Standard error of permutation distribution: 0.1461 
#>  P-value:   0.001 
#> 
#>  *-------------*
#> 

```
