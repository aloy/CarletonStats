# Permutation test for the Slope

Hypothesis test for a slope of a simple linear regression model. The
null hypothesis is that the population slope is 0.

## Usage

``` r
permTestSlope(x, ...)

# Default S3 method
permTestSlope(
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
permTestSlope(formula, data, subset, ...)
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

  a logical value. If `TRUE`, plot the distribution of the slopes
  obtained from each resample.

- plot.qq:

  a logical value. If `TRUE`, plot the normal quantile-quantile plot of
  the slopes obtained from each resample.

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

Returns invisibly a vector of the slopes obtained by the randomization.

## Details

Perform a permutation test to test \\latex\\, where \\latex\\is the
population slope. The rows of the second variable are permuted and the
slope is re-computed.

The mean and standard error of the permutation distribution is printed
as well as a P-value.

Observations with missing values are removed.

## Methods (by class)

- `permTestSlope(default)`: Permutation test for the slope

- `permTestSlope(formula)`: Permutation test for the slope

## References

Tim Hesterberg's website:
<https://www.timhesterberg.net/bootstrap-and-resampling>

## Author

Adam Loy, Laura Chihara

## Examples

``` r

plot(states03$HSGrad, states03$TeenBirths)

lm(HSGrad ~ TeenBirths, data = states03)
#> 
#> Call:
#> lm(formula = HSGrad ~ TeenBirths, data = states03)
#> 
#> Coefficients:
#> (Intercept)   TeenBirths  
#>     94.9878      -0.2152  
#> 

permTestSlope(states03$HSGrad, states03$TeenBirths)
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: two.sided 
#>  Observed slope between states03$HSGrad ,  states03$TeenBirths :  -2.1243 
#>  Mean of permutation distribution: 0.02045 
#>  Standard error of permutation distribution: 0.44499 
#>  P-value:   0.001 
#> 
#>  *-------------*
#> 

permTestSlope(TeenBirths ~ HSGrad, data = states03)
#> 
#>  ** Permutation test **
#> 
#>  Permutation test with alternative: two.sided 
#>  Observed slope between HSGrad ,  TeenBirths :  -2.1243 
#>  Mean of permutation distribution: -0.01319 
#>  Standard error of permutation distribution: 0.44299 
#>  P-value:   0.001 
#> 
#>  *-------------*
#> 

```
