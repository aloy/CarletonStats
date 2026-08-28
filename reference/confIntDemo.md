# Confidence Interval Demonstration

Draw many random samples and compute confidence interval. How many
intervals capture the true mean?

## Usage

``` r
confIntDemo(distr = "normal", size = 20, conf.level = 0.95)
```

## Arguments

- distr:

  distribution of the population to be sampled. Options include
  `"normal"`, `"exponential"`, `"uniform"` and `"binary"` (partial match
  allowed).

- size:

  sample size

- conf.level:

  confidence level.

## Value

The command invisibly returns the fraction of intervals that capture the
true mean.

## Details

This simulation will draw 100 random samples from a given population
distribution and compute the correpsonding confidence intervals. The 100
intervals will be drawn with an indication of the ones that missed the
true mean. A histogram of the population will also be created.

## Author

Laura Chihara

## Examples

``` r

confIntDemo()

confIntDemo(distr = "exponential", size = 40)

```
