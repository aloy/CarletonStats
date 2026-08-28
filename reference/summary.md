# Print a summary of an `carlboot` object

Print summary statistics and confidence intervals, if desired, for an
`lmeresamp` object.

## Usage

``` r
# S3 method for class 'carlboot'
summary(object, ...)

# S3 method for class 'carlperm'
summary(object, ...)
```

## Arguments

- object:

  The carlboot object to print.

- ...:

  not used

## Examples

``` r
boot_dist <- boot(ToothGrowth$len, ToothGrowth$supp, B = 1000)
summary(boot_dist)
#> Replications: 1000 
#> 
#> Summary Statistics of Bootstrap Distribution: 
#>   Observed    Mean       SE
#> 1      3.7 3.64019 1.932906
perm_dist <- permTest(states03$ViolentCrime, states03$DeathPenalty, B = 999)
summary(perm_dist)
#> Replications: 999 
#> 
#> Summary Statistics of Permutation Distribution: 
#> 
#>    Observed     Mean Alternative P.value
#> 1 -167.1057 1.061062   two.sided   0.009
```
