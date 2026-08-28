# Ice cream data

Nutritional information on vanilla and chocolate ice cream from a sample
of companies.

## Format

A data frame with 39 observations on the following 7 variables.

- Brand:

  Brand name

- VanillaCalories:

  Calories per serving in vanilla

- VanillaFat:

  Fat per serving (g) in vanilla

- VanillaSugar:

  Sugar per serving (g) in vanilla

- ChocCalories:

  Calories per serving in chocolate

- ChocFat:

  Fat per serving (g) in chocolate

- ChocSugar:

  Sugar per serving (g) in chocolate

## Source

Data collected by Carleton student Ann Butkowski (2008).

## Examples

``` r

head(Icecream)
#>            Brand VanillaCalories VanillaFat VanillaSugar ChocCalories ChocFat
#> 1 Baskin Robbins             260       16.0         26.0          260      14
#> 2  Ben & Jerry's             240       16.0         19.0          260      16
#> 3     Blue Bunny             140        7.0         12.0          130       7
#> 4        Breyers             140        7.0         13.0          140       8
#> 5      Brigham's             190       12.0         17.0          200      12
#> 6          Bulla             234       13.5         21.8          266      15
#>   ChocSugar
#> 1      31.0
#> 2      22.0
#> 3      14.0
#> 4      16.0
#> 5      18.0
#> 6      22.6
t.test(Icecream$VanillaCalories, Icecream$ChocCalories, paired = TRUE)
#> 
#>  Paired t-test
#> 
#> data:  Icecream$VanillaCalories and Icecream$ChocCalories
#> t = -3.5414, df = 38, p-value = 0.001071
#> alternative hypothesis: true mean difference is not equal to 0
#> 95 percent confidence interval:
#>  -11.525296  -3.141371
#> sample estimates:
#> mean difference 
#>       -7.333333 
#> 
```
