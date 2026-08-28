# Stem and leaf plot

Stem and leaf plot. Will accept a factor variable as a second argument
to create stem plots for each of the levels.

## Usage

``` r
stemPlot(x, ...)

# Default S3 method
stemPlot(x, grpvar = NULL, varname = NULL, grpvarname = NULL, ...)

# S3 method for class 'formula'
stemPlot(formula, data = parent.frame(), subset, ...)
```

## Arguments

- x:

  a numeric variable.

- ...:

  further arguments to be passed to or from methods.

- grpvar:

  a factor variable. A stem plot of `x` will be created for each level
  of the factor variable.

- varname:

  name of the numeric variable. This is for printing the output only.
  Change if you want to print out a name different from the actual
  variable name.

- grpvarname:

  name of the factor variable. This is for printing the output only.
  Change if you want to print out a name different from the actual
  variable name.

- formula:

  a formula of the form `x ~ grpvar` where `x` is numeric and `grpvar`
  is a factor variable.

- data:

  a data frame with the variables in the formula.

- subset:

  an optional expression specifying which observations to keep.

## Details

This command is just an enhanced version of R's `stem` command. It
allows the user to create the stem plot for a numeric variable grouped
by the levels of a factor variable.

## Methods (by class)

- `stemPlot(default)`: Stem and leaf plot

- `stemPlot(formula)`: Stem and leaf plot

## Author

Laura Chihara

## Examples

``` r


stemPlot(states03$Births, states03$Region)
#> 
#> ***Stem and Leaf plot for  states03$Births ***
#>    Grouped by levels of  states03$Region 
#> 
#>     Midwest 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 11344
#>   0 | 7788
#>   1 | 3
#>   1 | 58
#> 
#> 
#>     Northeast 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 111114
#>   0 | 78
#>   1 | 14
#>   1 | 
#>   2 | 
#>   2 | 5
#> 
#> 
#>     South 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 244566678
#>   1 | 024
#>   2 | 1
#>   3 | 7
#> 
#> 
#>     West 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 111223345789
#>   2 | 
#>   4 | 2
#> 

stemPlot(Births ~ Region, data = states03)
#> 
#> ***Stem and Leaf plot for  Births ***
#>    Grouped by levels of  Region 
#> 
#>     Midwest 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 11344
#>   0 | 7788
#>   1 | 3
#>   1 | 58
#> 
#> 
#>     Northeast 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 111114
#>   0 | 78
#>   1 | 14
#>   1 | 
#>   2 | 
#>   2 | 5
#> 
#> 
#>     South 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 244566678
#>   1 | 024
#>   2 | 1
#>   3 | 7
#> 
#> 
#>     West 
#>  :
#>   The decimal point is 5 digit(s) to the right of the |
#> 
#>   0 | 111223345789
#>   2 | 
#>   4 | 2
#> 
```
