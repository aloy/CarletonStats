# Missing observations in factors

In data frames with factor variables, convert any observation with ""
into \<NA\>.

## Usage

``` r
missingLevel(data)
```

## Arguments

- data:

  a data frame with factor variables.

## Value

Returns the same data frame with `""""` replaced by `<NA>` in factor
variables.

## Details

In a factor variable with the level `""""`, this command will convert
this to an `<NA>`.

## Note

When importing data from comma separated files (for example), missing
values in a categorical variable are often denoted by """. We often do
not want to treat this as a level of a factor variable in R.

## Author

Laura Chihara
