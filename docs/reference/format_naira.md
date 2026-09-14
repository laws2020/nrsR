# Format a number as Nigerian Naira

Format a number as Nigerian Naira

## Usage

``` r
format_naira(x, prefix = "₦", digits = 2)
```

## Arguments

- x:

  Numeric value.

- prefix:

  Character. Currency prefix. Default `"\u20a6"`.

- digits:

  Integer. Decimal places. Default `2`.

## Value

A formatted character string.

## Examples

``` r
format_naira(1624734)
#> [1] "₦1,624,734.00"
```
