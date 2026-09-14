# Print a monthly payslip summary

Print a monthly payslip summary

## Usage

``` r
# S3 method for class 'nigeria_net'
print(x, ...)
```

## Arguments

- x:

  An object of class `nigeria_net` from
  [`calc_net_salary`](https://laws2020.github.io/nrsR/reference/calc_net_salary.md).

- ...:

  Unused; for S3 compatibility.

## Value

Invisibly returns `x`. Called for its side effect of printing.

## Examples

``` r
print(calc_net_salary(1624734, annual_rent = 1800000))
#> 
#> ====================================================
#>  MONTHLY PAYSLIP SUMMARY
#> ====================================================
#>  Gross Income:                  ₦1,624,734.00
#> ----------------------------------------------------
#>  DEDUCTIONS
#>    Pension:                     ₦129,978.72
#>    NHF:                         ₦40,618.35
#>    PAYE Tax:                    ₦251,568.76
#> ----------------------------------------------------
#>  Total Deductions:              ₦422,165.83
#> ====================================================
#>  NET TAKE-HOME PAY:             ₦1,202,568.17
#> ====================================================
#> 
```
