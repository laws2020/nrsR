# Calculate monthly net (take-home) salary

Calculate monthly net (take-home) salary

## Usage

``` r
calc_net_salary(
  gross_monthly,
  annual_rent = 0,
  include_nhf = TRUE,
  include_nhis = FALSE,
  law = "NTA2025",
  basic_monthly = NULL
)
```

## Arguments

- gross_monthly:

  Numeric. Monthly gross income in Naira.

- annual_rent:

  Numeric. Annual rent paid. Default `0`.

- include_nhf:

  Logical. Include NHF? Default `TRUE`.

- include_nhis:

  Logical. Include NHIS? Default `FALSE`.

- law:

  Character. Tax law key from the registry. Default `"NTA2025"`.

- basic_monthly:

  Numeric or `NULL`. Basic salary for NHF base.

## Value

A named list of class `nigeria_net`.

## Examples

``` r
calc_net_salary(1624734, annual_rent = 1800000)
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
