# Detailed tax band breakdown

Returns a tidy data frame showing how each band contributes to the total
PAYE. Works with any law in the registry.

## Usage

``` r
tax_breakdown(
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

A data frame with per-band detail and cumulative tax.

## Examples

``` r
tax_breakdown(1624734)
#>              band rate_pct income_in_band     tax cumulative_tax
#> 1 First ₦800K @0%        0         800000       0              0
#> 2 Next ₦2.2M @15%       15        2200000  330000         330000
#> 3   Next ₦9M @18%       18        9000000 1620000        1950000
#> 4  Next ₦13M @21%       21        5449643 1144425        3094425
#> 5  Next ₦25M @23%       23              0       0        3094425
#> 6 Above ₦50M @25%       25              0       0        3094425
tax_breakdown(1624734, law = "PITA")
#>               band rate_pct income_in_band     tax cumulative_tax
#> 1  First ₦300K @7%        7         300000   21000          21000
#> 2  Next ₦300K @11%       11         300000   33000          54000
#> 3  Next ₦500K @15%       15         500000   75000         129000
#> 4  Next ₦500K @19%       19         500000   95000         224000
#> 5  Next ₦1.6M @21%       21        1600000  336000         560000
#> 6 Above ₦3.2M @24%       24       10150282 2436068        2996068
```
