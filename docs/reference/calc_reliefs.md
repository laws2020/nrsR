# Calculate statutory tax reliefs for an employee

Computes all deductions and reliefs that reduce an employee's taxable
income. All rates and caps are read directly from the law registry no
hardcoded values. When a new tax amendment is registered, this function
automatically picks up the new parameters.

## Usage

``` r
calc_reliefs(
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

  Numeric. Annual rent paid in Naira. Only relevant for laws with rent
  relief (e.g. NTA2025). Default `0`.

- include_nhf:

  Logical. Include NHF deduction? Default `TRUE`.

- include_nhis:

  Logical. Include NHIS deduction? Default `FALSE`.

- law:

  Character. Tax law key. Use
  [`list_tax_laws()`](https://laws2020.github.io/nrsR/reference/list_tax_laws.md)
  to see options. Default `"NTA2025"`.

- basic_monthly:

  Numeric or `NULL`. Monthly basic salary for NHF calculation. If
  `NULL`, gross is used.

## Value

A named list of class `nigeria_reliefs`.

## Examples

``` r
calc_reliefs(693228.96)
#> $gross_monthly
#> [1] 693229
#> 
#> $gross_annual
#> [1] 8318748
#> 
#> $pension_monthly
#> [1] 55458.32
#> 
#> $pension_annual
#> [1] 665499.8
#> 
#> $nhf_monthly
#> [1] 17330.72
#> 
#> $nhf_annual
#> [1] 207968.7
#> 
#> $nhis_monthly
#> [1] 0
#> 
#> $nhis_annual
#> [1] 0
#> 
#> $cra_annual
#> [1] 0
#> 
#> $annual_rent
#> [1] 0
#> 
#> $rent_relief_annual
#> [1] 0
#> 
#> $total_relief_monthly
#> [1] 72789.04
#> 
#> $total_relief_annual
#> [1] 873468.5
#> 
#> $taxable_income_annual
#> [1] 7445279
#> 
#> $law
#> [1] "NTA2025"
#> 
#> $law_description
#> [1] "Nigeria Tax Act 2025 (effective January 2026)"
#> 
#> attr(,"class")
#> [1] "nigeria_reliefs" "list"           
calc_reliefs(693228.96, annual_rent = 1200000, law = "NTA2025")
#> $gross_monthly
#> [1] 693229
#> 
#> $gross_annual
#> [1] 8318748
#> 
#> $pension_monthly
#> [1] 55458.32
#> 
#> $pension_annual
#> [1] 665499.8
#> 
#> $nhf_monthly
#> [1] 17330.72
#> 
#> $nhf_annual
#> [1] 207968.7
#> 
#> $nhis_monthly
#> [1] 0
#> 
#> $nhis_annual
#> [1] 0
#> 
#> $cra_annual
#> [1] 0
#> 
#> $annual_rent
#> [1] 1200000
#> 
#> $rent_relief_annual
#> [1] 240000
#> 
#> $total_relief_monthly
#> [1] 92789.04
#> 
#> $total_relief_annual
#> [1] 1113468
#> 
#> $taxable_income_annual
#> [1] 7205279
#> 
#> $law
#> [1] "NTA2025"
#> 
#> $law_description
#> [1] "Nigeria Tax Act 2025 (effective January 2026)"
#> 
#> attr(,"class")
#> [1] "nigeria_reliefs" "list"           
calc_reliefs(693228.96, law = "PITA")
#> $gross_monthly
#> [1] 693229
#> 
#> $gross_annual
#> [1] 8318748
#> 
#> $pension_monthly
#> [1] 55458.32
#> 
#> $pension_annual
#> [1] 665499.8
#> 
#> $nhf_monthly
#> [1] 17330.72
#> 
#> $nhf_annual
#> [1] 207968.7
#> 
#> $nhis_monthly
#> [1] 0
#> 
#> $nhis_annual
#> [1] 0
#> 
#> $cra_annual
#> [1] 1863750
#> 
#> $annual_rent
#> [1] 0
#> 
#> $rent_relief_annual
#> [1] 0
#> 
#> $total_relief_monthly
#> [1] 228101.5
#> 
#> $total_relief_annual
#> [1] 2737218
#> 
#> $taxable_income_annual
#> [1] 5581530
#> 
#> $law
#> [1] "PITA"
#> 
#> $law_description
#> [1] "Personal Income Tax Act (legacy)"
#> 
#> attr(,"class")
#> [1] "nigeria_reliefs" "list"           
```
