# Calculate PAYE for a batch of employees

Processes a data frame of employees and returns a tidy payroll summary.
Supports any law in the registry – just pass its key.

## Usage

``` r
calc_paye_batch(df, law = "NTA2025", include_nhf = TRUE, include_nhis = FALSE)
```

## Arguments

- df:

  A data frame with at least a `gross_monthly` column. Optional:
  `employee_id`, `name`, `annual_rent`, `basic_monthly`.

- law:

  Character. Law key or `"auto"`. Default `"NTA2025"`.

- include_nhf:

  Logical. Default `TRUE`.

- include_nhis:

  Logical. Default `FALSE`.

## Value

A data frame with one row per employee.

## Examples

``` r
employees <- data.frame(
  name          = c("Emeka", "Ngozi", "Tunde"),
  gross_monthly = c(693228.96, 1624734, 3500000),
  annual_rent   = c(0, 1800000, 2400000)
)
calc_paye_batch(employees)
#>    name gross_monthly gross_annual pension_monthly nhf_monthly nhis_monthly
#> 1 Emeka        693229      8318748        55458.32    17330.72            0
#> 2 Ngozi       1624734     19496808       129978.72    40618.35            0
#> 3 Tunde       3500000     42000000       280000.00    87500.00            0
#>   cra_annual annual_rent rent_relief_annual total_relief_annual
#> 1          0           0                  0            873468.5
#> 2          0     1800000             360000           2407164.8
#> 3          0     2400000             480000           4890000.0
#>   taxable_income_annual annual_tax monthly_tax effective_rate_pct net_monthly
#> 1               7445279    1130150    94179.19              13.59    526260.7
#> 2              17089643    3018825   251568.76              15.48   1202568.2
#> 3              37110000    7465300   622108.33              17.77   2510391.7
#>       law
#> 1 NTA2025
#> 2 NTA2025
#> 3 NTA2025
```
