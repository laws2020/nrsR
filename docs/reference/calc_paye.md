# Calculate Nigerian PAYE tax for one employee

Computes the full PAYE tax liability for a single employee. The entire
computation is driven by the law registry – passing a different `law`
key automatically uses that law's bands, rates, and relief rules.

## Usage

``` r
calc_paye(
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

  Numeric. Annual rent paid (for laws with rent relief). Default `0`.

- include_nhf:

  Logical. Include NHF? Default `TRUE`.

- include_nhis:

  Logical. Include NHIS? Default `FALSE`.

- law:

  Character. Tax law key from the registry. Use
  [`list_tax_laws()`](https://laws2020.github.io/nrsR/reference/list_tax_laws.md)
  to see all options. Default `"NTA2025"`. Pass `"auto"` to
  automatically select based on today's date.

- basic_monthly:

  Numeric or `NULL`. Basic salary for NHF base.

## Value

An object of class `nigeria_tax`.

## Examples

``` r
# NTA 2025 (default)
calc_paye(1624734, annual_rent = 1800000)
#> 
#> ==========================================================
#>  NIGERIAN PAYE TAX COMPUTATION
#>  Law: Nigeria Tax Act 2025 (effective January 2026) 
#> ==========================================================
#>  Monthly Gross:                      ₦1,624,734.00
#>  Annual Gross:                       ₦19,496,808.00
#> ----------------------------------------------------------
#>  DEDUCTIONS & RELIEFS
#>    Pension (8%/month):               ₦129,978.72
#>    NHF (2.5%/month):                 ₦40,618.35
#>    Rent Relief (annual):             ₦360,000.00
#>    Total Annual Relief:              ₦2,407,164.84
#> ----------------------------------------------------------
#>  Annual Taxable Income:              ₦17,089,643.16
#> ----------------------------------------------------------
#>  TAX BANDS
#>    First ₦800K @0%            ₦800,000.00  =>  ₦0.00
#>    Next ₦2.2M @15%            ₦2,200,000.00  =>  ₦330,000.00
#>    Next ₦9M @18%              ₦9,000,000.00  =>  ₦1,620,000.00
#>    Next ₦13M @21%             ₦5,089,643.16  =>  ₦1,068,825.06
#> ----------------------------------------------------------
#>  ANNUAL TAX:                         ₦3,018,825.06
#>  MONTHLY TAX:                        ₦251,568.76
#>  Effective Rate (on gross):          15.48%
#> ==========================================================
#> 

# Auto-select law by today's date
calc_paye(1624734, law = "auto")
#> 
#> ==========================================================
#>  NIGERIAN PAYE TAX COMPUTATION
#>  Law: Nigeria Tax Act 2025 (effective January 2026) 
#> ==========================================================
#>  Monthly Gross:                      ₦1,624,734.00
#>  Annual Gross:                       ₦19,496,808.00
#> ----------------------------------------------------------
#>  DEDUCTIONS & RELIEFS
#>    Pension (8%/month):               ₦129,978.72
#>    NHF (2.5%/month):                 ₦40,618.35
#>    Total Annual Relief:              ₦2,047,164.84
#> ----------------------------------------------------------
#>  Annual Taxable Income:              ₦17,449,643.16
#> ----------------------------------------------------------
#>  TAX BANDS
#>    First ₦800K @0%            ₦800,000.00  =>  ₦0.00
#>    Next ₦2.2M @15%            ₦2,200,000.00  =>  ₦330,000.00
#>    Next ₦9M @18%              ₦9,000,000.00  =>  ₦1,620,000.00
#>    Next ₦13M @21%             ₦5,449,643.16  =>  ₦1,144,425.06
#> ----------------------------------------------------------
#>  ANNUAL TAX:                         ₦3,094,425.06
#>  MONTHLY TAX:                        ₦257,868.76
#>  Effective Rate (on gross):          15.87%
#> ==========================================================
#> 

# Legacy PITA
calc_paye(1624734, law = "PITA")
#> 
#> ==========================================================
#>  NIGERIAN PAYE TAX COMPUTATION
#>  Law: Personal Income Tax Act (legacy) 
#> ==========================================================
#>  Monthly Gross:                      ₦1,624,734.00
#>  Annual Gross:                       ₦19,496,808.00
#> ----------------------------------------------------------
#>  DEDUCTIONS & RELIEFS
#>    Pension (8%/month):               ₦129,978.72
#>    NHF (2.5%/month):                 ₦40,618.35
#>    CRA (annual):                     ₦4,099,361.60
#>    Total Annual Relief:              ₦6,146,526.44
#> ----------------------------------------------------------
#>  Annual Taxable Income:              ₦13,350,281.56
#> ----------------------------------------------------------
#>  TAX BANDS
#>    First ₦300K @7%            ₦300,000.00  =>  ₦21,000.00
#>    Next ₦300K @11%            ₦300,000.00  =>  ₦33,000.00
#>    Next ₦500K @15%            ₦500,000.00  =>  ₦75,000.00
#>    Next ₦500K @19%            ₦500,000.00  =>  ₦95,000.00
#>    Next ₦1.6M @21%            ₦1,600,000.00  =>  ₦336,000.00
#>    Above ₦3.2M @24%           ₦10,150,281.56  =>  ₦2,436,067.57
#> ----------------------------------------------------------
#>  ANNUAL TAX:                         ₦2,996,067.57
#>  MONTHLY TAX:                        ₦249,672.30
#>  Effective Rate (on gross):          15.37%
#> ==========================================================
#> 
```
