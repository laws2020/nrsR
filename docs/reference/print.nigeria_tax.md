# Print a Nigerian PAYE tax computation

Print a Nigerian PAYE tax computation

## Usage

``` r
# S3 method for class 'nigeria_tax'
print(x, ...)
```

## Arguments

- x:

  An object of class `nigeria_tax` from
  [`calc_paye`](https://laws2020.github.io/nrsR/reference/calc_paye.md).

- ...:

  Unused; for S3 compatibility.

## Value

Invisibly returns `x`. Called for its side effect of printing.

## Examples

``` r
print(calc_paye(1624734, annual_rent = 1800000))
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
```
