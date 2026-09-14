# Print a Nigerian reliefs breakdown

Print a Nigerian reliefs breakdown

## Usage

``` r
# S3 method for class 'nigeria_reliefs'
print(x, ...)
```

## Arguments

- x:

  An object of class `nigeria_reliefs`.

- ...:

  Unused; for S3 compatibility.

## Value

`x`, invisibly.

## Examples

``` r
print(calc_reliefs(1624734, annual_rent = 1800000))
#> $gross_monthly
#> [1] 1624734
#> 
#> $gross_annual
#> [1] 19496808
#> 
#> $pension_monthly
#> [1] 129978.7
#> 
#> $pension_annual
#> [1] 1559745
#> 
#> $nhf_monthly
#> [1] 40618.35
#> 
#> $nhf_annual
#> [1] 487420.2
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
#> [1] 1800000
#> 
#> $rent_relief_annual
#> [1] 360000
#> 
#> $total_relief_monthly
#> [1] 200597.1
#> 
#> $total_relief_annual
#> [1] 2407165
#> 
#> $taxable_income_annual
#> [1] 17089643
#> 
#> $law
#> [1] "NTA2025"
#> 
#> $law_description
#> [1] "Nigeria Tax Act 2025 (effective January 2026)"
#> 
#> attr(,"class")
#> [1] "nigeria_reliefs" "list"           
```
