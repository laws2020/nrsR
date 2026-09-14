# Print a side-by-side tax law comparison

Print a side-by-side tax law comparison

## Usage

``` r
# S3 method for class 'nigeria_comparison'
print(x, ...)
```

## Arguments

- x:

  An object of class `nigeria_comparison` from
  [`compare_tax_laws`](https://laws2020.github.io/nrsR/reference/compare_tax_laws.md).

- ...:

  Unused; for S3 compatibility.

## Value

Invisibly returns `x`. Called for its side effect of printing.

## Examples

``` r
print(compare_tax_laws(1624734, annual_rent = 1800000))
#> 
#> ======================================================================
#>  TAX LAW COMPARISON
#> ======================================================================
#>  Metric                      PITA                NTA2025            
#> ----------------------------------------------------------------------
#>  Annual Relief               ₦6,146,526.44     ₦2,407,164.84    
#>  Taxable Income (Ann.)       ₦13,350,281.56    ₦17,089,643.16   
#>  Annual Tax                  ₦2,996,067.57     ₦3,018,825.06    
#>  Monthly Tax                 ₦249,672.30       ₦251,568.76      
#>  Effective Rate (%)          15.37%              15.48%             
#>  Net Monthly Pay             ₦1,204,464.63     ₦1,202,568.17    
#> ======================================================================
#> 
```
