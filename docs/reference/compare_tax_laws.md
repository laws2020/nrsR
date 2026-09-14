# Compare tax liability across any number of registered law versions

Unlike the old `compare_tax_laws()` which was hardcoded to PITA vs
NTA2025, this version accepts any vector of law keys – including future
amendments. Simply add a new law to the registry and it appears here
automatically.

## Usage

``` r
compare_tax_laws(
  gross_monthly,
  annual_rent = 0,
  laws = names(.TAX_LAW_REGISTRY),
  include_nhf = TRUE,
  include_nhis = FALSE
)
```

## Arguments

- gross_monthly:

  Numeric. Monthly gross income in Naira.

- annual_rent:

  Numeric. Annual rent paid. Default `0`.

- laws:

  Character vector of law keys to compare. Default: all registered laws
  in chronological order.

- include_nhf:

  Logical. Default `TRUE`.

- include_nhis:

  Logical. Default `FALSE`.

## Value

A data frame with one row per law, showing key metrics side-by-side.

## Examples

``` r
# Compare all registered laws
compare_tax_laws(1624734, annual_rent = 1800000)
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

# Compare specific laws only
compare_tax_laws(1624734, laws = c("PITA", "NTA2025"))
#> 
#> ======================================================================
#>  TAX LAW COMPARISON
#> ======================================================================
#>  Metric                      PITA                NTA2025            
#> ----------------------------------------------------------------------
#>  Annual Relief               ₦6,146,526.44     ₦2,047,164.84    
#>  Taxable Income (Ann.)       ₦13,350,281.56    ₦17,449,643.16   
#>  Annual Tax                  ₦2,996,067.57     ₦3,094,425.06    
#>  Monthly Tax                 ₦249,672.30       ₦257,868.76      
#>  Effective Rate (%)          15.37%              15.87%             
#>  Net Monthly Pay             ₦1,204,464.63     ₦1,196,268.17    
#> ======================================================================
#> 
```
