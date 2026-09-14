# List all registered tax laws

Returns a summary data frame of every tax law version in the registry,
including its key, description, and effective date range.

## Usage

``` r
list_tax_laws()
```

## Value

A data frame with columns: `key`, `description`, `effective_from`,
`effective_to`.

## Examples

``` r
list_tax_laws()
#>             key                                   description effective_from
#> PITA       PITA              Personal Income Tax Act (legacy)     1993-01-01
#> NTA2025 NTA2025 Nigeria Tax Act 2025 (effective January 2026)     2026-01-01
#>         effective_to n_bands
#> PITA      2025-12-31       6
#> NTA2025      current       6
```
