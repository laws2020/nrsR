# Get the tax law applicable on a given date

Looks up the registry and returns the law key whose effective date range
covers `as_of`. Useful for automatic date-based dispatch.

## Usage

``` r
get_applicable_law(as_of = Sys.Date())
```

## Arguments

- as_of:

  A `Date` or character string (e.g. `"2026-03-01"`). Defaults to
  today's date.

## Value

Character string – the law key (e.g. `"NTA2025"`).

## Examples

``` r
get_applicable_law("2025-06-01")   # returns "PITA"
#> [1] "PITA"
get_applicable_law("2026-03-01")   # returns "NTA2025"
#> [1] "NTA2025"
get_applicable_law()               # returns law for today
#> [1] "NTA2025"
```
