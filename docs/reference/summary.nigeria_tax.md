# Summarise a Nigerian PAYE tax computation

Summarise a Nigerian PAYE tax computation

## Usage

``` r
# S3 method for class 'nigeria_tax'
summary(object, ...)
```

## Arguments

- object:

  An object of class `nigeria_tax` from
  [`calc_paye`](https://laws2020.github.io/nrsR/reference/calc_paye.md).

- ...:

  Unused; for S3 compatibility.

## Value

Invisibly returns `object`. Called for its side effect of printing.

## Examples

``` r
summary(calc_paye(1624734, annual_rent = 1800000))
#> nrsr | Law: NTA2025 | Gross: ₦1,624,734.00 | Tax/month: ₦251,568.76 | Eff. rate: 15.48%
```
