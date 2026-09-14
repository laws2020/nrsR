# nrsR 0.0.1

## Package hardening

- Adopted `MIT + file LICENSE`; added `LICENSE` and `LICENSE.md`.  
- Replaced non-ASCII characters in source with escapes (`\u20a6`) /
  ASCII.  
- Completed roxygen docs for
  [`tax_breakdown()`](https://laws2020.github.io/nrsR/reference/tax_breakdown.md),
  [`calc_net_salary()`](https://laws2020.github.io/nrsR/reference/calc_net_salary.md),
  and the  
  `print`/`summary` S3 methods; regenerated `NAMESPACE`.  
- Added input validation to
  [`calc_paye_batch()`](https://laws2020.github.io/nrsR/reference/calc_paye_batch.md)
  (empty frame, non-numeric,  
  `NA`, and negative `gross_monthly` now abort with row-level
  messages).  
- Added a comprehensive `testthat` suite covering
  [`calc_paye()`](https://laws2020.github.io/nrsR/reference/calc_paye.md),  
  [`.apply_bands()`](https://laws2020.github.io/nrsR/reference/dot-apply_bands.md),
  [`calc_paye_batch()`](https://laws2020.github.io/nrsR/reference/calc_paye_batch.md),
  [`compare_tax_laws()`](https://laws2020.github.io/nrsR/reference/compare_tax_laws.md),  
  [`tax_breakdown()`](https://laws2020.github.io/nrsR/reference/tax_breakdown.md),
  the law registry,
  [`format_naira()`](https://laws2020.github.io/nrsR/reference/format_naira.md),
  and print methods.  
- Added GitHub Actions CI (`R-CMD-check`, test coverage).
