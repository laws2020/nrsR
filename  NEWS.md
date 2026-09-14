# nrsR 0.0.1  
  
## Package hardening  
  
* Adopted `MIT + file LICENSE`; added `LICENSE` and `LICENSE.md`.  
* Replaced non-ASCII characters in source with escapes (`\u20a6`) / ASCII.  
* Completed roxygen docs for `tax_breakdown()`, `calc_net_salary()`, and the  
  `print`/`summary` S3 methods; regenerated `NAMESPACE`.  
* Added input validation to `calc_paye_batch()` (empty frame, non-numeric,  
  `NA`, and negative `gross_monthly` now abort with row-level messages).  
* Added a comprehensive `testthat` suite covering `calc_paye()`,  
  `.apply_bands()`, `calc_paye_batch()`, `compare_tax_laws()`,  
  `tax_breakdown()`, the law registry, `format_naira()`, and print methods.  
* Added GitHub Actions CI (`R-CMD-check`, test coverage).
