test_that("tax_breakdown cumulative total equals calc_paye annual_tax", {
  gm <- 1624734
  bd <- tax_breakdown(gm)
  expect_equal(bd$cumulative_tax[nrow(bd)], calc_paye(gm)$annual_tax)
})
