test_that("NTA2025: zero income yields zero tax", {
  r <- calc_paye(0, law = "NTA2025")
  expect_equal(r$annual_tax, 0)
  expect_equal(r$monthly_tax, 0)
  expect_equal(r$effective_rate, 0)
})

test_that("NTA2025: taxable income within first (0%) band yields zero tax", {
  # Low gross -> after reliefs taxable income falls in the 0% band
  r <- calc_paye(50000, include_nhf = FALSE, law = "NTA2025")
  expect_equal(r$annual_tax, 0)
})

test_that("calc_paye monthly_tax equals annual_tax / 12", {
  r <- calc_paye(1624734, annual_rent = 1800000)
  expect_equal(r$monthly_tax, r$annual_tax / 12)
})

test_that("effective_rate = annual_tax / gross_annual", {
  r <- calc_paye(1624734)
  expect_equal(r$effective_rate, r$annual_tax / r$gross_annual)
})

test_that("PITA path computes and returns a nigeria_tax object", {
  r <- calc_paye(1624734, law = "PITA")
  expect_s3_class(r, "nigeria_tax")
  expect_gt(r$annual_tax, 0)
})

test_that("very high income reaches the top Inf band", {
  r <- calc_paye(10000000, law = "NTA2025")
  top <- r$band_detail[[length(r$band_detail)]]
  expect_gt(top$income_in_band, 0)
})

