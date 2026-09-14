
# test_that() groups related expectations together
test_that("NTA2025: pension is 8% of gross monthly", {

  r <- calc_reliefs(gross_monthly = 500000, law = 'NTA2025')

  # expect_equal checks a value matches exactly (within tolerance)
  expect_equal(r$pension_monthly, 40000)
  expect_equal(r$pension_annual,  480000)
})

test_that("calc_reliefs: negative gross raises error", {

  # expect_error checks that the function throws an error
  expect_error(calc_reliefs(-100), 'cannot be negative')
})

test_that("NTA2025: rent relief capped at 500000/year", {

  r <- calc_reliefs(500000, annual_rent = 4000000, law = 'NTA2025')
  expect_equal(r$rent_relief_annual, 500000)   # cap enforced
})

test_that("NHIS toggle adds 5% deduction", {
  r <- calc_reliefs(500000, include_nhis = TRUE, law = "NTA2025")
  expect_equal(r$nhis_monthly, 25000)
})

test_that("include_nhf = FALSE zeroes NHF", {
  r <- calc_reliefs(500000, include_nhf = FALSE)
  expect_equal(r$nhf_monthly, 0)
})

test_that("basic_monthly is used as NHF base when supplied", {
  r <- calc_reliefs(500000, basic_monthly = 200000, law = "NTA2025")
  expect_equal(r$nhf_monthly, 200000 * 0.025)
})

test_that("PITA applies CRA and no rent relief", {
  r <- calc_reliefs(500000, annual_rent = 1000000, law = "PITA")
  expect_gt(r$cra_annual, 0)
  expect_equal(r$rent_relief_annual, 0)
})
