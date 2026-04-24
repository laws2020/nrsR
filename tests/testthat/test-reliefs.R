
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
