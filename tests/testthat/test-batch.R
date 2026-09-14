make_df <- function() data.frame(
  employee_id   = c("E1", "E2"),
  name          = c("Emeka", "Ngozi"),
  gross_monthly = c(693228.96, 1624734),
  annual_rent   = c(0, 1800000),
  stringsAsFactors = FALSE
)

test_that("calc_paye_batch returns one row per employee and keeps id cols", {
  out <- calc_paye_batch(make_df())
  expect_equal(nrow(out), 2)
  expect_true(all(c("employee_id", "name") %in% names(out)))
})

test_that("optional columns default when absent", {
  out <- calc_paye_batch(data.frame(gross_monthly = c(500000, 800000)))
  expect_equal(nrow(out), 2)
})

test_that("law = 'auto' resolves without error", {
  out <- calc_paye_batch(data.frame(gross_monthly = 500000), law = "auto")
  expect_equal(nrow(out), 1)
})

test_that("validation rejects empty, non-numeric, NA and negative gross", {
  expect_error(calc_paye_batch(data.frame(gross_monthly = numeric(0))),
               "at least one row")
  expect_error(calc_paye_batch(data.frame(gross_monthly = c("a", "b"))),
               "must be numeric")
  expect_error(calc_paye_batch(data.frame(gross_monthly = c(500000, NA))),
               "NA at row")
  expect_error(calc_paye_batch(data.frame(gross_monthly = c(500000, -1))),
               "negative at row")
})
