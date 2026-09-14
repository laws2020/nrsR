test_that("get_applicable_law dispatches by date", {
  expect_equal(get_applicable_law("2025-06-01"), "PITA")
  expect_equal(get_applicable_law("2026-03-01"), "NTA2025")
})

test_that("get_applicable_law aborts for out-of-range dates", {
  expect_error(get_applicable_law("1800-01-01"), "No tax law found")
})

test_that(".get_law aborts on unknown key", {
  expect_error(nrsR:::.get_law("NOPE"), "Unknown tax law")
})

test_that("list_tax_laws returns expected columns", {
  out <- list_tax_laws()
  expect_true(all(c("key", "description", "effective_from",
                    "effective_to", "n_bands") %in% names(out)))
  expect_true(all(c("PITA", "NTA2025") %in% out$key))
})
