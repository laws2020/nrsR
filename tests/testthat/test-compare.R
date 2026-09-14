test_that("compare_tax_laws returns one row per law with correct class", {
  out <- compare_tax_laws(1624734, laws = c("PITA", "NTA2025"))
  expect_s3_class(out, "nigeria_comparison")
  expect_equal(nrow(out), 2)
  expect_setequal(out$law, c("PITA", "NTA2025"))
})
