test_that(".apply_bands splits income across bands correctly", {
  bands <- data.frame(
    band_width = c(1000, 1000, Inf),
    rate       = c(0.00, 0.10, 0.20),
    label      = c("b1", "b2", "b3"),
    stringsAsFactors = FALSE
  )
  res <- nrsR:::.apply_bands(2500, bands)
  expect_equal(res$band_detail[[1]]$income_in_band, 1000)
  expect_equal(res$band_detail[[2]]$income_in_band, 1000)
  expect_equal(res$band_detail[[3]]$income_in_band, 500)
  expect_equal(res$band_detail[[2]]$tax, 100)
  expect_equal(res$band_detail[[3]]$tax, 100)
  expect_equal(res$annual_tax, 200)
})

test_that(".apply_bands zero-pads unreached bands", {
  bands <- data.frame(
    band_width = c(1000, 1000, Inf),
    rate       = c(0.00, 0.10, 0.20),
    label      = c("b1", "b2", "b3"),
    stringsAsFactors = FALSE
  )
  res <- nrsR:::.apply_bands(500, bands)
  expect_equal(res$band_detail[[2]]$income_in_band, 0)
  expect_equal(res$band_detail[[3]]$tax, 0)
  expect_equal(res$annual_tax, 0)
})
