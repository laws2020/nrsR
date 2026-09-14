test_that("format_naira formats with symbol, commas and decimals", {
  expect_equal(format_naira(1624734), "\u20a61,624,734.00")
  expect_equal(format_naira(1000, digits = 0), "\u20a61,000")
})
