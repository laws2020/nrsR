test_that("print/summary methods run without error", {
  expect_output(print(calc_paye(1624734)), "PAYE TAX COMPUTATION")
  expect_output(print(calc_net_salary(1624734)), "PAYSLIP SUMMARY")
  expect_output(print(compare_tax_laws(1624734)), "TAX LAW COMPARISON")
  expect_output(summary(calc_paye(1624734)), "nrsr")
})
