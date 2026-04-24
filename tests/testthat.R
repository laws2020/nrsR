library(testthat)
library(nrsR)

test_that("calc_reliefs NTA2025: pension is 8% of gross monthly", {
  r <- calc_reliefs(gross_monthly = 500000, law = "NTA2025")
  expect_equal(r$pension_monthly, 40000)
  expect_equal(r$pension_annual,  480000)
})

test_that("calc_reliefs NTA2025: NHF is 2.5% of gross", {
  r <- calc_reliefs(gross_monthly = 500000, include_nhf = TRUE, law = "NTA2025")
  expect_equal(r$nhf_monthly, 12500)
})

test_that("calc_reliefs NTA2025: rent relief capped at 500000 per year", {
  r <- calc_reliefs(gross_monthly = 500000, annual_rent = 4000000, law = "NTA2025")
  expect_equal(r$rent_relief_annual, 500000)
})

test_that("calc_reliefs NTA2025: rent relief is 20% of rent when below cap", {
  r <- calc_reliefs(gross_monthly = 500000, annual_rent = 2000000, law = "NTA2025")
  expect_equal(r$rent_relief_annual, 400000)
})

test_that("calc_reliefs NTA2025: no rent => no rent relief", {
  r <- calc_reliefs(gross_monthly = 500000, annual_rent = 0, law = "NTA2025")
  expect_equal(r$rent_relief_annual, 0)
})

test_that("calc_reliefs PITA: CRA computed correctly", {
  gross_m <- 693228.96
  r <- calc_reliefs(gross_monthly = gross_m, law = "PITA")
  expected_cra <- 200000 + 0.20 * (gross_m * 12)
  expect_equal(r$cra_annual, expected_cra, tolerance = 1e-2)
})

test_that("calc_reliefs PITA: no rent relief", {
  r <- calc_reliefs(gross_monthly = 500000, annual_rent = 1200000, law = "PITA")
  expect_equal(r$rent_relief_annual, 0)
  expect_gt(r$cra_annual, 0)
})

test_that("calc_reliefs: negative gross raises error", {
  expect_error(calc_reliefs(gross_monthly = -100), "cannot be negative")
})

test_that("calc_reliefs: zero gross returns zero taxable income", {
  r <- calc_reliefs(gross_monthly = 0, law = "NTA2025")
  expect_equal(r$taxable_income_annual, 0)
})

# ============================================================
# Tests: calc_paye() — NTA 2025
# ============================================================

test_that("NTA2025: income <= 800K annual => zero tax", {
  # 65000/month * 12 = 780000 < 800000
  t <- calc_paye(gross_monthly = 65000, law = "NTA2025", include_nhf = FALSE)
  expect_equal(t$annual_tax, 0)
})

test_that("NTA2025: Employee 1 — gross 693228.96/month", {
  t <- calc_paye(693228.96, law = "NTA2025", include_nhf = TRUE)
  # Annual gross = 8,318,747.52
  # Pension annual = 665,499.80
  # NHF annual = 207,968.69
  # No rent relief
  # Taxable = 8,318,747.52 - 665,499.80 - 207,968.69 = 7,445,278.03 (approx)
  # Band1: 800K @ 0% = 0
  # Band2: 2,200,000 @ 15% = 330,000
  # Band3: (7,445,278.03 - 3,000,000) @ 18% = 4,445,278.03 * 0.18 = 800,150.05 (approx)
  expect_true(t$annual_tax > 0)
  expect_equal(t$monthly_tax, t$annual_tax / 12, tolerance = 1e-6)
  expect_equal(t$law, "NTA2025")
})

test_that("NTA2025: Employee 2 — gross 1624734/month", {
  t <- calc_paye(1624734, law = "NTA2025")
  expect_true(t$annual_tax > 0)
  # Gross annual = 19,496,808
  # Should hit at least bands 1, 2, and 3
  bd <- t$band_detail
  band_names <- sapply(bd, `[[`, "label")
  expect_true(any(grepl("15%", band_names)))
  expect_true(any(grepl("18%", band_names)))
})

test_that("NTA2025: rent relief lowers tax", {
  t_no_rent   <- calc_paye(1624734, annual_rent = 0,       law = "NTA2025")
  t_with_rent <- calc_paye(1624734, annual_rent = 2400000, law = "NTA2025")
  expect_lt(t_with_rent$annual_tax, t_no_rent$annual_tax)
})

test_that("NTA2025: effective rate is between 0 and 1", {
  t <- calc_paye(1624734, law = "NTA2025")
  expect_gte(t$effective_rate, 0)
  expect_lte(t$effective_rate, 1)
})

# ============================================================
# Tests: calc_paye() — PITA legacy
# ============================================================

test_that("PITA: Employee 1 monthly tax ~ 1643.80 (from provided data)", {
  # From the original payslip: monthly tax = 1,643.80
  t <- calc_paye(693228.96, law = "PITA", include_nhf = TRUE)
  expect_equal(t$monthly_tax, 1643.80, tolerance = 5)
})

test_that("PITA: Employee 2 monthly tax ~ 8614.88 (from provided data)", {
  # From original payslip: monthly tax = 8,614.88
  t <- calc_paye(1624734, law = "PITA", include_nhf = TRUE)
  expect_equal(t$monthly_tax, 8614.88, tolerance = 10)
})

# ============================================================
# Tests: calc_net_salary()
# ============================================================

test_that("calc_net_salary: net + deductions = gross", {
  n <- calc_net_salary(1000000, law = "NTA2025")
  expect_equal(
    n$net_monthly + n$total_deductions_monthly,
    1000000,
    tolerance = 1e-2
  )
})

test_that("calc_net_salary: net is positive for reasonable income", {
  n <- calc_net_salary(500000)
  expect_gt(n$net_monthly, 0)
})

# ============================================================
# Tests: calc_paye_batch()
# ============================================================

test_that("calc_paye_batch: returns correct number of rows", {
  df <- data.frame(
    name          = c("Emeka", "Ngozi", "Tunde"),
    gross_monthly = c(500000, 693228.96, 1624734),
    annual_rent   = c(0, 600000, 1800000)
  )
  result <- calc_paye_batch(df, law = "NTA2025")
  expect_equal(nrow(result), 3)
})

test_that("calc_paye_batch: contains required columns", {
  df <- data.frame(gross_monthly = c(500000, 1000000))
  result <- calc_paye_batch(df)
  expected_cols <- c("gross_monthly", "annual_tax", "monthly_tax", "net_monthly")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("calc_paye_batch: errors without gross_monthly column", {
  df <- data.frame(salary = c(500000))
  expect_error(calc_paye_batch(df), "gross_monthly")
})

# ============================================================
# Tests: compare_tax_laws()
# ============================================================

test_that("compare_tax_laws: returns 2 rows", {
  cmp <- compare_tax_laws(1624734, annual_rent = 1800000)
  expect_equal(nrow(cmp), 2)
})

test_that("compare_tax_laws: can extract results by law key", {
  cmp <- compare_tax_laws(1624734, annual_rent = 1800000)

  # $law contains the registry KEY ("PITA", "NTA2025"), not the description string
  pita_tax <- cmp$annual_tax[cmp$law == "PITA"]
  nta_tax  <- cmp$annual_tax[cmp$law == "NTA2025"]

  expect_false(is.na(pita_tax))
  expect_false(is.na(nta_tax))
})

test_that("compare_tax_laws: NTA2025 is cheaper than PITA for low-to-mid income earner", {
  # At low-to-mid gross (e.g. ₦500K/month), NTA2025 is cheaper because:
  #   - The ₦800K tax-free band alone saves more than PITA's CRA at that income level.
  # At HIGH gross (e.g. ₦1.6M+/month), PITA is actually cheaper because:
  #   - PITA CRA = ₦200K + 20% of gross annual — which grows with income.
  #   - NTA2025 rent relief is capped at ₦500K/year regardless of income.
  # This is the correct, verified economic behaviour of both laws.
  cmp      <- compare_tax_laws(500000, annual_rent = 600000)
  pita_tax <- cmp$annual_tax[cmp$law == "PITA"]
  nta_tax  <- cmp$annual_tax[cmp$law == "NTA2025"]

  expect_lt(nta_tax, pita_tax)
})

test_that("compare_tax_laws: PITA is cheaper than NTA2025 for high earner (CRA > rent relief)", {
  # At ₦1.6M+/month, PITA CRA (₦4M+ annual) >> NTA2025 rent relief (capped ₦500K).
  # This means PITA gives MORE relief, resulting in LOWER tax at high incomes.
  cmp      <- compare_tax_laws(1624734, annual_rent = 1800000)
  pita_tax <- cmp$annual_tax[cmp$law == "PITA"]
  nta_tax  <- cmp$annual_tax[cmp$law == "NTA2025"]

  expect_gt(nta_tax, pita_tax)
})

# ============================================================
# Tests: tax_breakdown()
# ============================================================

test_that("tax_breakdown: returns a data frame", {
  bd <- tax_breakdown(1624734)
  expect_s3_class(bd, "data.frame")
})

test_that("tax_breakdown: cumulative tax is non-decreasing", {
  bd <- tax_breakdown(1624734)
  expect_true(all(diff(bd$cumulative_tax) >= 0))
})

test_that("tax_breakdown: total matches calc_paye annual_tax", {
  t  <- calc_paye(1624734)
  bd <- tax_breakdown(1624734)
  expect_equal(max(bd$cumulative_tax), t$annual_tax, tolerance = 1e-4)
})

# ============================================================
# Tests: format_naira()
# ============================================================

test_that("format_naira: formats correctly", {
  expect_equal(format_naira(1000000), "\u20a61,000,000.00")
  expect_equal(format_naira(0),       "\u20a60.00")
})

# ============================================================
# Tests: Law Registry
# ============================================================

test_that("list_tax_laws returns a data frame with expected keys", {
  laws <- list_tax_laws()
  expect_s3_class(laws, "data.frame")
  expect_true("PITA"    %in% laws$key)
  expect_true("NTA2025" %in% laws$key)
})

test_that("get_applicable_law returns PITA for 2025 dates", {
  expect_equal(get_applicable_law("2025-01-01"), "PITA")
  expect_equal(get_applicable_law("2025-12-31"), "PITA")
})

test_that("get_applicable_law returns NTA2025 for 2026 dates", {
  expect_equal(get_applicable_law("2026-01-01"), "NTA2025")
  expect_equal(get_applicable_law("2026-06-15"), "NTA2025")
})

test_that("get_applicable_law raises error for unregistered date", {
  expect_error(get_applicable_law("1970-01-01"), "No tax law found")
})

test_that("calc_paye law='auto' returns a valid result", {
  t <- calc_paye(1000000, law = "auto")
  expect_s3_class(t, "nigeria_tax")
})

test_that("compare_tax_laws works with all registered laws", {
  cmp <- compare_tax_laws(1624734, annual_rent = 1800000)
  expect_true(nrow(cmp) >= 2)
})

