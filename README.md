
# nrsR <img src="man/figures/logo.png" align="right" height="139" />

> **Nigeria Revenue Service R Toolkit** — PAYE Tax Calculator

[![R
package](https://img.shields.io/badge/R-package-276DC3)](https://cran.r-project.org)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![NRS](https://img.shields.io/badge/Aligned%20with-NRS%202025%20Reforms-008751)](https://www.nrs.gov.ng)

------------------------------------------------------------------------

## About

**nrsR** is named after the **Nigeria Revenue Service (NRS)**, which
replaced the Federal Inland Revenue Service (FIRS) under the landmark
2025 tax reforms signed by President Tinubu.

The package computes Nigerian Pay-As-You-Earn (PAYE) income tax using a
**versioned law registry** architecture — future amendments require
editing exactly **one file** with zero changes to any calculation logic.

------------------------------------------------------------------------

## Supported Tax Laws

| Key        | Description                      | Effective          |
|------------|----------------------------------|--------------------|
| `PITA`     | Personal Income Tax Act (legacy) | up to Dec 2025     |
| `NTA2025`  | Nigeria Tax Act 2025             | Jan 2026 → current |
| *(future)* | Add new entry to registry        | automatic          |

------------------------------------------------------------------------

## New Tax Law at a Glance (NTA 2025)

| Band (Annual Income)           | Rate              |
|--------------------------------|-------------------|
| First ₦800,000                 | **0%** (tax-free) |
| Next ₦2,200,000 (₦800K – ₦3M)  | 15%               |
| Next ₦9,000,000 (₦3M – ₦12M)   | 18%               |
| Next ₦13,000,000 (₦12M – ₦25M) | 21%               |
| Next ₦25,000,000 (₦25M – ₦50M) | 23%               |
| Above ₦50,000,000              | 25%               |

------------------------------------------------------------------------

## Installation

``` r
# From source tarball
install.packages("nrsR_2.0.0.tar.gz", repos = NULL, type = "source")

# Or from GitHub (when published)
# remotes::install_github("laws2020/nrsR")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(nrsR)

# See all registered tax laws
list_tax_laws()

# Auto-select the right law by today's date
calc_paye(1624734, law = "auto")

# NTA 2025 — with rent relief
calc_paye(1624734, annual_rent = 1800000)

# Monthly take-home pay
calc_net_salary(693228.96)

# Compare all laws side-by-side
compare_tax_laws(1624734, annual_rent = 1800000)

# Batch payroll for entire workforce
calc_paye_batch(employee_df)

# Progressive band breakdown
tax_breakdown(1624734)
```

------------------------------------------------------------------------

## Adding a Future Amendment (The Whole Point)

Open `R/law_registry.R` and append one block to `.TAX_LAW_REGISTRY`:

``` r
NTA2028 = list(
  key            = "NTA2028",
  description    = "Nigeria Tax Amendment Act 2028",
  effective_from = as.Date("2028-01-01"),
  effective_to   = NA,
  bands          = data.frame(
    band_width = c(1200000, 2800000, ...),  # new thresholds
    rate       = c(0.00, 0.15, ...),
    label      = c("First ₦1.2M @0%", ...),
    stringsAsFactors = FALSE
  ),
  deductions   = list(pension_employee_rate = 0.08, nhf_rate = 0.025, ...),
  relief_rules = list(rent_relief_rate = 0.20, rent_relief_max = 700000, ...)
)
```

**That is the only change required.** Every function in the package
picks it up automatically.

------------------------------------------------------------------------

## Core Functions

| Function               | Description                        |
|------------------------|------------------------------------|
| `calc_paye()`          | PAYE tax for one employee          |
| `calc_net_salary()`    | Monthly take-home pay              |
| `calc_reliefs()`       | All statutory deductions & reliefs |
| `calc_paye_batch()`    | Batch payroll for a data frame     |
| `compare_tax_laws()`   | Side-by-side multi-law comparison  |
| `tax_breakdown()`      | Progressive band detail            |
| `list_tax_laws()`      | All laws in the registry           |
| `get_applicable_law()` | Law key for a given date           |
| `format_naira()`       | Format numbers as ₦ currency       |

------------------------------------------------------------------------

## References

- Nigeria Tax Act (NTA) 2025
- [Nigeria Revenue Service — nrs.gov.ng](https://www.nrs.gov.ng)
- [PaidHR: Understanding PAYE Tax in
  Nigeria](https://www.paidhr.com/blog/understanding-tax-computation-in-nigeria-a-step-by-step-guide)
