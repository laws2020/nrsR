#' Calculate statutory tax reliefs for an employee
#'
#' Computes all deductions and reliefs that reduce an employee's taxable
#' income. All rates and caps are read directly from the law registry —
#' no hardcoded values. When a new tax amendment is registered, this
#' function automatically picks up the new parameters.
#'
#' @param gross_monthly Numeric. Monthly gross income in Naira.
#' @param annual_rent Numeric. Annual rent paid in Naira.
#'   Only relevant for laws with rent relief (e.g. NTA2025). Default \code{0}.
#' @param include_nhf Logical. Include NHF deduction? Default \code{TRUE}.
#' @param include_nhis Logical. Include NHIS deduction? Default \code{FALSE}.
#' @param law Character. Tax law key. Use \code{list_tax_laws()} to see options.
#'   Default \code{"NTA2025"}.
#' @param basic_monthly Numeric or \code{NULL}. Monthly basic salary for NHF
#'   calculation. If \code{NULL}, gross is used.
#'
#' @return A named list of class \code{nigeria_reliefs}.
#'
#' @examples
#' calc_reliefs(693228.96)
#' calc_reliefs(693228.96, annual_rent = 1200000, law = "NTA2025")
#' calc_reliefs(693228.96, law = "PITA")
#'
#' @export
calc_reliefs <- function(gross_monthly,
                         annual_rent    = 0,
                         include_nhf    = TRUE,
                         include_nhis   = FALSE,
                         law            = "NTA2025",
                         basic_monthly  = NULL) {

  if (!is.numeric(gross_monthly) || length(gross_monthly) != 1)
    rlang::abort("`gross_monthly` must be a single numeric value.")
  if (gross_monthly < 0)
    rlang::abort("`gross_monthly` cannot be negative.")
  if (!is.numeric(annual_rent) || annual_rent < 0)
    rlang::abort("`annual_rent` must be a non-negative number.")

  law_def  <- .get_law(law)
  ded      <- law_def$deductions
  rel_r    <- law_def$relief_rules
  nhf_base <- if (!is.null(basic_monthly)) basic_monthly else gross_monthly
  gross_annual <- gross_monthly * 12

  pension_monthly <- gross_monthly * ded$pension_employee_rate
  pension_annual  <- pension_monthly * 12

  nhf_monthly <- if (include_nhf) nhf_base * ded$nhf_rate else 0
  nhf_annual  <- nhf_monthly * 12

  nhis_monthly <- if (include_nhis) gross_monthly * ded$nhis_rate else 0
  nhis_annual  <- nhis_monthly * 12

  cra_annual <- if (!is.null(rel_r$cra_fixed) && !is.null(rel_r$cra_variable_rate)) {
    rel_r$cra_fixed + (rel_r$cra_variable_rate * gross_annual)
  } else 0

  rent_relief_annual <- if (!is.null(rel_r$rent_relief_rate) && !is.null(rel_r$rent_relief_max)) {
    min(annual_rent * rel_r$rent_relief_rate, rel_r$rent_relief_max)
  } else 0

  total_relief_annual   <- pension_annual + nhf_annual + nhis_annual + cra_annual + rent_relief_annual
  total_relief_monthly  <- total_relief_annual / 12
  taxable_income_annual <- max(gross_annual - total_relief_annual, 0)

  structure(
    list(
      gross_monthly         = gross_monthly,
      gross_annual          = gross_annual,
      pension_monthly       = pension_monthly,
      pension_annual        = pension_annual,
      nhf_monthly           = nhf_monthly,
      nhf_annual            = nhf_annual,
      nhis_monthly          = nhis_monthly,
      nhis_annual           = nhis_annual,
      cra_annual            = cra_annual,
      annual_rent           = annual_rent,
      rent_relief_annual    = rent_relief_annual,
      total_relief_monthly  = total_relief_monthly,
      total_relief_annual   = total_relief_annual,
      taxable_income_annual = taxable_income_annual,
      law                   = law,
      law_description       = law_def$description
    ),
    class = c("nigeria_reliefs", "list")
  )
}
