#' Internal: apply progressive tax bands to an annual taxable income
#' @keywords internal
.apply_bands <- function(taxable_annual, bands) {
  remaining  <- taxable_annual
  cumulative <- 0
  band_detail <- vector("list", nrow(bands))

  for (i in seq_len(nrow(bands))) {
    chunk     <- min(remaining, bands$band_width[i])
    tax_chunk <- chunk * bands$rate[i]

    band_detail[[i]] <- list(
      label          = bands$label[i],
      rate           = bands$rate[i],
      band_width     = bands$band_width[i],
      income_in_band = chunk,
      tax            = tax_chunk
    )

    cumulative <- cumulative + tax_chunk
    remaining  <- remaining - chunk
    if (remaining <= 0) break
  }

  # Pad remaining bands with zeros
  for (j in seq_along(band_detail)) {
    if (is.null(band_detail[[j]])) {
      band_detail[[j]] <- list(
        label = bands$label[j], rate = bands$rate[j],
        band_width = bands$band_width[j], income_in_band = 0, tax = 0
      )
    }
  }

  list(band_detail = band_detail, annual_tax = cumulative)
}


#' Calculate Nigerian PAYE tax for one employee
#'
#' Computes the full PAYE tax liability for a single employee. The entire
#' computation is driven by the law registry — passing a different \code{law}
#' key automatically uses that law's bands, rates, and relief rules.
#'
#' @param gross_monthly Numeric. Monthly gross income in Naira.
#' @param annual_rent Numeric. Annual rent paid (for laws with rent relief).
#'   Default \code{0}.
#' @param include_nhf Logical. Include NHF? Default \code{TRUE}.
#' @param include_nhis Logical. Include NHIS? Default \code{FALSE}.
#' @param law Character. Tax law key from the registry. Use
#'   \code{list_tax_laws()} to see all options. Default \code{"NTA2025"}.
#'   Pass \code{"auto"} to automatically select based on today's date.
#' @param basic_monthly Numeric or \code{NULL}. Basic salary for NHF base.
#'
#' @return An object of class \code{nigeria_tax}.
#'
#' @examples
#' # NTA 2025 (default)
#' calc_paye(1624734, annual_rent = 1800000)
#'
#' # Auto-select law by today's date
#' calc_paye(1624734, law = "auto")
#'
#' # Legacy PITA
#' calc_paye(1624734, law = "PITA")
#'
#' @export
calc_paye <- function(gross_monthly,
                      annual_rent   = 0,
                      include_nhf   = TRUE,
                      include_nhis  = FALSE,
                      law           = "NTA2025",
                      basic_monthly = NULL) {

  # Auto-select law by today's date if requested
  if (identical(law, "auto")) law <- get_applicable_law()

  rel        <- calc_reliefs(gross_monthly, annual_rent, include_nhf, include_nhis, law, basic_monthly)
  law_def    <- .get_law(law)
  tax_result <- .apply_bands(rel$taxable_income_annual, law_def$bands)

  effective_rate <- if (rel$gross_annual > 0) tax_result$annual_tax / rel$gross_annual else 0

  structure(
    list(
      gross_monthly         = gross_monthly,
      gross_annual          = rel$gross_annual,
      reliefs               = rel,
      taxable_income_annual = rel$taxable_income_annual,
      band_detail           = tax_result$band_detail,
      annual_tax            = tax_result$annual_tax,
      monthly_tax           = tax_result$annual_tax / 12,
      effective_rate        = effective_rate,
      law                   = law,
      law_description       = law_def$description
    ),
    class = c("nigeria_tax", "list")
  )
}


#' Calculate monthly net (take-home) salary
#'
#' @inheritParams calc_paye
#' @return A named list of class \code{nigeria_net}.
#'
#' @examples
#' calc_net_salary(1624734, annual_rent = 1800000)
#'
#' @export
calc_net_salary <- function(gross_monthly,
                            annual_rent   = 0,
                            include_nhf   = TRUE,
                            include_nhis  = FALSE,
                            law           = "NTA2025",
                            basic_monthly = NULL) {

  if (identical(law, "auto")) law <- get_applicable_law()
  tax <- calc_paye(gross_monthly, annual_rent, include_nhf, include_nhis, law, basic_monthly)
  rel <- tax$reliefs

  total_deductions <- rel$pension_monthly + rel$nhf_monthly + rel$nhis_monthly + tax$monthly_tax

  structure(
    list(
      gross_monthly            = gross_monthly,
      pension_monthly          = rel$pension_monthly,
      nhf_monthly              = rel$nhf_monthly,
      nhis_monthly             = rel$nhis_monthly,
      paye_monthly             = tax$monthly_tax,
      total_deductions_monthly = total_deductions,
      net_monthly              = gross_monthly - total_deductions,
      net_annual               = (gross_monthly - total_deductions) * 12,
      tax_obj                  = tax
    ),
    class = c("nigeria_net", "list")
  )
}
