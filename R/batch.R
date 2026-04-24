#' Calculate PAYE for a batch of employees
#'
#' Processes a data frame of employees and returns a tidy payroll summary.
#' Supports any law in the registry — just pass its key.
#'
#' @param df A data frame with at least a \code{gross_monthly} column.
#'   Optional: \code{employee_id}, \code{name}, \code{annual_rent},
#'   \code{basic_monthly}.
#' @param law Character. Law key or \code{"auto"}. Default \code{"NTA2025"}.
#' @param include_nhf Logical. Default \code{TRUE}.
#' @param include_nhis Logical. Default \code{FALSE}.
#'
#' @return A data frame with one row per employee.
#'
#' @examples
#' employees <- data.frame(
#'   name          = c("Emeka", "Ngozi", "Tunde"),
#'   gross_monthly = c(693228.96, 1624734, 3500000),
#'   annual_rent   = c(0, 1800000, 2400000)
#' )
#' calc_paye_batch(employees)
#'
#' @export
calc_paye_batch <- function(df,
                            law          = "NTA2025",
                            include_nhf  = TRUE,
                            include_nhis = FALSE) {

  if (!is.data.frame(df))
    rlang::abort("`df` must be a data frame.")
  if (!"gross_monthly" %in% names(df))
    rlang::abort("`df` must contain a `gross_monthly` column.")

  if (identical(law, "auto")) law <- get_applicable_law()

  if (!"annual_rent"   %in% names(df)) df$annual_rent   <- 0
  if (!"basic_monthly" %in% names(df)) df$basic_monthly <- NA_real_

  results <- lapply(seq_len(nrow(df)), function(i) {
    row     <- df[i, ]
    basic_m <- if (is.na(row$basic_monthly)) NULL else row$basic_monthly

    tax <- calc_paye(row$gross_monthly, row$annual_rent, include_nhf, include_nhis, law, basic_m)
    rel <- tax$reliefs

    data.frame(
      gross_monthly          = row$gross_monthly,
      gross_annual           = tax$gross_annual,
      pension_monthly        = rel$pension_monthly,
      nhf_monthly            = rel$nhf_monthly,
      nhis_monthly           = rel$nhis_monthly,
      cra_annual             = rel$cra_annual,
      annual_rent            = rel$annual_rent,
      rent_relief_annual     = rel$rent_relief_annual,
      total_relief_annual    = rel$total_relief_annual,
      taxable_income_annual  = tax$taxable_income_annual,
      annual_tax             = tax$annual_tax,
      monthly_tax            = tax$monthly_tax,
      effective_rate_pct     = round(tax$effective_rate * 100, 2),
      net_monthly            = row$gross_monthly - rel$pension_monthly -
        rel$nhf_monthly - rel$nhis_monthly - tax$monthly_tax,
      law                    = law,
      stringsAsFactors       = FALSE
    )
  })

  out <- do.call(rbind, results)
  id_cols <- intersect(c("employee_id", "name"), names(df))
  if (length(id_cols) > 0) out <- cbind(df[, id_cols, drop = FALSE], out)
  out
}


#' Compare tax liability across any number of registered law versions
#'
#' Unlike the old \code{compare_tax_laws()} which was hardcoded to PITA vs
#' NTA2025, this version accepts any vector of law keys — including future
#' amendments. Simply add a new law to the registry and it appears here
#' automatically.
#'
#' @param gross_monthly Numeric. Monthly gross income in Naira.
#' @param annual_rent Numeric. Annual rent paid. Default \code{0}.
#' @param laws Character vector of law keys to compare.
#'   Default: all registered laws in chronological order.
#' @param include_nhf Logical. Default \code{TRUE}.
#' @param include_nhis Logical. Default \code{FALSE}.
#'
#' @return A data frame with one row per law, showing key metrics side-by-side.
#'
#' @examples
#' # Compare all registered laws
#' compare_tax_laws(1624734, annual_rent = 1800000)
#'
#' # Compare specific laws only
#' compare_tax_laws(1624734, laws = c("PITA", "NTA2025"))
#'
#' @export
compare_tax_laws <- function(gross_monthly,
                             annual_rent   = 0,
                             laws          = names(.TAX_LAW_REGISTRY),
                             include_nhf   = TRUE,
                             include_nhis  = FALSE) {

  rows <- lapply(laws, function(lk) {
    tax <- calc_paye(gross_monthly, annual_rent, include_nhf, include_nhis, law = lk)
    rel <- tax$reliefs
    data.frame(
      law                   = lk,
      law_description       = tax$law_description,
      total_relief_annual   = rel$total_relief_annual,
      taxable_income_annual = tax$taxable_income_annual,
      annual_tax            = tax$annual_tax,
      monthly_tax           = tax$monthly_tax,
      effective_rate_pct    = round(tax$effective_rate * 100, 2),
      net_monthly           = gross_monthly - rel$pension_monthly -
        rel$nhf_monthly - rel$nhis_monthly - tax$monthly_tax,
      stringsAsFactors      = FALSE
    )
  })

  out <- do.call(rbind, rows)
  class(out) <- c("nigeria_comparison", "data.frame")
  out
}


#' Detailed tax band breakdown
#'
#' Returns a tidy data frame showing how each band contributes to the
#' total PAYE. Works with any law in the registry.
#'
#' @inheritParams calc_paye
#' @return A data frame with per-band detail and cumulative tax.
#'
#' @examples
#' tax_breakdown(1624734)
#' tax_breakdown(1624734, law = "PITA")
#'
#' @export
tax_breakdown <- function(gross_monthly,
                          annual_rent   = 0,
                          include_nhf   = TRUE,
                          include_nhis  = FALSE,
                          law           = "NTA2025",
                          basic_monthly = NULL) {

  if (identical(law, "auto")) law <- get_applicable_law()
  tax  <- calc_paye(gross_monthly, annual_rent, include_nhf, include_nhis, law, basic_monthly)

  rows <- lapply(tax$band_detail, function(b) {
    data.frame(band = b$label, rate_pct = b$rate * 100,
               income_in_band = b$income_in_band, tax = b$tax,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$cumulative_tax <- cumsum(out$tax)
  out
}
