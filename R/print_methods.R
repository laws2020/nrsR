#' Format a number as Nigerian Naira
#'
#' @param x Numeric value.
#' @param prefix Character. Currency prefix. Default \code{"₦"}.
#' @param digits Integer. Decimal places. Default \code{2}.
#' @return A formatted character string.
#' @examples
#' format_naira(1624734)
#' @export
format_naira <- function(x, prefix = "\u20a6", digits = 2) {
  formatted <- formatC(round(x, digits), format = "f", digits = digits,
                       big.mark = ",", drop0trailing = FALSE)
  paste0(prefix, formatted)
}

.divider <- function(n = 58, char = "=") cat(rep(char, n), "\n", sep = "")
.dline   <- function(n = 58)           cat(rep("-", n),  "\n", sep = "")
.row     <- function(label, value, w = 35) cat(sprintf(" %-*s %s\n", w, label, value))

#' @export
print.nigeria_tax <- function(x, ...) {
  cat("\n"); .divider()
  cat(" NIGERIAN PAYE TAX COMPUTATION\n")
  cat(" Law:", x$law_description, "\n"); .divider()

  rel <- x$reliefs
  .row("Monthly Gross:",    format_naira(x$gross_monthly))
  .row("Annual Gross:",     format_naira(x$gross_annual))
  .dline()
  cat(" DEDUCTIONS & RELIEFS\n")
  .row("  Pension (8%/month):", format_naira(rel$pension_monthly))
  if (rel$nhf_monthly  > 0) .row("  NHF (2.5%/month):",   format_naira(rel$nhf_monthly))
  if (rel$nhis_monthly > 0) .row("  NHIS (5%/month):",     format_naira(rel$nhis_monthly))
  if (rel$cra_annual   > 0) .row("  CRA (annual):",        format_naira(rel$cra_annual))
  if (rel$rent_relief_annual > 0)
    .row("  Rent Relief (annual):", format_naira(rel$rent_relief_annual))
  .row("  Total Annual Relief:",   format_naira(rel$total_relief_annual))
  .dline()
  .row("Annual Taxable Income:", format_naira(x$taxable_income_annual))
  .dline()
  cat(" TAX BANDS\n")
  for (b in x$band_detail) {
    if (b$income_in_band > 0)
      cat(sprintf("   %-28s %s  =>  %s\n",
                  b$label, format_naira(b$income_in_band), format_naira(b$tax)))
  }
  .dline()
  .row("ANNUAL TAX:",  format_naira(x$annual_tax))
  .row("MONTHLY TAX:", format_naira(x$monthly_tax))
  .row("Effective Rate (on gross):", sprintf("%.2f%%", x$effective_rate * 100))
  .divider(); cat("\n")
  invisible(x)
}

#' @export
print.nigeria_net <- function(x, ...) {
  cat("\n"); .divider(52)
  cat(" MONTHLY PAYSLIP SUMMARY\n"); .divider(52)
  .row("Gross Income:", format_naira(x$gross_monthly), 30)
  .dline(52)
  cat(" DEDUCTIONS\n")
  .row("  Pension:",  format_naira(x$pension_monthly),  30)
  if (x$nhf_monthly  > 0) .row("  NHF:",  format_naira(x$nhf_monthly),  30)
  if (x$nhis_monthly > 0) .row("  NHIS:", format_naira(x$nhis_monthly), 30)
  .row("  PAYE Tax:", format_naira(x$paye_monthly), 30)
  .dline(52)
  .row("Total Deductions:", format_naira(x$total_deductions_monthly), 30)
  .divider(52)
  .row("NET TAKE-HOME PAY:", format_naira(x$net_monthly), 30)
  .divider(52); cat("\n")
  invisible(x)
}

#' @export
print.nigeria_comparison <- function(x, ...) {
  cat("\n"); .divider(70)
  cat(" TAX LAW COMPARISON\n"); .divider(70)

  metrics <- list(
    list("Annual Relief",         "total_relief_annual",   FALSE),
    list("Taxable Income (Ann.)", "taxable_income_annual", FALSE),
    list("Annual Tax",            "annual_tax",            FALSE),
    list("Monthly Tax",           "monthly_tax",           FALSE),
    list("Effective Rate (%)",    "effective_rate_pct",    TRUE),
    list("Net Monthly Pay",       "net_monthly",           FALSE)
  )

  # Header
  header <- sprintf(" %-26s", "Metric")
  for (lk in x$law) header <- paste0(header, sprintf("  %-18s", lk))
  cat(header, "\n"); .dline(70)

  for (m in metrics) {
    line <- sprintf(" %-26s", m[[1]])
    for (v in x[[m[[2]]]]) {
      fmt <- if (m[[3]]) sprintf("%.2f%%", v) else format_naira(v)
      line <- paste0(line, sprintf("  %-18s", fmt))
    }
    cat(line, "\n")
  }
  .divider(70); cat("\n")
  invisible(x)
}

#' @export
summary.nigeria_tax <- function(object, ...) {
  cat("nrsr | Law:", object$law, "| Gross:", format_naira(object$gross_monthly),
      "| Tax/month:", format_naira(object$monthly_tax),
      "| Eff. rate:", sprintf("%.2f%%\n", object$effective_rate * 100))
  invisible(object)
}

