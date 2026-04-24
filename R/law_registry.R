#' nrsr: Nigeria Revenue Service R Toolkit
#'
#' PAYE tax computation for Nigeria, driven by a versioned law registry.
#' Named after the Nigeria Revenue Service (NRS), which replaced FIRS
#' under the 2025 tax reforms.
#'
#' @keywords internal
"_PACKAGE"

# ============================================================
#  LAW REGISTRY — The Single Source of Truth
#
#  HOW TO ADD A FUTURE AMENDMENT:
#  1. Add ONE new entry to .TAX_LAW_REGISTRY below.
#  2. Done. No other file needs to change.
#
#  Each entry contains:
#    - bands           : progressive tax bands (band_width, rate, label)
#    - deductions      : statutory rates (pension, nhf, nhis)
#    - relief_rules    : which reliefs apply and their parameters
#    - effective_from  : Date from which this law applies
#    - effective_to    : Date until which this law applies (NA = current)
#    - description     : Human-readable description
# ============================================================

.TAX_LAW_REGISTRY <- list(

  # ----------------------------------------------------------
  # PITA — Personal Income Tax Act (legacy, up to Dec 2025)
  # ----------------------------------------------------------
  PITA = list(
    key            = "PITA",
    description    = "Personal Income Tax Act (legacy)",
    effective_from = as.Date("1993-01-01"),
    effective_to   = as.Date("2025-12-31"),

    bands = data.frame(
      band_width = c(300000,  300000,  500000,  500000,  1600000,  Inf),
      rate       = c(0.07,    0.11,    0.15,    0.19,    0.21,     0.24),
      label      = c("First \u20a6300K @7%",  "Next \u20a6300K @11%",
                     "Next \u20a6500K @15%",  "Next \u20a6500K @19%",
                     "Next \u20a61.6M @21%",  "Above \u20a63.2M @24%"),
      stringsAsFactors = FALSE
    ),

    deductions = list(
      pension_employee_rate = 0.08,
      pension_employer_rate = 0.10,
      nhf_rate              = 0.025,
      nhis_rate             = 0.05
    ),

    relief_rules = list(
      cra_fixed          = 200000,   # Fixed annual CRA
      cra_variable_rate  = 0.20,     # 20% of annual gross
      rent_relief_rate   = NULL,     # Not applicable
      rent_relief_max    = NULL,     # Not applicable
      tax_free_threshold = 0         # No zero-rate threshold
    )
  ),

  # ----------------------------------------------------------
  # NTA2025 — Nigeria Tax Act 2025 (effective Jan 2026)
  # ----------------------------------------------------------
  NTA2025 = list(
    key            = "NTA2025",
    description    = "Nigeria Tax Act 2025 (effective January 2026)",
    effective_from = as.Date("2026-01-01"),
    effective_to   = NA,             # Current law — no end date yet

    bands = data.frame(
      band_width = c(800000,  2200000,  9000000,  13000000,  25000000,  Inf),
      rate       = c(0.00,    0.15,     0.18,     0.21,      0.23,      0.25),
      label      = c("First \u20a6800K @0%",  "Next \u20a62.2M @15%",
                     "Next \u20a69M @18%",    "Next \u20a613M @21%",
                     "Next \u20a625M @23%",   "Above \u20a650M @25%"),
      stringsAsFactors = FALSE
    ),

    deductions = list(
      pension_employee_rate = 0.08,
      pension_employer_rate = 0.10,
      nhf_rate              = 0.025,
      nhis_rate             = 0.05
    ),

    relief_rules = list(
      cra_fixed          = NULL,     # CRA abolished
      cra_variable_rate  = NULL,     # CRA abolished
      rent_relief_rate   = 0.20,     # 20% of annual rent
      rent_relief_max    = 500000,   # Capped at ₦500,000/year
      tax_free_threshold = 800000    # First ₦800K is tax-free
    )
  )

  # ----------------------------------------------------------
  # TEMPLATE: How to add a future amendment — e.g., NTA2028
  #
  # NTA2028 = list(
  #   key            = "NTA2028",
  #   description    = "Nigeria Tax Amendment Act 2028",
  #   effective_from = as.Date("2028-01-01"),
  #   effective_to   = NA,
  #
  #   bands = data.frame(
  #     band_width = c(...),   # <-- just update these numbers
  #     rate       = c(...),
  #     label      = c(...),
  #     stringsAsFactors = FALSE
  #   ),
  #
  #   deductions = list(
  #     pension_employee_rate = 0.08,   # update if changed
  #     pension_employer_rate = 0.10,
  #     nhf_rate              = 0.025,
  #     nhis_rate             = 0.05
  #   ),
  #
  #   relief_rules = list(
  #     cra_fixed          = NULL,
  #     cra_variable_rate  = NULL,
  #     rent_relief_rate   = 0.20,     # update if changed
  #     rent_relief_max    = 700000,   # e.g., cap was raised
  #     tax_free_threshold = 1200000   # e.g., threshold was raised
  #   )
  # )
  # ----------------------------------------------------------
)


# ============================================================
#  REGISTRY ACCESS HELPERS
# ============================================================

#' List all registered tax laws
#'
#' Returns a summary data frame of every tax law version in the registry,
#' including its key, description, and effective date range.
#'
#' @return A data frame with columns: \code{key}, \code{description},
#'   \code{effective_from}, \code{effective_to}.
#'
#' @examples
#' list_tax_laws()
#'
#' @export
list_tax_laws <- function() {
  rows <- lapply(.TAX_LAW_REGISTRY, function(law) {
    data.frame(
      key            = law$key,
      description    = law$description,
      effective_from = as.character(law$effective_from),
      effective_to   = ifelse(is.na(law$effective_to), "current", as.character(law$effective_to)),
      n_bands        = nrow(law$bands),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Get the tax law applicable on a given date
#'
#' Looks up the registry and returns the law key whose effective date
#' range covers \code{as_of}. Useful for automatic date-based dispatch.
#'
#' @param as_of A \code{Date} or character string (e.g. \code{"2026-03-01"}).
#'   Defaults to today's date.
#'
#' @return Character string — the law key (e.g. \code{"NTA2025"}).
#'
#' @examples
#' get_applicable_law("2025-06-01")   # returns "PITA"
#' get_applicable_law("2026-03-01")   # returns "NTA2025"
#' get_applicable_law()               # returns law for today
#'
#' @export
get_applicable_law <- function(as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  for (law in .TAX_LAW_REGISTRY) {
    end <- if (is.na(law$effective_to)) as.Date("9999-12-31") else law$effective_to
    if (as_of >= law$effective_from && as_of <= end) {
      return(law$key)
    }
  }
  rlang::abort(paste0("No tax law found for date: ", as_of,
                      ". Available laws: ", paste(names(.TAX_LAW_REGISTRY), collapse = ", ")))
}


#' Retrieve a tax law definition from the registry
#'
#' @param law Character. Law key (e.g. \code{"NTA2025"}, \code{"PITA"}).
#'   Use \code{list_tax_laws()} to see all available keys.
#'
#' @return The full law definition list.
#' @keywords internal
.get_law <- function(law) {
  if (!law %in% names(.TAX_LAW_REGISTRY)) {
    rlang::abort(paste0(
      "Unknown tax law: '", law, "'. ",
      "Available: ", paste(names(.TAX_LAW_REGISTRY), collapse = ", "), ". ",
      "Use list_tax_laws() to see all registered laws."
    ))
  }
  .TAX_LAW_REGISTRY[[law]]
}
