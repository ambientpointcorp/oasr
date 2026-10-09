# Day-count handling. The whole design turns on one point: for a monthly-pay
# amortising loan, 30/360 means exactly one twelfth per period by construction.
# Derived from a calendar it is not even right -- an end-of-month anchor gives
# US 30/360 counts of 28, 33 and 30 rather than a flat 30 -- and computing it
# that way would also perturb every existing result, because `wac / 12`,
# `wac * (1/12)` and `wac * 30 / 360` are not bit-identical in IEEE arithmetic.
# So the 30/360 branch returns the legacy expression and never touches a date.

.ACCRUAL_SYNONYMS <- c(
  "30/360"     = "30/360",
  "30E/360"    = "30/360",
  "30/360 US"  = "30/360",
  "BOND"       = "30/360",
  "ACT/365"    = "ACT/365",
  "ACTUAL/365" = "ACT/365",
  "A/365"      = "ACT/365",
  "ACT/365F"   = "ACT/365",
  "ACT/360"    = "ACT/360",
  "ACTUAL/360" = "ACT/360",
  "A/360"      = "ACT/360"
)

#' @keywords internal
#' @noRd
.normalise_accrual <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("30/360")
  }
  key <- toupper(trimws(as.character(x)))
  out <- .ACCRUAL_SYNONYMS[key]
  if (is.na(out)) {
    stop("Unsupported accrual_method: '", x, "'. Supported: ",
         paste(unique(.ACCRUAL_SYNONYMS), collapse = ", "))
  }
  unname(out)
}

#' @keywords internal
#' @noRd
.add_months <- function(d, n) {
  lt <- as.POSIXlt(d)
  day0 <- lt$mday

  # Month arithmetic done on a month counter, so it vectorises over n and
  # cannot overflow into the following month the way date addition does.
  total <- (lt$year + 1900L) * 12L + lt$mon + n
  yr <- total %/% 12L
  mo <- total %% 12L

  first <- as.Date(sprintf("%04d-%02d-01", yr, mo + 1L))
  next_first <- as.Date(sprintf("%04d-%02d-01",
                                ifelse(mo == 11L, yr + 1L, yr),
                                ifelse(mo == 11L, 1L, mo + 2L)))
  last_day <- as.integer(next_first - first)

  # An end-of-month anchor stays at end of month rather than spilling over.
  first + (pmin(day0, last_day) - 1L)
}

#' @keywords internal
#' @noRd
.accrual_column <- function(asset_data) {
  if ("accrual_method" %in% names(asset_data)) {
    "accrual_method"
  } else if ("Accrual" %in% names(asset_data)) {
    # The spelling the surrounding suite already carries on its loan tapes.
    "Accrual"
  } else {
    NA_character_
  }
}

#' @keywords internal
#' @noRd
.period_rate <- function(annual_rate, method, start_date, n_periods) {
  if (method == "30/360" || is.null(start_date) || is.na(start_date)) {
    # The legacy expression, reproduced exactly.
    return(rep(annual_rate / 12, n_periods))
  }

  dates <- .add_months(start_date, 0:n_periods)
  days <- as.numeric(diff(dates))
  basis <- if (method == "ACT/360") 360 else 365
  annual_rate * days / basis
}

#' @keywords internal
#' @noRd
.needs_anchor <- function(method, start_date) {
  method != "30/360" && (is.null(start_date) || is.na(start_date))
}
