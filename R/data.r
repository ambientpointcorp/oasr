#' Sample Loan-Level Data for OAS Analysis
#'
#' A loan-level dataset of twelve fixed-rate mortgages, for demonstration and
#' testing. Every column is an attribute of an individual loan, so the frame can
#' be passed straight to \code{\link{project_cashflows}} and
#' \code{\link{calculate_oas}} with no preparation.
#'
#' \code{balance} is derived by amortising \code{original_balance} over
#' \code{age} months at \code{wac}, so \code{original_balance}, \code{balance},
#' \code{term} and \code{age} are mutually consistent. The loans span the
#' kinks in the default model -- four have \code{ltv} above 0.80 and one has
#' \code{fico} below 620 -- and every remaining term (\code{term - age}) is at
#' most 354 months, so a 360-month projection captures the pool in full.
#'
#' @format A data frame with 12 rows and 14 variables:
#' \describe{
#'   \item{asset_id}{Loan identifier}
#'   \item{original_balance}{Balance at origination (dollars)}
#'   \item{balance}{Current balance (dollars), amortised from origination}
#'   \item{wac}{Note rate (annual, decimal)}
#'   \item{servicing}{Servicing fee (annual, decimal)}
#'   \item{io_strip}{Interest-only strip (annual, decimal)}
#'   \item{accrual_method}{Day-count convention for interest accrual. All
#'     twelve loans are \code{"30/360"}, the convention the projection engine
#'     applies today}
#'   \item{term}{Original amortisation term (months)}
#'   \item{age}{Months elapsed since origination}
#'   \item{ltv}{Loan-to-value ratio (decimal)}
#'   \item{fico}{Borrower FICO score}
#'   \item{state}{State of the property}
#'   \item{property_type}{SFR, CONDO or MULTI}
#'   \item{occupancy_type}{PRIMARY, INVESTMENT or SECOND_HOME}
#' }
#'
#' @examples
#' data(sample_abs_data)
#' head(sample_abs_data)
#'
#' # balance is the amortised balance, not a free parameter
#' with(sample_abs_data, all(balance <= original_balance))
"sample_abs_data"

#' Create Sample Loan-Level Data
#'
#' Internal generator for \code{\link{sample_abs_data}}. Kept in the package so
#' the dataset can be regenerated and so the amortisation that produces
#' \code{balance} is on the record.
#'
#' @return A data frame of twelve loans.
#'
#' @keywords internal
create_sample_abs_data <- function() {

  # Balance remaining after `age` payments on a level-payment loan.
  amortised_balance <- function(original_balance, rate, term, age) {
    i <- rate / 12
    if (i <= 0) {
      return(original_balance * (1 - age / term))
    }
    original_balance * ((1 + i)^term - (1 + i)^age) / ((1 + i)^term - 1)
  }

  sample_abs_data <- data.frame(
    asset_id = sprintf("L%03d", 1:12),
    original_balance = c(350000, 198000, 425000, 150000, 275000, 520000,
                         310000, 240000, 185000, 465000, 295000, 215000),
    wac = c(0.0625, 0.0550, 0.0712, 0.0488, 0.0675, 0.0425,
            0.0750, 0.0588, 0.0638, 0.0525, 0.0600, 0.0562),
    servicing = c(0.0025, 0.0025, 0.0030, 0.0025, 0.0030, 0.0025,
                  0.0035, 0.0025, 0.0025, 0.0025, 0.0030, 0.0025),
    io_strip = c(0.0010, 0.0010, 0.0015, 0.0010, 0.0015, 0.0010,
                 0.0020, 0.0010, 0.0010, 0.0010, 0.0015, 0.0010),
    accrual_method = c("30/360", "30/360", "30/360", "30/360", "30/360", "30/360",
                       "30/360", "30/360", "30/360", "30/360", "30/360", "30/360"),
    term = c(360, 360, 360, 180, 360, 360, 360, 240, 360, 360, 360, 360),
    age = c(24, 48, 12, 36, 60, 96, 6, 72, 120, 18, 30, 84),
    ltv = c(0.80, 0.72, 0.90, 0.65, 0.85, 0.58,
            0.95, 0.70, 0.62, 0.78, 0.82, 0.68),
    fico = c(742, 768, 668, 790, 705, 781, 612, 729, 755, 773, 698, 736),
    state = c("CA", "NY", "FL", "TX", "IL", "WA",
              "NV", "OH", "PA", "MA", "AZ", "GA"),
    property_type = c("SFR", "SFR", "CONDO", "SFR", "MULTI", "SFR",
                      "SFR", "SFR", "CONDO", "SFR", "SFR", "MULTI"),
    occupancy_type = c("PRIMARY", "PRIMARY", "INVESTMENT", "PRIMARY",
                       "INVESTMENT", "PRIMARY", "PRIMARY", "PRIMARY",
                       "SECOND_HOME", "PRIMARY", "INVESTMENT", "INVESTMENT"),
    stringsAsFactors = FALSE
  )

  sample_abs_data$balance <- round(
    mapply(amortised_balance,
           sample_abs_data$original_balance,
           sample_abs_data$wac,
           sample_abs_data$term,
           sample_abs_data$age),
    2
  )

  # Put balance next to the original it was derived from.
  sample_abs_data[, c("asset_id", "original_balance", "balance", "wac",
                      "servicing", "io_strip", "accrual_method", "term",
                      "age", "ltv", "fico", "state", "property_type",
                      "occupancy_type")]
}

#' Sample Structured Deal for OAS Analysis
#'
#' A three-tranche sequential-pay deal sized to the pool in
#' \code{\link{sample_abs_data}}: the bond balances sum to the pool's current
#' balance, so the structure can be passed to \code{\link{aggregate_cashflows}}
#' or to \code{\link{calculate_oas}} as \code{deal_structure} without rescaling.
#'
#' @format A list with two components:
#' \describe{
#'   \item{bonds}{A data frame of bond_id, balance and coupon, senior first}
#'   \item{waterfall}{A list of the principal, interest and loss rules}
#' }
#'
#' @examples
#' data(sample_deal_structure)
#' str(sample_deal_structure)
#'
#' # the tranches are sized to the pool
#' data(sample_abs_data)
#' sum(sample_deal_structure$bonds$balance) == round(sum(sample_abs_data$balance), 2)
"sample_deal_structure"

#' Create Sample Deal Structure
#'
#' Internal generator for \code{\link{sample_deal_structure}}. Sizes the
#' tranches off \code{\link{create_sample_abs_data}} so the two datasets cannot
#' drift apart.
#'
#' @return A list with bonds and waterfall components.
#'
#' @keywords internal
create_sample_deal_structure <- function() {

  pool_balance <- round(sum(create_sample_abs_data()$balance), 2)

  # 77 / 15 / 8 senior-mezzanine-subordinate; the sub absorbs the rounding so
  # the tranches tie to the pool exactly.
  senior <- round(pool_balance * 0.77, 2)
  mezzanine <- round(pool_balance * 0.15, 2)
  subordinate <- round(pool_balance - senior - mezzanine, 2)

  bonds <- data.frame(
    bond_id = c("A", "B", "C"),
    balance = c(senior, mezzanine, subordinate),
    coupon = c(0.045, 0.065, 0.085),
    stringsAsFactors = FALSE
  )

  waterfall <- list(
    principal = "sequential",
    interest = "sequential",
    losses = "reverse_sequential"
  )

  list(
    bonds = bonds,
    waterfall = waterfall
  )
}
