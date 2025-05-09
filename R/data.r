#' Sample ABS Data for OAS Analysis
#'
#' A dataset containing sample asset-backed securities (ABS) for demonstration
#' and testing purposes.
#'
#' @format A data frame with 5 rows and 7 variables:
#' \describe{
#'   \item{asset_id}{Asset identifier}
#'   \item{balance}{Current balance of the asset (in dollars)}
#'   \item{wac}{Weighted average coupon rate (annual, in decimal)}
#'   \item{wam}{Weighted average maturity (in months)}
#'   \item{age}{Current age of the asset (in months)}
#'   \item{ltv}{Loan-to-value ratio (in decimal)}
#'   \item{fico}{FICO credit score of the borrower}
#'   \item{state}{State where the property is located}
#'   \item{property_type}{Type of property (SFR = Single Family Residence, etc.)}
#'   \item{occupancy_type}{Occupancy type (PRIMARY, INVESTMENT, etc.)}
#'   \item{market_price}{Market price as a percentage of face value}
#' }
#'
#' @examples
#' data(sample_abs_data)
#' head(sample_abs_data)
"sample_abs_data"

#' Create Sample ABS Data
#'
#' Internal function to create sample ABS data for the package
#'
#' @keywords internal
create_sample_abs_data <- function() {
  # Create sample data
  sample_abs_data <- data.frame(
    asset_id = c("A001", "A002", "A003", "A004", "A005"),
    balance = c(200000, 150000, 300000, 250000, 180000),
    wac = c(0.055, 0.048, 0.062, 0.053, 0.047),
    wam = c(300, 280, 320, 260, 240),
    age = c(60, 40, 20, 100, 120),
    ltv = c(0.75, 0.68, 0.82, 0.72, 0.65),
    fico = c(720, 760, 680, 710, 750),
    state = c("CA", "NY", "FL", "TX", "IL"),
    property_type = c("SFR", "SFR", "CONDO", "SFR", "MULTI"),
    occupancy_type = c("PRIMARY", "PRIMARY", "INVESTMENT", "PRIMARY", "INVESTMENT"),
    market_price = c(102.5, 101.2, 98.5, 100.8, 103.2)
  )

  return(sample_abs_data)
}

#' Sample Structured Deal Data for OAS Analysis
#'
#' A dataset containing a sample structured deal for demonstration and testing purposes.
#'
#' @format A list with two components:
#' \describe{
#'   \item{bonds}{A data frame with bond information}
#'   \item{waterfall}{A list with waterfall rules}
#' }
#'
#' @examples
#' data(sample_deal_structure)
#' str(sample_deal_structure)
"sample_deal_structure"

#' Create Sample Deal Structure
#'
#' Internal function to create sample deal structure for the package
#'
#' @keywords internal
create_sample_deal_structure <- function() {
  # Create sample bond data
  bonds <- data.frame(
    bond_id = c("A", "B", "C"),
    balance = c(800000, 200000, 80000),
    coupon = c(0.04, 0.06, 0.08)
  )

  # Create waterfall rules
  waterfall <- list(
    principal = "sequential",
    interest = "sequential",
    losses = "reverse_sequential"
  )

  # Combine into deal structure
  sample_deal_structure <- list(
    bonds = bonds,
    waterfall = waterfall
  )

  return(sample_deal_structure)
}
