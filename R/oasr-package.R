#' @details
#' The main entry points are:
#' \itemize{
#'   \item \code{\link{simulate_rates}}: interface with dnsr for interest rate simulation
#'   \item \code{\link{prepayment_model}}: create prepayment model specifications
#'   \item \code{\link{calculate_credit_vectors}}: project prepayment, default and recovery rates
#'   \item \code{\link{project_cashflows}}: project cash flows under each rate path
#'   \item \code{\link{calculate_oas}}: solve for the option-adjusted spread
#'   \item \code{\link{run_oas_sensitivity}}: perform sensitivity analysis
#' }
#'
#' @importFrom stats sd
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

# ggplot2 aesthetics are evaluated inside the data mask, so R CMD check has no
# way to see where these names are bound. Declaring them here is the documented
# remedy rather than a suppression.
globalVariables(c(
  "amount", "component", "duration", "multiplier", "oas", "path",
  "price", "rate", "shift", "time", "time_point", "wal"
))
