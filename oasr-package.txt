#' @title Option-Adjusted Spread Analysis for Fixed Income Securities
#' @description A comprehensive toolkit for option-adjusted spread (OAS) analysis of fixed income securities with embedded options.
#' Provides functions for modeling prepayments, defaults, and interest rates to calculate accurate option-adjusted spreads.
#' Supports various prepayment and default models, including CPR (Conditional Prepayment Rate), CDR (Conditional Default Rate),
#' and time-to-event survival models. Leverages the 'dnsr' package for interest rate modeling and yield curve simulation.
#'
#' @section Key Functions:
#' \itemize{
#'   \item \code{\link{calculate_oas}}: Calculate option-adjusted spread
#'   \item \code{\link{prepayment_model}}: Create prepayment model specifications
#'   \item \code{\link{project_cashflows}}: Project cash flows under various scenarios
#'   \item \code{\link{simulate_rates}}: Interface with dnsr for interest rate simulation
#'   \item \code{\link{run_oas_sensitivity}}: Perform sensitivity analysis
#' }
#'
#' @docType package
#' @name oasR-package
"_PACKAGE"

# Ensure necessary packages are imported
#' @import dnsr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats predict quantile sd optimize
#' @importFrom parallel mclapply detectCores
#' @importFrom dplyr filter select mutate arrange
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom checkmate assert_data_frame assert_numeric assert_list assert_class
NULL
