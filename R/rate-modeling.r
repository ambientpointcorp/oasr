#' @title Simulate Interest Rate Paths for OAS Analysis
#' @description Interfaces with the dnsr package to simulate interest rate paths
#' for use in option-adjusted spread analysis.
#'
#' @param dns_params List. Model parameters from dnsr package, typically from
#'   \code{dnsr::get_calibrated_dns_params()}. Its \code{lambda} element is the
#'   Nelson-Siegel decay rate, in the same units as \code{maturities} -- that
#'   is, per year. Diebold and Li's conventional 0.0609 is per month, so the
#'   equivalent here is \code{0.0609 * 12 = 0.7308}. Passing the per-month
#'   value against a grid in years puts the curvature hump 29 years out,
#'   which leaves the slope and curvature loadings correlated -0.97 over a
#'   standard 3M-30Y grid; \code{simulate_rates()} warns when the loadings
#'   come out that close together. See \code{dnsr::ns_lambda()} to express a
#'   decay rate by the maturity at which the curvature loading peaks.
#' @param n_paths Integer. Number of simulation paths
#' @param n_periods Integer. Number of time periods (typically months)
#' @param maturities Numeric vector. Maturities (in years) for which to calculate yields
#' @param frequency Character. Time frequency, one of "monthly" (default), "quarterly", or "annual"
#' @param seed Integer. Random seed for reproducibility
#'
#' @return A list containing:
#'   \item{yield_paths}{A list of matrices, one for each maturity, with dimensions [path, time]}
#'   \item{factors}{The simulated DNS factors}
#'   \item{params}{The DNS parameters used for simulation}
#'   \item{maturities}{The maturities for which yields were calculated}
#'
#' @examples
#' \dontrun{
#' dns_params <- dnsr::get_calibrated_dns_params()
#' rate_paths <- simulate_rates(dns_params, n_paths = 100, n_periods = 360,
#'                              maturities = c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30))
#' }
#'
#' @export
simulate_rates <- function(dns_params, n_paths = 100, n_periods = 360,
                           maturities = c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30),
                           frequency = "monthly", seed = NULL) {

  # Parameter validation
  checkmate::assert_list(dns_params)
  checkmate::assert_int(n_paths, lower = 1)
  checkmate::assert_int(n_periods, lower = 1)
  checkmate::assert_numeric(maturities, lower = 0)
  checkmate::assert_choice(frequency, c("monthly", "quarterly", "annual"))

  # Calculate time horizon based on frequency
  time_factor <- switch(frequency,
                        monthly = 12,
                        quarterly = 4,
                        annual = 1)

  time_horizon <- n_periods / time_factor

  # FIX: Ensure params and initial_factors are correctly extracted from dns_params
  # Get parameters from dns_params
  if (!("params" %in% names(dns_params))) {
    # If dns_params doesn't have a params element, try to use it directly
    params <- dns_params
  } else {
    params <- dns_params$params
  }

  # Check if params is still NULL
  if (is.null(params)) {
    stop("Could not extract valid params from dns_params. Please check the structure of dns_params.")
  }

  # Ensure params has required elements
  required_elements <- c("kappa", "theta", "sigma")
  if (!all(required_elements %in% names(params))) {
    # Try to reconstruct params from other elements if possible
    if ("kappa1" %in% names(params)) {
      # Old format with individual parameters
      params <- list(
        kappa = c(params$kappa1, params$kappa2, params$kappa3),
        theta = c(params$theta1, params$theta2, params$theta3),
        sigma = c(params$sigma1, params$sigma2, params$sigma3)
      )
    } else {
      stop("params must contain elements: ", paste(required_elements, collapse = ", "))
    }
  }

  # FIX: Handle initial_factors similarly
  if (!("initial_factors" %in% names(dns_params))) {
    # Default values if not provided
    initial_factors <- c(0.035, -0.015, 0.005)
    warning("initial_factors not found in dns_params. Using default values: ",
            paste(initial_factors, collapse = ", "))
  } else {
    initial_factors <- dns_params$initial_factors
  }

  # Handle lambda similarly. The default is per year, as `maturities` is:
  # it places the curvature hump at 3.5 years, matching dnsr's own default.
  if (!("lambda" %in% names(dns_params))) {
    lambda <- dnsr::ns_lambda(3.5)
    warning("lambda not found in dns_params. Using default value: ",
            signif(lambda, 4), " per year (curvature hump at 3.5 years).")
  } else {
    lambda <- dns_params$lambda
  }
  checkmate::assert_number(lambda, lower = .Machine$double.eps, finite = TRUE)

  # Check that lambda resolves the slope and curvature loadings apart over the
  # grid being asked for. When those two loadings are nearly collinear the
  # factors contribute almost the same shape, so the yields depend on a
  # difference the loadings cannot express. The usual cause is a lambda given
  # in the wrong units -- a per-month value against a grid in years, or the
  # reverse -- which no range check on lambda itself would catch, since the
  # two conventions differ only by a factor of 12 and both land in the
  # plausible interval. Testing the loadings instead is free of any
  # assumption about units, and catches a genuinely narrow maturity grid too.
  if (length(maturities) >= 3) {
    slope_load <- (1 - exp(-lambda * maturities)) / (lambda * maturities)
    curve_load <- slope_load - exp(-lambda * maturities)
    collinearity <- suppressWarnings(stats::cor(slope_load, curve_load))
    if (is.finite(collinearity) && abs(collinearity) > 0.95) {
      warning("The Nelson-Siegel slope and curvature loadings correlate ",
              signif(collinearity, 3), " at lambda = ", signif(lambda, 4),
              " over the maturities requested, so the two factors are not ",
              "separately resolved and the simulated yields will reflect ",
              "that. lambda must be in the same units as `maturities`, ",
              "which are years: Diebold and Li's per-month 0.0609 is ",
              "0.7308 per year, and its curvature hump sits at ",
              signif(dnsr::ns_curvature_peak(lambda), 4), " here.")
    }
  }

  # Add names element to params if missing
  if (!("names" %in% names(params))) {
    params$names <- c("Level", "Slope", "Curvature")
  }

  # Add lambda to params if not present
  if (!("lambda" %in% names(params))) {
    params$lambda <- lambda
  }

  # Use dnsr to simulate factor paths
  sim_result <- dnsr::simulate_dns_factors(
    params = params,
    initial_factors = initial_factors,
    time_horizon = time_horizon,
    n_steps = n_periods,
    n_paths = n_paths,
    include_correlation = TRUE,
    seed = seed
  )

  # Use dnsr to calculate yield curves
  yield_curves <- dnsr::calculate_dns_yields(
    factors = sim_result$factors,
    maturities = maturities,
    lambda = lambda
  )

  # Reformat the yield paths to be more convenient for OAS analysis
  # We want a list where each element is a matrix for a specific maturity
  # with dimensions [path, time]
  yield_paths <- list()

  for (m_idx in seq_along(maturities)) {
    maturity <- maturities[m_idx]
    maturity_key <- as.character(maturity)

    # Extract yields for this maturity across all paths and time points
    # and transpose to get [path, time] format
    yield_paths[[maturity_key]] <- t(yield_curves[, m_idx, ])
  }

  return(list(
    yield_paths = yield_paths,
    factors = sim_result$factors,
    params = dns_params,
    maturities = maturities
  ))
}

#' @title Extract Reference Rates from Simulated Rate Paths
#' @description Extracts a specific reference rate (e.g., 1-year, 10-year) from
#' the simulated rate paths for use in cash flow projection.
#'
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param reference_key Character. The key for the reference rate to extract (e.g., "10" for 10-year)
#'
#' @return A matrix of reference rates with dimensions [path, time]
#'
#' @examples
#' \dontrun{
#' rate_paths <- simulate_rates(dns_params, n_paths = 100)
#' reference_rates <- extract_reference_rates(rate_paths, "10")
#' }
#'
#' @export
extract_reference_rates <- function(rate_paths, reference_key = "10") {
  # Parameter validation
  checkmate::assert_list(rate_paths)

  # Check if the requested reference rate exists
  if (!reference_key %in% names(rate_paths$yield_paths)) {
    stop("Reference rate key '", reference_key, "' not found in rate paths. ",
         "Available keys: ", paste(names(rate_paths$yield_paths), collapse = ", "))
  }

  # Extract the reference rate matrix
  reference_rates <- rate_paths$yield_paths[[reference_key]]

  return(reference_rates)
}

#' @title Get Discount Factors from Rate Paths
#' @description Calculates discount factors for each path and time point based on
#' a reference rate plus a spread.
#'
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param reference_key Character. The key for the reference rate to use
#' @param spread Numeric. Spread to add to the reference rate (in decimal form)
#' @param times Numeric vector. Time points (in years) for which to calculate discount factors
#'
#' @return A list with one matrix per path, each containing discount factors for the specified times
#'
#' @examples
#' \dontrun{
#' rate_paths <- simulate_rates(dns_params, n_paths = 100)
#' times <- seq(1/12, 30, by = 1/12)
#' discount_factors <- get_discount_factors(rate_paths, "10", 0.01, times)
#' }
#'
#' @export
get_discount_factors <- function(rate_paths, reference_key = "10", spread = 0, times = NULL) {
  # Extract reference rates
  reference_rates <- extract_reference_rates(rate_paths, reference_key)

  # Get dimensions
  n_paths <- nrow(reference_rates)
  n_periods <- ncol(reference_rates)

  # If times not provided, use default sequence
  if (is.null(times)) {
    times <- seq(1/12, n_periods/12, by = 1/12)
  }

  # Parameter validation
  checkmate::assert_numeric(times, lower = 0)
  checkmate::assert_numeric(spread)

  # Initialize list to store discount factors
  discount_factors <- vector("list", n_paths)

  # Calculate discount factors for each path
  for (i in 1:n_paths) {
    # Get rates for this path (with spread)
    rates <- reference_rates[i, ] + spread

    # Calculate discount factors: exp(-r * t)
    # For each time point, we need to use the appropriate rate
    # We'll use the rate from the closest period
    df <- numeric(length(times))

    for (t in seq_along(times)) {
      # Convert time to period index
      period_idx <- min(round(times[t] * 12), n_periods)

      # Calculate discount factor
      df[t] <- exp(-rates[period_idx] * times[t])
    }

    discount_factors[[i]] <- df
  }

  return(list(
    discount_factors = discount_factors,
    times = times,
    reference_rates = reference_rates,
    spread = spread
  ))
}
