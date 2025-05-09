#' @title Simulate Interest Rate Paths for OAS Analysis
#' @description Interfaces with the dnsr package to simulate interest rate paths
#' for use in option-adjusted spread analysis.
#'
#' @param dns_params List. Model parameters from dnsr package, typically from
#'   \code{dnsr::get_calibrated_dns_params()}
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
  
  # Get parameters from dns_params
  params <- dns_params$params
  initial_factors <- dns_params$initial_factors
  lambda <- dns_params$lambda
  
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
