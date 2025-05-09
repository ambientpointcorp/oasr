#' @title Calculate Present Value of Cash Flows
#' @description Calculates the present value of cash flows using a reference rate plus a spread.
#'
#' @param cashflows List. The result from \code{\link{project_cashflows}} or \code{\link{aggregate_cashflows}}
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param oas Numeric. Option-adjusted spread in basis points
#' @param reference_key Character. Key for the reference rate to use
#' @param asset_level Logical. Whether the cashflows are at the asset level or bond level
#' @param total_idx Integer. Index of the total cash flow component in cashflows
#'
#' @return A list containing:
#'   \item{present_values}{Matrix of present values for each path and asset/bond}
#'   \item{mean_pvs}{Vector of mean present values for each asset/bond}
#'   \item{pv_stats}{Data frame with statistics about present values}
#'
#' @export
calculate_present_value <- function(cashflows, rate_paths, oas, reference_key = "10",
                                   asset_level = TRUE, total_idx = 8) {
  
  # Parameter validation
  checkmate::assert_list(cashflows)
  checkmate::assert_list(rate_paths)
  checkmate::assert_numeric(oas)
  
  # Extract reference rates
  reference_rates <- extract_reference_rates(rate_paths, reference_key)
  
  # Get dimensions
  n_paths <- length(cashflows)
  n_periods <- dim(cashflows[[1]])[1]
  n_items <- dim(cashflows[[1]])[3]  # Assets or bonds
  
  # Convert OAS from basis points to decimal
  oas_decimal <- oas / 10000
  
  # Initialize matrix for present values (paths x items)
  present_values <- matrix(0, n_paths, n_items)
  
  # Calculate present value for each path and asset/bond
  for (i in 1:n_paths) {
    cf_array <- cashflows[[i]]
    
    # Process each asset/bond
    for (j in 1:n_items) {
      # Extract cash flows for this item
      if (asset_level) {
        # Asset-level cash flows
        cf <- cf_array[, , j]
      } else {
        # Bond-level cash flows
        cf <- cf_array[, , j]
      }
      
      # Find periods with non-zero total cash flows
      total_cf <- cf[, total_idx]
      non_zero_idx <- which(total_cf > 0)
      
      if (length(non_zero_idx) > 0) {
        # Times (in years)
        times <- non_zero_idx / 12
        
        # Discount rates: yield + OAS
        rates <- reference_rates[i, non_zero_idx] + oas_decimal
        
        # Calculate discount factors
        discount_factors <- exp(-rates * times)
        
        # Calculate present value for this asset/bond
        present_values[i, j] <- sum(total_cf[non_zero_idx] * discount_factors)
      }
    }
  }
  
  # Calculate statistics
  mean_pvs <- colMeans(present_values)
  sd_pvs <- apply(present_values, 2, sd)
  min_pvs <- apply(present_values, 2, min)
  max_pvs <- apply(present_values, 2, max)
  
  # Combine statistics into a data frame
  pv_stats <- data.frame(
    item = 1:n_items,
    mean_pv = mean_pvs,
    sd_pv = sd_pvs,
    min_pv = min_pvs,
    max_pv = max_pvs,
    cv_pv = sd_pvs / mean_pvs  # Coefficient of variation
  )
  
  return(list(
    present_values = present_values,
    mean_pvs = mean_pvs,
    pv_stats = pv_stats
  ))
}

#' @title Calculate Effective Duration and Convexity
#' @description Calculates the effective duration and convexity of cash flows
#' by shocking the interest rates.
#'
#' @param cashflows List. The result from \code{\link{project_cashflows}} or \code{\link{aggregate_cashflows}}
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param oas Numeric. Option-adjusted spread in basis points
#' @param shock Numeric. Interest rate shock in basis points
#' @param reference_key Character. Key for the reference rate to use
#' @param asset_level Logical. Whether the cashflows are at the asset level or bond level
#' @param total_idx Integer. Index of the total cash flow component in cashflows
#'
#' @return A list containing:
#'   \item{duration}{Vector of effective durations for each asset/bond}
#'   \item{convexity}{Vector of effective convexities for each asset/bond}
#'   \item{price_up}{Vector of prices with upward rate shock}
#'   \item{price_base}{Vector of prices with base rates}
#'   \item{price_down}{Vector of prices with downward rate shock}
#'
#' @export
calculate_duration_convexity <- function(cashflows, rate_paths, oas, shock = 100,
                                        reference_key = "10", asset_level = TRUE,
                                        total_idx = 8) {
  
  # Parameter validation
  checkmate::assert_list(cashflows)
  checkmate::assert_list(rate_paths)
  checkmate::assert_numeric(oas)
  checkmate::assert_numeric(shock, lower = 0)
  
  # Get dimensions
  n_items <- dim(cashflows[[1]])[3]  # Assets or bonds
  
  # Calculate base prices
  base_result <- calculate_present_value(
    cashflows = cashflows,
    rate_paths = rate_paths,
    oas = oas,
    reference_key = reference_key,
    asset_level = asset_level,
    total_idx = total_idx
  )
  
  # Create shocked rate paths (up)
  yield_paths_up <- rate_paths
  for (key in names(rate_paths$yield_paths)) {
    yield_paths_up$yield_paths[[key]] <- rate_paths$yield_paths[[key]] + shock/10000
  }
  
  # Create shocked rate paths (down)
  yield_paths_down <- rate_paths
  for (key in names(rate_paths$yield_paths)) {
    yield_paths_down$yield_paths[[key]] <- rate_paths$yield_paths[[key]] - shock/10000
  }
  
  # Calculate prices with shocked rates
  up_result <- calculate_present_value(
    cashflows = cashflows,
    rate_paths = yield_paths_up,
    oas = oas,
    reference_key = reference_key,
    asset_level = asset_level,
    total_idx = total_idx
  )
  
  down_result <- calculate_present_value(
    cashflows = cashflows,
    rate_paths = yield_paths_down,
    oas = oas,
    reference_key = reference_key,
    asset_level = asset_level,
    total_idx = total_idx
  )
  
  # Calculate effective duration for each asset/bond
  # Duration = -1 * (Price_down - Price_up) / (2 * Price_base * shock)
  duration <- -1 * (down_result$mean_pvs - up_result$mean_pvs) / 
    (2 * base_result$mean_pvs * (shock/10000))
  
  # Calculate effective convexity for each asset/bond
  # Convexity = (Price_up + Price_down - 2 * Price_base) / (Price_base * shock^2)
  convexity <- (up_result$mean_pvs + down_result$mean_pvs - 2 * base_result$mean_pvs) / 
    (base_result$mean_pvs * (shock/10000)^2)
  
  return(list(
    duration = duration,
    convexity = convexity,
    price_up = up_result$mean_pvs,
    price_base = base_result$mean_pvs,
    price_down = down_result$mean_pvs
  ))
}

#' @title Calculate Z-Spread
#' @description Calculates the Z-spread (zero-volatility spread) for fixed cash flows.
#'
#' @param cashflows Numeric vector. Fixed cash flows
#' @param times Numeric vector. Times (in years) for each cash flow
#' @param market_price Numeric. Market price of the security
#' @param spot_rates Numeric vector. Spot rates for each time point
#' @param initial_spread Numeric. Initial guess for the Z-spread in basis points
#'
#' @return A list containing:
#'   \item{z_spread}{The calculated Z-spread in basis points}
#'   \item{price}{The calculated price using the Z-spread}
#'   \item{error}{The pricing error}
#'
#' @export
calculate_z_spread <- function(cashflows, times, market_price, spot_rates, 
                              initial_spread = 100) {
  
  # Parameter validation
  checkmate::assert_numeric(cashflows)
  checkmate::assert_numeric(times, lower = 0)
  checkmate::assert_numeric(market_price, lower = 0)
  checkmate::assert_numeric(spot_rates)
  checkmate::assert_numeric(initial_spread)
  
  # Convert initial spread from basis points to decimal
  initial_spread_decimal <- initial_spread / 10000
  
  # Function to calculate price given a spread
  calculate_price <- function(spread_decimal) {
    # Calculate discount factors: exp(-(spot_rate + spread) * time)
    discount_factors <- exp(-(spot_rates + spread_decimal) * times)
    
    # Calculate present value
    price <- sum(cashflows * discount_factors)
    
    return(price)
  }
  
  # Function to minimize - the absolute difference between calculated price and market price
  objective_fn <- function(spread_decimal) {
    price <- calculate_price(spread_decimal)
    return(abs(price - market_price))
  }
  
  # Find Z-spread using optimization
  result <- stats::optimize(
    f = objective_fn,
    interval = c(-0.05, 0.05),  # Search in a reasonable range
    tol = 1e-6
  )
  
  # Convert spread from decimal to basis points
  z_spread <- result$minimum * 10000
  
  # Calculate price using the Z-spread
  price <- calculate_price(result$minimum)
  
  # Calculate error
  error <- price - market_price
  
  return(list(
    z_spread = z_spread,
    price = price,
    error = error
  ))
}

#' @title Calculate Option Cost
#' @description Calculates the option cost as the difference between Z-spread and OAS.
#'
#' @param z_spread Numeric. Z-spread in basis points
#' @param oas Numeric. Option-adjusted spread in basis points
#'
#' @return The option cost in basis points
#'
#' @export
calculate_option_cost <- function(z_spread, oas) {
  # Parameter validation
  checkmate::assert_numeric(z_spread)
  checkmate::assert_numeric(oas)
  
  # Calculate option cost
  option_cost <- z_spread - oas
  
  return(option_cost)
}

#' @title Calculate Price from Cash Flows
#' @description Calculates the price of a security from its cash flows using spot rates plus a spread.
#'
#' @param cashflows Numeric vector. Cash flows
#' @param times Numeric vector. Times (in years) for each cash flow
#' @param spot_rates Numeric vector. Spot rates for each time point
#' @param spread Numeric. Spread in basis points
#'
#' @return The calculated price
#'
#' @export
calculate_price <- function(cashflows, times, spot_rates, spread = 0) {
  
  # Parameter validation
  checkmate::assert_numeric(cashflows)
  checkmate::assert_numeric(times, lower = 0)
  checkmate::assert_numeric(spot_rates)
  checkmate::assert_numeric(spread)
  
  # Convert spread from basis points to decimal
  spread_decimal <- spread / 10000
  
  # Calculate discount factors: exp(-(spot_rate + spread) * time)
  discount_factors <- exp(-(spot_rates + spread_decimal) * times)
  
  # Calculate present value
  price <- sum(cashflows * discount_factors)
  
  return(price)
}
