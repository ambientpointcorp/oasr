#' @title Create Prepayment Model for OAS Analysis
#' @description Creates a prepayment model specification for use in cash flow projection.
#' Supports multiple model types including CPR/CDR-based models and time-to-event survival models.
#'
#' @param type Character. Type of prepayment model: "cpr_cdr", "psa", "survival", or "custom"
#' @param base_cpr Numeric. Base Conditional Prepayment Rate (CPR), annual rate in decimal form
#' @param base_cdr Numeric. Base Conditional Default Rate (CDR), annual rate in decimal form
#' @param recovery_rate Numeric. Loss recovery rate on defaulted principal (0-1)
#' @param recovery_lag Integer. Number of periods (months) until recovery is realized
#' @param seasoning Numeric. Seasoning multiplier for prepayment ramp
#' @param custom_fn Function. For "custom" type, a function that calculates prepayment and default rates
#' @param burnout_factor Numeric. Factor to account for burnout effect (higher values = more burnout)
#' @param rate_sensitivity Numeric. Sensitivity of prepayments to rate changes (refinancing incentive)
#' @param seasonal_factors Numeric vector. Monthly seasonal factors (length 12)
#' @param ... Additional parameters for specific model types
#'
#' @return A list with the prepayment model specification
#'
#' @examples
#' # Basic CPR/CDR model
#' model1 <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01, recovery_rate = 0.65)
#'
#' # PSA model
#' model2 <- prepayment_model(type = "psa", psa_multiplier = 1.0, base_cdr = 0.01, recovery_rate = 0.65)
#'
#' @export
prepayment_model <- function(type = "cpr_cdr", 
                             base_cpr = 0.06,
                             base_cdr = 0.01,
                             recovery_rate = 0.65,
                             recovery_lag = 12,
                             seasoning = 1.0,
                             custom_fn = NULL,
                             burnout_factor = 0.3,
                             rate_sensitivity = 1.0,
                             seasonal_factors = c(0.94, 0.96, 0.98, 1.02, 1.05, 1.06, 
                                               1.04, 1.03, 1.02, 0.98, 0.97, 0.95),
                             ...) {
  
  # Parameter validation
  checkmate::assert_choice(type, c("cpr_cdr", "psa", "survival", "custom"))
  
  if (type != "custom") {
    checkmate::assert_numeric(base_cpr, lower = 0, upper = 1)
    checkmate::assert_numeric(base_cdr, lower = 0, upper = 1)
    checkmate::assert_numeric(recovery_rate, lower = 0, upper = 1)
    checkmate::assert_int(recovery_lag, lower = 0)
    checkmate::assert_numeric(seasoning, lower = 0)
    checkmate::assert_numeric(burnout_factor, lower = 0)
    checkmate::assert_numeric(rate_sensitivity, lower = 0)
    checkmate::assert_numeric(seasonal_factors, lower = 0)
  } else {
    checkmate::assert_function(custom_fn)
  }
  
  # Create the prepayment model
  prepay_model <- list(
    type = type,
    base_cpr = base_cpr,
    base_cdr = base_cdr,
    recovery_rate = recovery_rate,
    recovery_lag = recovery_lag,
    seasoning = seasoning,
    burnout_factor = burnout_factor,
    rate_sensitivity = rate_sensitivity,
    seasonal_factors = seasonal_factors
  )
  
  # Add model-specific parameters
  dots <- list(...)
  prepay_model <- c(prepay_model, dots)
  
  # For custom model, add the function
  if (type == "custom") {
    prepay_model$custom_fn <- custom_fn
  }
  
  # For PSA model, ensure we have a PSA multiplier
  if (type == "psa" && !("psa_multiplier" %in% names(prepay_model))) {
    prepay_model$psa_multiplier <- 1.0
  }
  
  # For survival model, ensure we have the required parameters
  if (type == "survival") {
    required_params <- c("prepay_hazard", "default_hazard")
    if (!all(required_params %in% names(prepay_model))) {
      stop("Survival model requires parameters: ", paste(required_params, collapse = ", "))
    }
  }
  
  class(prepay_model) <- c("prepayment_model", "list")
  return(prepay_model)
}

#' @title Calculate Prepayment and Default Rates
#' @description Calculates prepayment and default rates for cash flow projection
#' based on the prepayment model, asset characteristics, and interest rate paths.
#'
#' @param asset_data Data frame. Asset data with characteristics
#' @param rate_paths List. Result from \code{\link{simulate_rates}}
#' @param prepay_model List. Prepayment model from \code{\link{prepayment_model}}
#' @param reference_key Character. Key for the reference rate to use
#'
#' @return A list containing:
#'   \item{prepay_rates}{Array of prepayment rates with dimensions [path, time, asset]}
#'   \item{default_rates}{Array of default rates with dimensions [path, time, asset]}
#'   \item{recovery_rates}{Array of recovery rates with dimensions [path, time, asset]}
#'
#' @export
calculate_credit_vectors <- function(asset_data, rate_paths, prepay_model, reference_key = "10") {
  
  # Parameter validation
  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(rate_paths)
  checkmate::assert_class(prepay_model, "prepayment_model")
  
  # Extract reference rates
  reference_rates <- extract_reference_rates(rate_paths, reference_key)
  
  # Get dimensions
  n_paths <- nrow(reference_rates)
  n_periods <- ncol(reference_rates)
  n_assets <- nrow(asset_data)
  
  # Initialize arrays for prepayment, default, and recovery rates
  prepay_rates <- array(0, dim = c(n_paths, n_periods, n_assets))
  default_rates <- array(0, dim = c(n_paths, n_periods, n_assets))
  recovery_rates <- array(0, dim = c(n_paths, n_periods, n_assets))
  
  # For each asset, calculate rates based on the model type
  for (i in 1:n_assets) {
    asset <- asset_data[i, ]
    
    # Extract asset-specific characteristics
    asset_age <- if ("age" %in% names(asset)) asset$age else 0
    asset_wac <- if ("wac" %in% names(asset)) asset$wac else 0.05
    asset_ltv <- if ("ltv" %in% names(asset)) asset$ltv else 0.75
    asset_fico <- if ("fico" %in% names(asset)) asset$fico else 720
    asset_state <- if ("state" %in% names(asset)) asset$state else NA
    asset_occ_type <- if ("occupancy_type" %in% names(asset)) asset$occupancy_type else "PRIMARY"
    asset_property_type <- if ("property_type" %in% names(asset)) asset$property_type else "SFR"
    
    # Calculate vectors based on model type
    if (prepay_model$type == "cpr_cdr") {
      # CPR/CDR model
      for (path in 1:n_paths) {
        for (t in 1:n_periods) {
          # Current loan age in months
          current_age <- asset_age + t - 1
          
          # Calculate refinancing incentive based on rates
          current_rate <- reference_rates[path, t]
          rate_spread <- 0.015 # 150 bps typical spread between Treasury and mortgage rate
          market_rate <- current_rate + rate_spread
          refi_incentive <- asset_wac - market_rate
          
          # Apply S-curve for refinancing response
          refi_component <- if (refi_incentive <= 0) {
            0.001  # Minimal refinancing when underwater
          } else {
            0.2 * tanh(prepay_model$rate_sensitivity * refi_incentive) 
          }
          
          # Apply seasoning ramp
          seasoning_effect <- min(1, current_age / 30) * 
            exp(-prepay_model$seasoning * max(0, current_age - 60) / 120)
          
          # Burnout effect
          burnout_effect <- exp(-prepay_model$burnout_factor * max(0, refi_incentive))
          
          # Seasonal effect
          month_of_year <- (t - 1) %% 12 + 1
          seasonal_effect <- prepay_model$seasonal_factors[month_of_year]
          
          # Combine effects for final CPR
          cpr <- prepay_model$base_cpr * seasoning_effect * burnout_effect * 
            seasonal_effect * (1 + refi_component)
          
          # Default rate - could make more sophisticated
          cdr <- prepay_model$base_cdr * 
            (1 + max(0, (asset_ltv - 0.8) * 2)) * # Higher LTV increases default risk
            (1 - min(1, (asset_fico - 620) / 200)) # Lower FICO increases default risk
          
          # Convert to monthly rates (SMM/MDR)
          smm <- 1 - (1 - cpr)^(1/12)
          mdr <- 1 - (1 - cdr)^(1/12)
          
          # Store in arrays
          prepay_rates[path, t, i] <- smm
          default_rates[path, t, i] <- mdr
          
          # Calculate recovery for defaults
          # If t + recovery_lag <= n_periods, store recovery in the future
          recovery_time <- t + prepay_model$recovery_lag
          if (recovery_time <= n_periods) {
            recovery_rates[path, recovery_time, i] <- 
              default_rates[path, t, i] * prepay_model$recovery_rate
          }
        }
      }
    } else if (prepay_model$type == "psa") {
      # PSA model
      for (path in 1:n_paths) {
        for (t in 1:n_periods) {
          # Current loan age in months
          current_age <- asset_age + t - 1
          
          # Calculate PSA prepayment rate
          # PSA assumes 0.2% CPR in month 1, increasing by 0.2% per month until month 30, then 6% after
          base_psa_cpr <- min(0.06, 0.002 * current_age)
          
          # Apply PSA multiplier
          cpr <- base_psa_cpr * prepay_model$psa_multiplier
          
          # Default rate - similar to CPR/CDR model
          cdr <- prepay_model$base_cdr * 
            (1 + max(0, (asset_ltv - 0.8) * 2)) * 
            (1 - min(1, (asset_fico - 620) / 200))
          
          # Convert to monthly rates
          smm <- 1 - (1 - cpr)^(1/12)
          mdr <- 1 - (1 - cdr)^(1/12)
          
          # Store in arrays
          prepay_rates[path, t, i] <- smm
          default_rates[path, t, i] <- mdr
          
          # Calculate recovery for defaults
          recovery_time <- t + prepay_model$recovery_lag
          if (recovery_time <= n_periods) {
            recovery_rates[path, recovery_time, i] <- 
              default_rates[path, t, i] * prepay_model$recovery_rate
          }
        }
      }
    } else if (prepay_model$type == "survival") {
      # Survival model (time-to-event)
      for (path in 1:n_paths) {
        for (t in 1:n_periods) {
          # Current loan age in months
          current_age <- asset_age + t - 1
          
          # Prepayment hazard rate
          prepay_hazard <- prepay_model$prepay_hazard * 
            exp(-prepay_model$seasoning * current_age / 120)
          
          # Default hazard rate
          default_hazard <- prepay_model$default_hazard * 
            (1 + max(0, (asset_ltv - 0.8) * 2)) * 
            (1 - min(1, (asset_fico - 620) / 200))
          
          # Store in arrays (hazard rates are already in monthly form)
          prepay_rates[path, t, i] <- prepay_hazard
          default_rates[path, t, i] <- default_hazard
          
          # Calculate recovery for defaults
          recovery_time <- t + prepay_model$recovery_lag
          if (recovery_time <= n_periods) {
            recovery_rates[path, recovery_time, i] <- 
              default_rates[path, t, i] * prepay_model$recovery_rate
          }
        }
      }
    } else if (prepay_model$type == "custom") {
      # Custom model - call the provided function
      result <- prepay_model$custom_fn(
        asset = asset,
        reference_rates = reference_rates,
        n_paths = n_paths,
        n_periods = n_periods
      )
      
      # Check result format
      if (!all(c("prepay_rates", "default_rates", "recovery_rates") %in% names(result))) {
        stop("Custom function must return a list with prepay_rates, default_rates, and recovery_rates")
      }
      
      # Store results
      prepay_rates[, , i] <- result$prepay_rates
      default_rates[, , i] <- result$default_rates
      recovery_rates[, , i] <- result$recovery_rates
    }
  }
  
  return(list(
    prepay_rates = prepay_rates,
    default_rates = default_rates,
    recovery_rates = recovery_rates
  ))
}

#' @title Create Survival-Based Prepayment Model
#' @description Creates a prepayment model based on survival analysis (time-to-event)
#' with specified hazard rates for prepayment and default.
#'
#' @param prepay_hazard Numeric. Monthly hazard rate for prepayment
#' @param default_hazard Numeric. Monthly hazard rate for default
#' @param recovery_rate Numeric. Loss recovery rate on defaulted principal (0-1)
#' @param recovery_lag Integer. Number of periods (months) until recovery is realized
#' @param seasoning Numeric. Seasoning factor for the hazard function
#'
#' @return A prepayment model list with type = "survival"
#'
#' @examples
#' model <- survival_prepayment_model(
#'   prepay_hazard = 0.005,
#'   default_hazard = 0.001,
#'   recovery_rate = 0.65
#' )
#'
#' @export
survival_prepayment_model <- function(prepay_hazard = 0.005,
                                      default_hazard = 0.001,
                                      recovery_rate = 0.65,
                                      recovery_lag = 12,
                                      seasoning = 0.3) {
  
  prepay_model <- prepayment_model(
    type = "survival",
    prepay_hazard = prepay_hazard,
    default_hazard = default_hazard,
    recovery_rate = recovery_rate,
    recovery_lag = recovery_lag,
    seasoning = seasoning
  )
  
  return(prepay_model)
}

#' @title Create PSA Prepayment Model
#' @description Creates a prepayment model based on the Public Securities Association (PSA)
#' standard prepayment model. 100% PSA assumes 0.2% CPR in month 1, increasing by 0.2% 
#' per month until month 30, then 6% afterwards.
#'
#' @param psa_multiplier Numeric. Multiplier for the standard PSA curve
#' @param base_cdr Numeric. Base Conditional Default Rate (CDR), annual rate in decimal form
#' @param recovery_rate Numeric. Loss recovery rate on defaulted principal (0-1)
#' @param recovery_lag Integer. Number of periods (months) until recovery is realized
#'
#' @return A prepayment model list with type = "psa"
#'
#' @examples
#' model <- psa_prepayment_model(
#'   psa_multiplier = 1.5,
#'   base_cdr = 0.01,
#'   recovery_rate = 0.65
#' )
#'
#' @export
psa_prepayment_model <- function(psa_multiplier = 1.0,
                                 base_cdr = 0.01,
                                 recovery_rate = 0.65,
                                 recovery_lag = 12) {
  
  prepay_model <- prepayment_model(
    type = "psa",
    base_cpr = 0.06, # Not used directly in PSA model, but included for consistency
    base_cdr = base_cdr,
    recovery_rate = recovery_rate,
    recovery_lag = recovery_lag,
    psa_multiplier = psa_multiplier
  )
  
  return(prepay_model)
}

#' @title Create Custom Prepayment Model
#' @description Creates a custom prepayment model based on a user-provided function
#' that calculates prepayment, default, and recovery rates.
#'
#' @param custom_fn Function. A function that takes asset, reference_rates, n_paths, n_periods
#'   and returns a list with prepay_rates, default_rates, and recovery_rates
#' @param ... Additional parameters to be stored in the model
#'
#' @return A prepayment model list with type = "custom"
#'
#' @examples
#' \dontrun{
#' # Example custom function
#' my_custom_fn <- function(asset, reference_rates, n_paths, n_periods) {
#'   # Create arrays for rates
#'   prepay_rates <- array(0, dim = c(n_paths, n_periods))
#'   default_rates <- array(0, dim = c(n_paths, n_periods))
#'   recovery_rates <- array(0, dim = c(n_paths, n_periods))
#'   
#'   # Fill arrays with custom logic
#'   # ...
#'   
#'   return(list(
#'     prepay_rates = prepay_rates,
#'     default_rates = default_rates,
#'     recovery_rates = recovery_rates
#'   ))
#' }
#'
#' model <- custom_prepayment_model(my_custom_fn)
#' }
#'
#' @export
custom_prepayment_model <- function(custom_fn, ...) {
  
  checkmate::assert_function(custom_fn)
  
  prepay_model <- prepayment_model(
    type = "custom",
    custom_fn = custom_fn,
    ...
  )
  
  return(prepay_model)
}
