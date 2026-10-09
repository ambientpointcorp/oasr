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
#' model1 <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06,
#'                            base_cdr = 0.01, recovery_rate = 0.65)
#'
#' # PSA model
#' model2 <- prepayment_model(type = "psa", psa_multiplier = 1.0,
#'                            base_cdr = 0.01, recovery_rate = 0.65)
#'
#' @export
prepayment_model <- function(type = "cpr_cdr",
                             base_cpr = 0.06,
                             base_cdr = 0.01,
                             recovery_rate = 0.65,
                             recovery_lag = 12,
                             seasoning = 0.0,
                             custom_fn = NULL,
                             burnout_factor = 0.0,
                             rate_sensitivity = 0.0,
                             seasonal_factors = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                                  1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
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

#' @title Vectorized Per-Asset Credit Vector Computation (Internal)
#' @description Computes prepay/default/recovery matrices for a single asset.
#'   Defined at the top level so it carries no closure environment and can be
#'   safely serialized to parallel worker nodes.
#'
#' @keywords internal
#' @noRd
.compute_asset_vectors <- function(i, asset_ages, asset_wacs, asset_ltvs, asset_ficos,
                                   asset_data, reference_rates, prepay_model,
                                   t_vec, n_paths, n_periods, recovery_lag) {

  asset_age  <- asset_ages[i]
  asset_wac  <- asset_wacs[i]
  asset_ltv  <- asset_ltvs[i]
  asset_fico <- asset_ficos[i]

  # Current ages across time: vector of length n_periods
  current_ages <- asset_age + t_vec - 1

  if (prepay_model$type == "cpr_cdr") {
    # ------------------------------------------------------------------
    # CPR/CDR model — fully vectorized over paths and time
    # ------------------------------------------------------------------

    # Refinancing incentive: depends on reference_rates [n_paths x n_periods]
    rate_spread <- 0.015
    market_rate <- reference_rates + rate_spread
    refi_incentive <- asset_wac - market_rate

    # S-curve refinancing component: [n_paths x n_periods]
    refi_component <- ifelse(
      refi_incentive <= 0,
      0.0,
      0.2 * tanh(prepay_model$rate_sensitivity * refi_incentive)
    )

    # Seasoning effect: vector of length n_periods
    seasoning_effect <- pmin(1, current_ages / 30) *
      exp(-prepay_model$seasoning * pmax(0, current_ages - 60) / 120)

    # Burnout effect: [n_paths x n_periods]
    burnout_effect <- exp(-prepay_model$burnout_factor * pmax(0, refi_incentive))

    # Seasonal effect: vector of length n_periods
    month_of_year <- (t_vec - 1) %% 12 + 1
    seasonal_effect <- prepay_model$seasonal_factors[month_of_year]

    # Per-period scalar multiplier: [n_periods]
    time_multiplier <- prepay_model$base_cpr * seasoning_effect * seasonal_effect

    # CPR [n_paths x n_periods]. Clamped below 1 so the annual-to-monthly
    # conversion cannot take the root of a negative number; values already
    # under the cap pass through untouched.
    cpr <- sweep(burnout_effect * (1 + refi_component), 2, time_multiplier, `*`)
    cpr <- pmin(cpr, 1 - 1e-12)

    # CDR: scalar per asset
    cdr_scalar <- prepay_model$base_cdr *
      (1 + max(0, (asset_ltv - 0.8) * 2)) *
      (1 - min(1, (asset_fico - 620) / 200))

    # Convert annual rates to monthly
    prepay_mat  <- 1 - (1 - cpr)^(1/12)
    mdr_scalar  <- 1 - (1 - cdr_scalar)^(1/12)
    default_mat <- matrix(mdr_scalar, nrow = n_paths, ncol = n_periods)

  } else if (prepay_model$type == "psa") {
    # ------------------------------------------------------------------
    # PSA model — vectorized
    # ------------------------------------------------------------------

    base_psa_cpr <- pmin(0.06, 0.002 * current_ages)
    cpr <- pmin(base_psa_cpr * prepay_model$psa_multiplier, 1 - 1e-12)

    cdr_scalar <- prepay_model$base_cdr *
      (1 + max(0, (asset_ltv - 0.8) * 2)) *
      (1 - min(1, (asset_fico - 620) / 200))

    smm <- 1 - (1 - cpr)^(1/12)
    mdr_scalar <- 1 - (1 - cdr_scalar)^(1/12)

    prepay_mat  <- matrix(smm, nrow = n_paths, ncol = n_periods, byrow = TRUE)
    default_mat <- matrix(mdr_scalar, nrow = n_paths, ncol = n_periods)

  } else if (prepay_model$type == "survival") {
    # ------------------------------------------------------------------
    # Survival model — vectorized
    # ------------------------------------------------------------------

    prepay_hazard <- prepay_model$prepay_hazard *
      exp(-prepay_model$seasoning * current_ages / 120)

    default_hazard_scalar <- prepay_model$default_hazard *
      (1 + max(0, (asset_ltv - 0.8) * 2)) *
      (1 - min(1, (asset_fico - 620) / 200))

    prepay_mat  <- matrix(prepay_hazard, nrow = n_paths, ncol = n_periods, byrow = TRUE)
    default_mat <- matrix(default_hazard_scalar, nrow = n_paths, ncol = n_periods)

  } else if (prepay_model$type == "custom") {
    # ------------------------------------------------------------------
    # Custom model — delegate to user function
    # ------------------------------------------------------------------
    asset <- asset_data[i, ]
    result <- prepay_model$custom_fn(
      asset = asset,
      reference_rates = reference_rates,
      n_paths = n_paths,
      n_periods = n_periods
    )

    if (!all(c("prepay_rates", "default_rates", "recovery_rates") %in% names(result))) {
      stop("Custom function must return a list with prepay_rates, default_rates, and recovery_rates")
    }

    return(list(
      prepay_mat   = result$prepay_rates,
      default_mat  = result$default_rates,
      recovery_mat = result$recovery_rates
    ))
  }

  # --- Recovery rates: shift defaults forward by recovery_lag ---
  recovery_mat <- matrix(0, nrow = n_paths, ncol = n_periods)
  if (recovery_lag > 0 && recovery_lag < n_periods) {
    src_cols <- seq_len(n_periods - recovery_lag)
    dst_cols <- src_cols + recovery_lag
    recovery_mat[, dst_cols] <- default_mat[, src_cols] * prepay_model$recovery_rate
  } else if (recovery_lag == 0) {
    recovery_mat <- default_mat * prepay_model$recovery_rate
  }

  list(
    prepay_mat   = prepay_mat,
    default_mat  = default_mat,
    recovery_mat = recovery_mat
  )
}


#' @title Calculate Prepayment and Default Rates (Vectorized & Parallelized)
#' @description Calculates prepayment and default rates for cash flow projection
#' based on the prepayment model, asset characteristics, and interest rate paths.
#' This version replaces nested for-loops with vectorized matrix operations and
#' supports optional parallel processing across assets via the \code{parallel} package.
#'
#' @param asset_data Data frame. Asset data with characteristics
#' @param rate_paths List. Result from \code{\link{simulate_rates}}
#' @param prepay_model List. Prepayment model from \code{\link{prepayment_model}}
#' @param reference_key Character. Key for the reference rate to use
#' @param parallel Logical. Deprecated spelling of \code{run_parallel}, kept
#'        because existing callers pass it. Prefer \code{run_parallel}.
#' @param run_parallel Logical. Whether to use parallel processing across assets. Default is FALSE.
#'        Note: this parameter is named \code{run_parallel} (not \code{parallel}) to avoid
#'        shadowing the \code{parallel} package namespace. For backward compatibility,
#'        \code{parallel} is also accepted.
#' @param n_cores Integer. Number of cores to use for parallel processing.
#'        If NULL, uses all available cores minus 1.
#'
#' @return A list containing:
#'   \item{prepay_rates}{Array of prepayment rates with dimensions `[path, time, asset]`}
#'   \item{default_rates}{Array of default rates with dimensions `[path, time, asset]`}
#'   \item{recovery_rates}{Array of recovery rates with dimensions `[path, time, asset]`}
#'
#' @export
calculate_credit_vectors <- function(asset_data, rate_paths, prepay_model,
                                     reference_key = "10",
                                     parallel = FALSE, n_cores = NULL,
                                     run_parallel = parallel) {

  # Parameter validation
  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(rate_paths)
  checkmate::assert_class(prepay_model, "prepayment_model")
  checkmate::assert_logical(run_parallel, len = 1)
  if (!is.null(n_cores)) {
    checkmate::assert_int(n_cores, lower = 1)
  }

  # Extract reference rates
  reference_rates <- extract_reference_rates(rate_paths, reference_key)

  # Get dimensions
  n_paths <- nrow(reference_rates)
  n_periods <- ncol(reference_rates)
  n_assets <- nrow(asset_data)

  # Pre-extract asset characteristics as vectors
  asset_ages  <- if ("age"  %in% names(asset_data)) asset_data$age  else rep(0,    n_assets)
  asset_wacs  <- if ("wac"  %in% names(asset_data)) asset_data$wac  else rep(0.05, n_assets)
  asset_ltvs  <- if ("ltv"  %in% names(asset_data)) asset_data$ltv  else rep(0.80, n_assets)
  asset_ficos <- if ("fico" %in% names(asset_data)) asset_data$fico else rep(620,  n_assets)

  # Pre-compute shared time vector
  t_vec <- seq_len(n_periods)

  # Recovery lag from model
  recovery_lag <- prepay_model$recovery_lag

  # Bundle all shared arguments once
  shared_args <- list(
    asset_ages      = asset_ages,
    asset_wacs      = asset_wacs,
    asset_ltvs      = asset_ltvs,
    asset_ficos     = asset_ficos,
    asset_data      = asset_data,
    reference_rates = reference_rates,
    prepay_model    = prepay_model,
    t_vec           = t_vec,
    n_paths         = n_paths,
    n_periods       = n_periods,
    recovery_lag    = recovery_lag
  )

  # ---------------------------------------------------------------------------
  # Execute: parallel or sequential over assets
  # ---------------------------------------------------------------------------
  use_parallel <- run_parallel && n_assets > 1

  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    n_cores <- min(n_cores, n_assets)

    if (.Platform$OS.type == "unix") {
      asset_results <- parallel::mclapply(seq_len(n_assets), function(i) {
        do.call(.compute_asset_vectors, c(list(i = i), shared_args))
      }, mc.cores = n_cores)

      # mclapply returns error conditions in-band — check for them
      for (j in seq_along(asset_results)) {
        if (inherits(asset_results[[j]], "try-error")) {
          stop("Parallel credit vector computation failed for asset ", j, ": ",
               as.character(asset_results[[j]]))
        }
      }
    } else {
      worker_fn <- .compute_asset_vectors
      environment(worker_fn) <- baseenv()

      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterExport(cl, varlist = c("worker_fn", "shared_args"),
                              envir = environment())

      asset_results <- parallel::parLapply(cl, seq_len(n_assets), function(i) {
        do.call(worker_fn, c(list(i = i), shared_args))
      })
    }
  } else {
    asset_results <- lapply(seq_len(n_assets), function(i) {
      do.call(.compute_asset_vectors, c(list(i = i), shared_args))
    })
  }

  # ---------------------------------------------------------------------------
  # Assemble results into 3D arrays [n_paths, n_periods, n_assets]
  # ---------------------------------------------------------------------------
  prepay_rates   <- array(0, dim = c(n_paths, n_periods, n_assets))
  default_rates  <- array(0, dim = c(n_paths, n_periods, n_assets))
  recovery_rates <- array(0, dim = c(n_paths, n_periods, n_assets))

  for (i in seq_len(n_assets)) {
    prepay_rates[, , i]   <- asset_results[[i]]$prepay_mat
    default_rates[, , i]  <- asset_results[[i]]$default_mat
    recovery_rates[, , i] <- asset_results[[i]]$recovery_mat
  }

  return(list(
    prepay_rates   = prepay_rates,
    default_rates  = default_rates,
    recovery_rates = recovery_rates,
    # Carried so the projection can take recovery on the amount that actually
    # defaulted, rather than on the whole balance standing at the time.
    recovery_rate  = prepay_model$recovery_rate,
    recovery_lag   = recovery_lag
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
                                      seasoning = 0.0) {

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
