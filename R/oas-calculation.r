#' @title Calculate Option-Adjusted Spread (OAS)
#' @description Calculates the option-adjusted spread for a security with embedded options.
#'
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param prepay_model List. Prepayment model from \code{\link{prepayment_model}}
#' @param market_price Numeric. Market price of the security (as a percentage of par)
#' @param deal_structure List. Optional deal structure for structured securities
#' @param search_range Numeric vector. Range of OAS values to search (in basis points)
#' @param tolerance Numeric. Tolerance for price convergence
#' @param max_iterations Integer. Maximum number of iterations for the search
#' @param verbose Logical. Whether to print progress information
#' @param reference_key Character. Key of the reference rate used for credit
#'        vectors, discounting, and as the index for any variable rate loans or
#'        floating rate bonds. Defaults to the 10-year.
#' @param discounting Character. Passed to \code{\link{calculate_present_value}}:
#'        \code{"path"} compounds the short rate along each path, \code{"spot"}
#'        reproduces the earlier behaviour.
#' @param accrual_method Character. Day-count convention passed to
#'        \code{\link{project_cashflows}} when the asset data carries no
#'        \code{accrual_method} or \code{Accrual} column of its own.
#' @param start_date Date. Anchor for the payment schedule, needed by the ACT
#'        conventions. Passed to \code{\link{project_cashflows}}.
#' @param terminal_balance Character. Passed to \code{\link{project_cashflows}}:
#'        whether the balance outstanding at the horizon is returned as a final
#'        cash flow or dropped.
#' @param recovery_basis Character. Passed to \code{\link{project_cashflows}}:
#'        whether recovery is taken on the amount that defaulted or on the
#'        balance standing at the time.
#'
#' @return A list containing the OAS calculation results
#'
#' @examples
#' \dontrun{
#' # Get DNS parameters and simulate rate paths
#' dns_params <- dnsr::get_calibrated_dns_params()
#' rate_paths <- simulate_rates(dns_params, n_paths = 100)
#'
#' # Define prepayment model
#' prepay_model <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01)
#'
#' # Calculate OAS
#' oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)
#' }
#'
#' @export
calculate_oas <- function(asset_data, rate_paths, prepay_model, market_price,
                         deal_structure = NULL, search_range = c(-500, 500),
                         tolerance = 0.01, max_iterations = 50, verbose = TRUE,
                         reference_key = "10", discounting = c("path", "spot"),
                         accrual_method = "30/360", start_date = NULL,
                         terminal_balance = c("return", "drop"),
                         recovery_basis = c("defaults", "balance")) {

  discounting <- match.arg(discounting)
  terminal_balance <- match.arg(terminal_balance)
  recovery_basis <- match.arg(recovery_basis)

  # Parameter validation
  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(rate_paths)
  checkmate::assert_class(prepay_model, "prepayment_model")
  checkmate::assert_numeric(market_price, lower = 0)
  checkmate::assert_numeric(search_range, len = 2)
  checkmate::assert_numeric(tolerance, lower = 0)
  checkmate::assert_count(max_iterations, positive = TRUE)

  # Store start time for performance tracking
  start_time <- Sys.time()

  # Calculate total asset balance
  total_balance <- sum(asset_data$balance)

  # Determine if we're working with a structured security
  structured <- !is.null(deal_structure)

  # Reference rates are needed by variable rate loans and floating rate bonds.
  # Resolving them here and passing them down is what makes those reachable
  # through calculate_oas() at all.
  reference_rates <- extract_reference_rates(rate_paths, reference_key)

  # Nothing between here and the loop depends on the OAS, so it is computed
  # once. Previously all of it sat inside the price function and was redone on
  # every bisection iteration, which was the bulk of the run time.
  credit_vectors <- calculate_credit_vectors(
    asset_data = asset_data,
    rate_paths = rate_paths,
    prepay_model = prepay_model,
    reference_key = reference_key
  )

  cashflows <- project_cashflows(
    asset_data = asset_data,
    credit_vectors = credit_vectors,
    reference_rates = reference_rates,
    accrual_method = accrual_method,
    start_date = start_date,
    terminal_balance = terminal_balance,
    recovery_basis = recovery_basis
  )

  if (structured) {
    aggregated_cashflows <- aggregate_cashflows(
      cashflows = cashflows,
      asset_data = asset_data,
      deal_structure = deal_structure,
      reference_rates = reference_rates
    )
    total_bond_balance <- sum(deal_structure$bonds$balance)
  } else {
    aggregated_cashflows <- NULL
  }

  avg_life_result <- calculate_average_life(
    cashflows = cashflows,
    asset_level = TRUE
  )

  # Present value at a given OAS: the only part of the valuation the search
  # actually varies.
  price_at <- function(oas) {
    if (verbose) {
      cat("Calculating price with OAS =", oas, "bps...\n")
    }

    if (structured) {
      pv_result <- calculate_present_value(
        cashflows = aggregated_cashflows,
        rate_paths = rate_paths,
        oas = oas,
        reference_key = reference_key,
        asset_level = FALSE,
        total_idx = cashflow_components("bond")[["total"]],
        discounting = discounting
      )
      weighted_price <- sum(pv_result$mean_pvs) * 100 / total_bond_balance
    } else {
      pv_result <- calculate_present_value(
        cashflows = cashflows,
        rate_paths = rate_paths,
        oas = oas,
        reference_key = reference_key,
        asset_level = TRUE,
        total_idx = cashflow_components("asset")[["total"]],
        discounting = discounting
      )
      weighted_price <- sum(pv_result$mean_pvs) * 100 / total_balance
    }

    list(price = weighted_price, pv_result = pv_result)
  }

  # Binary search for OAS
  lower_bound <- min(search_range)
  upper_bound <- max(search_range)
  current_oas <- (lower_bound + upper_bound) / 2

  iteration <- 0
  best_oas <- current_oas
  best_priced <- NULL
  best_error <- Inf
  converged <- FALSE

  if (verbose) {
    cat("Starting OAS calculation with binary search...\n")
    cat("Market price:", market_price, "\n")
    cat("Search range:", lower_bound, "to", upper_bound, "bps\n")
    cat("Tolerance:", tolerance, "\n")
  }

  while (iteration < max_iterations) {
    iteration <- iteration + 1

    # Calculate price for current OAS
    priced <- price_at(current_oas)
    current_price <- priced$price
    current_error <- abs(current_price - market_price)

    if (verbose) {
      cat(sprintf("Iteration %d: OAS = %.2f bps, Price = %.4f, Target = %.4f, Error = %.4f\n",
                  iteration, current_oas, current_price, market_price, current_error))
    }

    # Track the best spread seen, not a whole valuation: the expensive parts
    # of the result are built once, after the search settles.
    if (current_error < best_error) {
      best_error <- current_error
      best_oas <- current_oas
      best_priced <- priced
    }

    # Check if we've converged
    if (current_error < tolerance) {
      converged <- TRUE
      if (verbose) {
        cat("Converged within tolerance. OAS =", current_oas, "bps\n")
      }
      break
    }

    # Update bounds
    if (current_price > market_price) {
      # Price too high, increase OAS (which decreases price)
      lower_bound <- current_oas
    } else {
      # Price too low, decrease OAS (which increases price)
      upper_bound <- current_oas
    }

    # Update current OAS
    current_oas <- (lower_bound + upper_bound) / 2

    # Add damping if needed
    if (iteration > 10 && abs(upper_bound - lower_bound) < 1) {
      # We're very close but not converging - add damping
      if (verbose) {
        cat("Adding damping to improve convergence\n")
      }
      current_oas <- 0.7 * current_oas + 0.3 * (lower_bound + upper_bound) / 2
    }
  }

  # Use the best spread seen if the search did not converge
  if (!converged) {
    warning("Maximum iterations reached without convergence. Using best result with error: ", best_error)
    current_oas <- best_oas
  }

  # Duration is wanted for the answer, not for the search, so it is computed
  # once here rather than at every trial spread.
  duration_result <- calculate_duration_convexity(
    cashflows = cashflows,
    rate_paths = rate_paths,
    oas = current_oas,
    reference_key = reference_key,
    asset_level = TRUE,
    discounting = discounting
  )

  result <- list(
    price = best_priced$price,
    oas = current_oas,
    pv_result = best_priced$pv_result,
    cashflows = cashflows,
    credit_vectors = credit_vectors,
    avg_life = avg_life_result,
    duration = duration_result,
    structured_cashflows = aggregated_cashflows
  )

  # Calculate execution time
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")

  # Add additional information to result
  result$market_price <- market_price
  result$price_error <- abs(result$price - market_price)
  result$converged <- converged
  result$iterations <- iteration
  result$execution_time <- execution_time
  result$search_range <- search_range

  # The conventions this solve ran under, so a sensitivity or surface built
  # from it re-solves on the same basis rather than silently reverting to the
  # defaults.
  result$options <- list(
    reference_key = reference_key,
    discounting = discounting,
    accrual_method = accrual_method,
    start_date = start_date,
    terminal_balance = terminal_balance,
    recovery_basis = recovery_basis
  )
  result$asset_data <- asset_data
  result$rate_paths <- rate_paths
  result$prepay_model <- prepay_model

  # Create S3 class
  class(result) <- c("oas_result", class(result))

  return(result)
}

#' @title Binary Search OAS with Fixed Cash Flows
#' @description Performs a binary search to find the OAS that matches a target price
#' for a fixed set of cash flows.
#'
#' @param cashflows Numeric vector. Cash flows
#' @param times Numeric vector. Times (in years) for each cash flow
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param market_price Numeric. Market price of the security
#' @param search_range Numeric vector. Range of OAS values to search (in basis points)
#' @param tolerance Numeric. Tolerance for price convergence
#' @param max_iterations Integer. Maximum number of iterations for the search
#'
#' @return A list containing the OAS calculation results
#'
#' @export
binary_search_oas <- function(cashflows, times, rate_paths, market_price,
                             search_range = c(-500, 500), tolerance = 0.01,
                             max_iterations = 50) {

  # Parameter validation
  checkmate::assert_numeric(cashflows)
  checkmate::assert_numeric(times, lower = 0)
  checkmate::assert_list(rate_paths)
  checkmate::assert_numeric(market_price, lower = 0)
  checkmate::assert_numeric(search_range, len = 2)
  checkmate::assert_numeric(tolerance, lower = 0)
  checkmate::assert_count(max_iterations, positive = TRUE)

  # Extract reference rates
  reference_key <- "10"  # Default to 10-year
  reference_rates <- extract_reference_rates(rate_paths, reference_key)
  n_paths <- nrow(reference_rates)

  # Function to calculate price given an OAS
  calculate_price <- function(oas_bps) {
    # Convert OAS from basis points to decimal
    oas_decimal <- oas_bps / 10000

    # Calculate present values for each path
    path_pvs <- numeric(n_paths)

    for (i in 1:n_paths) {
      # Get rates for this path
      rates <- reference_rates[i, ]

      # Use rates at times closest to cash flow times
      cf_rates <- numeric(length(times))
      for (t in seq_along(times)) {
        # Clamped into range at both ends: a time under half a month rounds to
        # 0, which indexes nothing.
        time_in_months <- max(1L, min(round(times[t] * 12), ncol(reference_rates)))
        cf_rates[t] <- rates[time_in_months]
      }

      # Calculate discount factors with OAS
      discount_factors <- exp(-(cf_rates + oas_decimal) * times)

      # Calculate present value for this path
      path_pvs[i] <- sum(cashflows * discount_factors)
    }

    # Return average price
    mean(path_pvs)
  }

  # Binary search for OAS
  lower_bound <- min(search_range)
  upper_bound <- max(search_range)
  current_oas <- (lower_bound + upper_bound) / 2

  iteration <- 0
  best_oas <- current_oas
  best_error <- Inf
  best_price <- calculate_price(current_oas)

  while (iteration < max_iterations) {
    iteration <- iteration + 1

    # Calculate price for current OAS
    current_price <- calculate_price(current_oas)
    current_error <- abs(current_price - market_price)

    # Check if this is the best result so far
    if (current_error < best_error) {
      best_error <- current_error
      best_oas <- current_oas
      best_price <- current_price
    }

    # Check if we've converged
    if (current_error < tolerance) {
      break
    }

    # Update bounds
    if (current_price > market_price) {
      # Price too high, increase OAS (which decreases price)
      lower_bound <- current_oas
    } else {
      # Price too low, decrease OAS (which increases price)
      upper_bound <- current_oas
    }

    # Update current OAS
    current_oas <- (lower_bound + upper_bound) / 2
  }

  # Return results
  return(list(
    oas = best_oas,
    price = best_price,
    market_price = market_price,
    price_error = abs(best_price - market_price),
    converged = iteration < max_iterations,
    iterations = iteration
  ))
}

#' @title Print Method for OAS Result
#' @description Prints a summary of the OAS calculation result.
#'
#' @param x An object of class "oas_result"
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#'
#' @export
#' @method print oas_result
print.oas_result <- function(x, ...) {
  cat("Option-Adjusted Spread (OAS) Analysis Result\n")
  cat("--------------------------------------------\n")
  cat("OAS:", round(x$oas, 2), "bps\n")
  cat("Model Price:", round(x$price, 4), "\n")
  cat("Market Price:", round(x$market_price, 4), "\n")
  cat("Price Error:", round(x$price_error, 4), "\n")
  # Modified line - check if nested structure exists
  wal <- if (is.list(x$avg_life$avg_lives) && "overall_wal" %in% names(x$avg_life$avg_lives)) {
    x$avg_life$avg_lives$overall_wal
  } else if (is.list(x$avg_life) && "overall_wal" %in% names(x$avg_life)) {
    x$avg_life$overall_wal
  } else {
    mean(x$avg_life$avg_lives)  # Fallback to mean of avg_lives if no overall_wal
  }
  cat("Weighted Average Life:", round(wal, 2), "years\n")
  cat("Effective Duration:", round(mean(x$duration$duration), 2), "\n")
  cat("Effective Convexity:", round(mean(x$duration$convexity), 2), "\n")
  cat("Convergence:", if(x$converged) "Yes" else "No", "\n")
  cat("Iterations:", x$iterations, "\n")
  cat("Execution Time:", round(as.numeric(x$execution_time), 2), "seconds\n")
  invisible(x)
}

#' @title Summary Method for OAS Result
#' @description Provides a summary of the OAS calculation result.
#'
#' @param object An object of class "oas_result"
#' @param ... Additional arguments (not used)
#'
#' @return A list with a summary of the OAS calculation. The
#'   \code{convergence} element holds \code{converged}, a logical, and
#'   \code{iterations}, an integer.
#'
#' @export
#' @method summary oas_result
summary.oas_result <- function(object, ...) {
  # Modified to handle different structures
  wal <- if (is.list(object$avg_life$avg_lives) && "overall_wal" %in% names(object$avg_life$avg_lives)) {
    object$avg_life$avg_lives$overall_wal
  } else if (is.list(object$avg_life) && "overall_wal" %in% names(object$avg_life)) {
    object$avg_life$overall_wal
  } else {
    mean(object$avg_life$avg_lives)  # Fallback to mean of avg_lives if no overall_wal
  }
  # Create a summary data frame
  summary_df <- data.frame(
    Metric = c(
      "OAS (bps)",
      "Model Price",
      "Market Price",
      "Price Error",
      "Weighted Average Life (years)",
      "Effective Duration",
      "Effective Convexity"
    ),
    Value = c(
      round(object$oas, 2),
      round(object$price, 4),
      round(object$market_price, 4),
      round(object$price_error, 4),
      round(wal, 2),
      round(mean(object$duration$duration), 2),
      round(mean(object$duration$convexity), 2)
    )
  )

  # Provide information about convergence
  converged <- isTRUE(object$converged)
  iterations <- object$iterations

  # Calculate asset-specific metrics if available
  if (!is.null(object$asset_data)) {
    n_assets <- nrow(object$asset_data)

    # Asset information
    asset_info <- data.frame(
      Asset = 1:n_assets,
      Balance = object$asset_data$balance,
      WAC = object$asset_data$wac,
      WAL = object$avg_life$avg_lives,
      Duration = object$duration$duration,
      Convexity = object$duration$convexity
    )

    # Return all summaries
    return(list(
      overall = summary_df,
      convergence = list(converged = converged, iterations = iterations),
      execution_time = object$execution_time,
      asset_info = asset_info
    ))
  } else {
    # Return basic summary
    return(list(
      overall = summary_df,
      convergence = list(converged = converged, iterations = iterations),
      execution_time = object$execution_time
    ))
  }
}

#' @title Plot Method for OAS Result
#' @description Creates plots to visualize the OAS analysis results.
#'
#' @param x An object of class "oas_result"
#' @param type Character. Type of plot to create: "cashflows", "rates", "prepayments", or "all"
#' @param ... Additional arguments (not used)
#'
#' @return A list of ggplot objects
#'
#' @export
#' @method plot oas_result
plot.oas_result <- function(x, type = "cashflows", ...) {
  # Parameter validation
  checkmate::assert_choice(type, c("cashflows", "rates", "prepayments", "all"))

  # Initialize list to store plots
  plots <- list()

  # Plot cash flows
  if (type == "cashflows" || type == "all") {
    plots$cashflows <- plot_cashflows(x)
  }

  # Plot interest rates
  if (type == "rates" || type == "all") {
    plots$rates <- plot_rate_paths(x$rate_paths)
  }

  # Plot prepayments
  if (type == "prepayments" || type == "all") {
    plots$prepayments <- plot_prepayment_rates(x)
  }

  # Return the plots
  if (length(plots) == 1) {
    return(plots[[1]])
  } else {
    return(plots)
  }
}
