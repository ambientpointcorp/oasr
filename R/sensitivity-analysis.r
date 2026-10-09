#' @title Re-solve on the Same Conventions (Internal)
#' @description Re-runs \code{\link{calculate_oas}} with whatever conventions
#'   the base result was solved under. Results from before \code{options} was
#'   recorded fall back to the function defaults.
#'
#' @keywords internal
#' @noRd
.resolve_oas <- function(oas_result, asset_data, rate_paths, prepay_model,
                         market_price, max_iterations) {
  args <- list(
    asset_data = asset_data, rate_paths = rate_paths,
    prepay_model = prepay_model, market_price = market_price,
    max_iterations = max_iterations, verbose = FALSE
  )
  do.call(calculate_oas, c(args, oas_result$options))
}

#' @title Scale a Prepayment Model (Internal)
#' @description Multiplies whichever parameter carries prepayment speed for the
#'   model's type. Shared by \code{\link{run_oas_sensitivity}} and
#'   \code{\link{create_oas_surface}}, which previously held identical copies.
#'
#' @keywords internal
#' @noRd
.modify_prepay_model <- function(model, multiplier) {
  new_model <- model

  if (model$type == "cpr_cdr") {
    # Capped below 1: a multiplier of 2 on a fast base speed would otherwise
    # leave the annual-to-monthly conversion undefined.
    new_model$base_cpr <- min(model$base_cpr * multiplier, 1 - 1e-12)
  } else if (model$type == "psa") {
    new_model$psa_multiplier <- model$psa_multiplier * multiplier
  } else if (model$type == "survival") {
    new_model$prepay_hazard <- model$prepay_hazard * multiplier
  }

  new_model
}

#' @title Run OAS Sensitivity Analysis
#' @description Runs sensitivity analysis for OAS by varying key parameters.
#'
#' @param oas_result An object of class "oas_result"
#' @param sensitivity_type Character. Type of sensitivity to run: "rates", "prepayments", "both"
#' @param rate_shifts Numeric vector. Rate shifts in basis points to analyze
#' @param prepay_multipliers Numeric vector. Prepayment speed multipliers to analyze
#' @param max_iterations Integer. Maximum iterations for each OAS calculation
#' @param verbose Logical. Whether to print progress information
#'
#' @return A list containing sensitivity analysis results
#'
#' @examples
#' \dontrun{
#' # Calculate OAS
#' oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)
#' 
#' # Run sensitivity analysis
#' sensitivity <- run_oas_sensitivity(oas_result)
#' 
#' # Plot results
#' plot(sensitivity)
#' }
#'
#' @export
run_oas_sensitivity <- function(oas_result, sensitivity_type = "both",
                              rate_shifts = c(-200, -100, -50, 0, 50, 100, 200),
                              prepay_multipliers = c(0.5, 0.75, 1.0, 1.25, 1.5, 2.0),
                              max_iterations = 20, verbose = TRUE) {
  
  # Parameter validation
  checkmate::assert_class(oas_result, "oas_result")
  checkmate::assert_choice(sensitivity_type, c("rates", "prepayments", "both"))
  checkmate::assert_numeric(rate_shifts)
  checkmate::assert_numeric(prepay_multipliers, lower = 0)
  checkmate::assert_count(max_iterations, positive = TRUE)
  
  # Extract required data from oas_result
  asset_data <- oas_result$asset_data
  rate_paths <- oas_result$rate_paths
  prepay_model <- oas_result$prepay_model
  market_price <- oas_result$market_price
  
  # Initialize results data frames
  rate_results <- NULL
  prepay_results <- NULL
  
  # ---- Interest Rate Shifts ----
  if (sensitivity_type %in% c("rates", "both")) {
    if (verbose) {
      cat("Running interest rate sensitivity analysis...\n")
    }
    
    rate_results <- data.frame(
      shift = rate_shifts,
      oas = numeric(length(rate_shifts)),
      price = numeric(length(rate_shifts)),
      wal = numeric(length(rate_shifts)),
      duration = numeric(length(rate_shifts)),
      convexity = numeric(length(rate_shifts))
    )
    
    for (i in seq_along(rate_shifts)) {
      shift <- rate_shifts[i]
      if (verbose) {
        cat("  Calculating for rate shift of", shift, "bps...\n")
      }
      
      # Shift yield curves
      shifted_paths <- rate_paths
      for (key in names(rate_paths$yield_paths)) {
        shifted_paths$yield_paths[[key]] <- rate_paths$yield_paths[[key]] + shift/10000
      }
      
      # Recalculate OAS with fixed market price
      result <- .resolve_oas(oas_result, asset_data, shifted_paths, prepay_model, market_price, max_iterations)
      
      # Store results
      rate_results$oas[i] <- result$oas
      rate_results$price[i] <- result$price
      rate_results$wal[i] <- result$avg_life$avg_lives$overall_wal
      rate_results$duration[i] <- mean(result$duration$duration)
      rate_results$convexity[i] <- mean(result$duration$convexity)
    }
  }
  
  # ---- Prepayment Speed Multipliers ----
  if (sensitivity_type %in% c("prepayments", "both")) {
    if (verbose) {
      cat("Running prepayment sensitivity analysis...\n")
    }
    
    prepay_results <- data.frame(
      multiplier = prepay_multipliers,
      oas = numeric(length(prepay_multipliers)),
      price = numeric(length(prepay_multipliers)),
      wal = numeric(length(prepay_multipliers)),
      duration = numeric(length(prepay_multipliers)),
      convexity = numeric(length(prepay_multipliers))
    )
    
    for (i in seq_along(prepay_multipliers)) {
      multiplier <- prepay_multipliers[i]
      if (verbose) {
        cat("  Calculating for prepayment multiplier of", multiplier, "...\n")
      }
      
      # Modify prepayment model
      modified_model <- .modify_prepay_model(prepay_model, multiplier)
      
      # Recalculate OAS with fixed market price
      result <- .resolve_oas(oas_result, asset_data, rate_paths, modified_model, market_price, max_iterations)
      
      # Store results
      prepay_results$oas[i] <- result$oas
      prepay_results$price[i] <- result$price
      prepay_results$wal[i] <- result$avg_life$avg_lives$overall_wal
      prepay_results$duration[i] <- mean(result$duration$duration)
      prepay_results$convexity[i] <- mean(result$duration$convexity)
    }
  }
  
  # Create result object
  result <- list(
    rate_sensitivity = rate_results,
    prepay_sensitivity = prepay_results,
    base_oas = oas_result$oas,
    base_price = oas_result$price,
    base_wal = oas_result$avg_life$avg_lives$overall_wal,
    base_duration = mean(oas_result$duration$duration),
    base_convexity = mean(oas_result$duration$convexity)
  )
  
  class(result) <- c("oas_sensitivity", class(result))
  
  return(result)
}

#' @title Print Method for OAS Sensitivity Analysis
#' @description Prints a summary of the OAS sensitivity analysis.
#'
#' @param x An object of class "oas_sensitivity"
#' @param ... Additional arguments (not used)
#'
#' @return The object invisibly
#'
#' @export
#' @method print oas_sensitivity
print.oas_sensitivity <- function(x, ...) {
  cat("OAS Sensitivity Analysis\n")
  cat("----------------------\n")
  cat("Base OAS:", round(x$base_oas, 2), "bps\n")
  cat("Base Price:", round(x$base_price, 4), "\n")
  cat("Base WAL:", round(x$base_wal, 2), "years\n")
  
  if (!is.null(x$rate_sensitivity)) {
    cat("\nInterest Rate Sensitivity:\n")
    print(x$rate_sensitivity[, c("shift", "oas", "price", "wal")])
  }
  
  if (!is.null(x$prepay_sensitivity)) {
    cat("\nPrepayment Sensitivity:\n")
    print(x$prepay_sensitivity[, c("multiplier", "oas", "price", "wal")])
  }
  
  invisible(x)
}

#' @title Plot Method for OAS Sensitivity Analysis
#' @description Creates plots to visualize OAS sensitivity analysis results.
#'
#' @param x An object of class "oas_sensitivity"
#' @param ... Additional arguments (not used)
#'
#' @return A list of ggplot objects
#'
#' @export
#' @method plot oas_sensitivity
plot.oas_sensitivity <- function(x, ...) {
  # Initialize list to store plots
  plots <- list()
  
  # ---- Interest Rate Sensitivity Plots ----
  if (!is.null(x$rate_sensitivity)) {
    # OAS vs Rate Shift
    p1 <- ggplot2::ggplot(x$rate_sensitivity, ggplot2::aes(x = shift, y = oas)) +
      ggplot2::geom_line(linewidth = 1, color = "blue") +
      ggplot2::geom_point(size = 3, color = "blue") +
      ggplot2::labs(
        title = "OAS Sensitivity to Interest Rate Shifts",
        x = "Interest Rate Shift (bps)",
        y = "OAS (bps)"
      ) +
      ggplot2::theme_minimal()
    
    plots$oas_rate <- p1
    
    # WAL vs Rate Shift
    p2 <- ggplot2::ggplot(x$rate_sensitivity, ggplot2::aes(x = shift, y = wal)) +
      ggplot2::geom_line(linewidth = 1, color = "red") +
      ggplot2::geom_point(size = 3, color = "red") +
      ggplot2::labs(
        title = "WAL Sensitivity to Interest Rate Shifts",
        x = "Interest Rate Shift (bps)",
        y = "Weighted Average Life (years)"
      ) +
      ggplot2::theme_minimal()
    
    plots$wal_rate <- p2
    
    # Duration vs Rate Shift
    if ("duration" %in% names(x$rate_sensitivity)) {
      p3 <- ggplot2::ggplot(x$rate_sensitivity, ggplot2::aes(x = shift, y = duration)) +
        ggplot2::geom_line(linewidth = 1, color = "purple") +
        ggplot2::geom_point(size = 3, color = "purple") +
        ggplot2::labs(
          title = "Duration Sensitivity to Interest Rate Shifts",
          x = "Interest Rate Shift (bps)",
          y = "Effective Duration"
        ) +
        ggplot2::theme_minimal()
      
      plots$duration_rate <- p3
    }
  }
  
  # ---- Prepayment Sensitivity Plots ----
  if (!is.null(x$prepay_sensitivity)) {
    # OAS vs Prepayment Multiplier
    p4 <- ggplot2::ggplot(x$prepay_sensitivity, ggplot2::aes(x = multiplier, y = oas)) +
      ggplot2::geom_line(linewidth = 1, color = "blue") +
      ggplot2::geom_point(size = 3, color = "blue") +
      ggplot2::labs(
        title = "OAS Sensitivity to Prepayment Speed",
        x = "Prepayment Speed Multiplier",
        y = "OAS (bps)"
      ) +
      ggplot2::theme_minimal()
    
    plots$oas_prepay <- p4
    
    # WAL vs Prepayment Multiplier
    p5 <- ggplot2::ggplot(x$prepay_sensitivity, ggplot2::aes(x = multiplier, y = wal)) +
      ggplot2::geom_line(linewidth = 1, color = "red") +
      ggplot2::geom_point(size = 3, color = "red") +
      ggplot2::labs(
        title = "WAL Sensitivity to Prepayment Speed",
        x = "Prepayment Speed Multiplier",
        y = "Weighted Average Life (years)"
      ) +
      ggplot2::theme_minimal()
    
    plots$wal_prepay <- p5
    
    # Duration vs Prepayment Multiplier
    if ("duration" %in% names(x$prepay_sensitivity)) {
      p6 <- ggplot2::ggplot(x$prepay_sensitivity, ggplot2::aes(x = multiplier, y = duration)) +
        ggplot2::geom_line(linewidth = 1, color = "purple") +
        ggplot2::geom_point(size = 3, color = "purple") +
        ggplot2::labs(
          title = "Duration Sensitivity to Prepayment Speed",
          x = "Prepayment Speed Multiplier",
          y = "Effective Duration"
        ) +
        ggplot2::theme_minimal()
      
      plots$duration_prepay <- p6
    }
  }
  
  # Create a grid layout if there are multiple plots
  if (length(plots) > 1) {
    return(plots)
  } else if (length(plots) == 1) {
    return(plots[[1]])
  } else {
    return(NULL)
  }
}

#' @title Create OAS Surface Analysis
#' @description Creates a surface analysis of OAS across two dimensions:
#' interest rate shifts and prepayment multipliers.
#'
#' @param oas_result An object of class "oas_result"
#' @param rate_shifts Numeric vector. Rate shifts in basis points to analyze
#' @param prepay_multipliers Numeric vector. Prepayment speed multipliers to analyze
#' @param max_iterations Integer. Maximum iterations for each OAS calculation
#' @param verbose Logical. Whether to print progress information
#'
#' @return An object of class "oas_surface"
#'
#' @export
create_oas_surface <- function(oas_result, 
                              rate_shifts = seq(-200, 200, by = 100),
                              prepay_multipliers = seq(0.5, 2.0, by = 0.5),
                              max_iterations = 20, verbose = TRUE) {
  
  # Parameter validation
  checkmate::assert_class(oas_result, "oas_result")
  checkmate::assert_numeric(rate_shifts)
  checkmate::assert_numeric(prepay_multipliers, lower = 0)
  checkmate::assert_count(max_iterations, positive = TRUE)
  
  # Extract required data from oas_result
  asset_data <- oas_result$asset_data
  rate_paths <- oas_result$rate_paths
  prepay_model <- oas_result$prepay_model
  market_price <- oas_result$market_price
  
  # Initialize results matrix
  n_shifts <- length(rate_shifts)
  n_multipliers <- length(prepay_multipliers)
  
  oas_matrix <- matrix(NA, nrow = n_shifts, ncol = n_multipliers)
  price_matrix <- matrix(NA, nrow = n_shifts, ncol = n_multipliers)
  wal_matrix <- matrix(NA, nrow = n_shifts, ncol = n_multipliers)
  
  # Run analysis for each combination
  total_combinations <- n_shifts * n_multipliers
  combination_counter <- 0
  
  for (i in seq_along(rate_shifts)) {
    shift <- rate_shifts[i]
    
    # Shift yield curves
    shifted_paths <- rate_paths
    for (key in names(rate_paths$yield_paths)) {
      shifted_paths$yield_paths[[key]] <- rate_paths$yield_paths[[key]] + shift/10000
    }
    
    for (j in seq_along(prepay_multipliers)) {
      multiplier <- prepay_multipliers[j]
      
      combination_counter <- combination_counter + 1
      if (verbose) {
        cat(sprintf("Processing combination %d of %d: Rate shift = %d bps, Prepay multiplier = %.2f\n",
                    combination_counter, total_combinations, shift, multiplier))
      }
      
      # Modify prepayment model
      modified_model <- .modify_prepay_model(prepay_model, multiplier)
      
      # Calculate OAS with fixed market price
      result <- try(.resolve_oas(oas_result, asset_data, shifted_paths, modified_model, market_price, max_iterations), silent = TRUE)
      
      # Store results if successful
      if (!inherits(result, "try-error")) {
        oas_matrix[i, j] <- result$oas
        price_matrix[i, j] <- result$price
        wal_matrix[i, j] <- result$avg_life$avg_lives$overall_wal
      } else {
        if (verbose) {
          cat("  Error in OAS calculation for this combination\n")
        }
      }
    }
  }
  
  # Convert to data frames for easier plotting
  surface_data <- expand.grid(
    shift = rate_shifts,
    multiplier = prepay_multipliers
  )
  
  surface_data$oas <- as.vector(oas_matrix)
  surface_data$price <- as.vector(price_matrix)
  surface_data$wal <- as.vector(wal_matrix)
  
  # Create result object
  result <- list(
    surface_data = surface_data,
    oas_matrix = oas_matrix,
    price_matrix = price_matrix,
    wal_matrix = wal_matrix,
    rate_shifts = rate_shifts,
    prepay_multipliers = prepay_multipliers,
    base_oas = oas_result$oas,
    base_price = oas_result$price,
    base_wal = oas_result$avg_life$avg_lives$overall_wal
  )
  
  class(result) <- c("oas_surface", class(result))
  
  return(result)
}

#' @title Plot Method for OAS Surface Analysis
#' @description Creates plots to visualize OAS surface analysis results.
#'
#' @param x An object of class "oas_surface"
#' @param type Character. Type of plot: "oas", "price", "wal", or "all"
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object or list of ggplot objects
#'
#' @export
#' @method plot oas_surface
plot.oas_surface <- function(x, type = "oas", ...) {
  # Parameter validation
  checkmate::assert_choice(type, c("oas", "price", "wal", "all"))
  
  # Initialize list to store plots
  plots <- list()
  
  if (type == "oas" || type == "all") {
    # OAS surface plot
    p1 <- ggplot2::ggplot(x$surface_data, ggplot2::aes(x = shift, y = multiplier, z = oas)) +
      ggplot2::geom_tile(ggplot2::aes(fill = oas)) +
      ggplot2::scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red", 
        midpoint = x$base_oas
      ) +
      ggplot2::labs(
        title = "OAS Surface Analysis",
        x = "Interest Rate Shift (bps)",
        y = "Prepayment Speed Multiplier",
        fill = "OAS (bps)"
      ) +
      ggplot2::theme_minimal()
    
    plots$oas <- p1
  }
  
  if (type == "price" || type == "all") {
    # Price surface plot
    p2 <- ggplot2::ggplot(x$surface_data, ggplot2::aes(x = shift, y = multiplier, z = price)) +
      ggplot2::geom_tile(ggplot2::aes(fill = price)) +
      ggplot2::scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red", 
        midpoint = x$base_price
      ) +
      ggplot2::labs(
        title = "Price Surface Analysis",
        x = "Interest Rate Shift (bps)",
        y = "Prepayment Speed Multiplier",
        fill = "Price"
      ) +
      ggplot2::theme_minimal()
    
    plots$price <- p2
  }
  
  if (type == "wal" || type == "all") {
    # WAL surface plot
    p3 <- ggplot2::ggplot(x$surface_data, ggplot2::aes(x = shift, y = multiplier, z = wal)) +
      ggplot2::geom_tile(ggplot2::aes(fill = wal)) +
      ggplot2::scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red", 
        midpoint = x$base_wal
      ) +
      ggplot2::labs(
        title = "WAL Surface Analysis",
        x = "Interest Rate Shift (bps)",
        y = "Prepayment Speed Multiplier",
        fill = "WAL (years)"
      ) +
      ggplot2::theme_minimal()
    
    plots$wal <- p3
  }
  
  # Return appropriate plot(s)
  if (type == "all") {
    return(plots)
  } else {
    return(plots[[type]])
  }
}
