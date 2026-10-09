#' @title Plot Cash Flows
#' @description Creates plots of cash flows from OAS analysis.
#'
#' @param oas_result An object of class "oas_result"
#' @param asset_idx Integer or NULL. Specific asset index to plot, NULL for total
#' @param components Character vector. Which cash flow components to include
#' @param average_paths Logical. Whether to average across paths
#' @param seed Integer. Optional seed for the path sample, so the plot reproduces
#'
#' @return A ggplot object
#'
#' @export
plot_cashflows <- function(oas_result, asset_idx = NULL,
                          components = c("scheduled_principal", "prepayment", "interest", "recovery"),
                          average_paths = TRUE, seed = NULL) {
  # Fix the sampled paths when asked, leaving the caller's random stream as it
  # was found.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      saved_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }

  # Parameter validation
  checkmate::assert_class(oas_result, "oas_result")
  if (!is.null(asset_idx)) {
    checkmate::assert_int(asset_idx, lower = 1, upper = nrow(oas_result$asset_data))
  }

  # Extract cashflows
  cashflows <- oas_result$cashflows

  # Component positions come from the one place they are written down.
  component_idx <- as.list(cashflow_components("asset"))

  # Filter to requested components
  components <- intersect(components, names(component_idx))

  # Get dimensions
  n_paths <- length(cashflows)
  n_periods <- dim(cashflows[[1]])[1]
  n_assets <- dim(cashflows[[1]])[3]

  # Convert to data frame for ggplot
  plot_data <- data.frame()

  if (average_paths) {
    # Average across all paths
    if (is.null(asset_idx)) {
      # Sum across all assets
      for (comp in components) {
        comp_idx <- component_idx[[comp]]

        # Sum across all assets for each path
        path_sums <- matrix(0, n_paths, n_periods)

        for (i in 1:n_paths) {
          for (a in 1:n_assets) {
            path_sums[i, ] <- path_sums[i, ] + cashflows[[i]][, comp_idx, a]
          }
        }

        # Calculate mean across paths
        mean_flow <- colMeans(path_sums)

        # Add to data frame
        comp_data <- data.frame(
          period = 1:n_periods,
          time = (1:n_periods) / 12,
          amount = mean_flow,
          component = comp
        )

        plot_data <- rbind(plot_data, comp_data)
      }
    } else {
      # Specific asset
      for (comp in components) {
        comp_idx <- component_idx[[comp]]

        # Extract cash flows for this asset across all paths
        path_values <- matrix(0, n_paths, n_periods)

        for (i in 1:n_paths) {
          path_values[i, ] <- cashflows[[i]][, comp_idx, asset_idx]
        }

        # Calculate mean across paths
        mean_flow <- colMeans(path_values)

        # Add to data frame
        comp_data <- data.frame(
          period = 1:n_periods,
          time = (1:n_periods) / 12,
          amount = mean_flow,
          component = comp
        )

        plot_data <- rbind(plot_data, comp_data)
      }
    }

    # Create plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = amount, fill = component)) +
      ggplot2::geom_area(position = "stack") +
      ggplot2::labs(
        title = if (is.null(asset_idx)) "Average Cash Flows Across All Assets" else paste("Average Cash Flows for Asset", asset_idx),
        x = "Time (years)",
        y = "Amount ($)",
        fill = "Cash Flow Component"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

  } else {
    # Plot a sample of paths
    sample_paths <- sample(1:n_paths, min(5, n_paths))

    if (is.null(asset_idx)) {
      # Sum across all assets
      for (i in sample_paths) {
        for (comp in components) {
          comp_idx <- component_idx[[comp]]

          # Sum across all assets
          asset_sum <- numeric(n_periods)
          for (a in 1:n_assets) {
            asset_sum <- asset_sum + cashflows[[i]][, comp_idx, a]
          }

          # Add to data frame
          comp_data <- data.frame(
            period = 1:n_periods,
            time = (1:n_periods) / 12,
            amount = asset_sum,
            component = comp,
            path = paste("Path", i)
          )

          plot_data <- rbind(plot_data, comp_data)
        }
      }
    } else {
      # Specific asset
      for (i in sample_paths) {
        for (comp in components) {
          comp_idx <- component_idx[[comp]]

          # Extract cash flows for this asset
          flow <- cashflows[[i]][, comp_idx, asset_idx]

          # Add to data frame
          comp_data <- data.frame(
            period = 1:n_periods,
            time = (1:n_periods) / 12,
            amount = flow,
            component = comp,
            path = paste("Path", i)
          )

          plot_data <- rbind(plot_data, comp_data)
        }
      }
    }

    # Create plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = amount, fill = component)) +
      ggplot2::geom_area(position = "stack") +
      ggplot2::facet_wrap(~path) +
      ggplot2::labs(
        title = if (is.null(asset_idx)) "Cash Flows by Path Across All Assets" else paste("Cash Flows by Path for Asset", asset_idx),
        x = "Time (years)",
        y = "Amount ($)",
        fill = "Cash Flow Component"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
  }

  return(p)
}

#' @title Plot Rate Paths
#' @description Creates plots of interest rate paths.
#'
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param maturity Character. Which maturity to plot (e.g., "10" for 10-year)
#' @param sample_size Integer. Number of paths to sample
#' @param seed Integer. Optional seed for the path sample, so the plot reproduces
#'
#' @return A ggplot object
#'
#' @export
plot_rate_paths <- function(rate_paths, maturity = "10", sample_size = 10, seed = NULL) {
  # Fix the sampled paths when asked, leaving the caller's random stream as it
  # was found.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      saved_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }

  # Parameter validation
  checkmate::assert_list(rate_paths)
  checkmate::assert_character(maturity)
  checkmate::assert_int(sample_size, lower = 1)

  # Check if the requested maturity exists
  if (!maturity %in% names(rate_paths$yield_paths)) {
    stop("Maturity '", maturity, "' not found in rate_paths. ",
         "Available maturities: ", paste(names(rate_paths$yield_paths), collapse = ", "))
  }

  # Extract rate matrix
  rate_matrix <- rate_paths$yield_paths[[maturity]]

  # Get dimensions
  n_paths <- nrow(rate_matrix)
  n_periods <- ncol(rate_matrix)

  # Sample paths
  sample_paths <- sample(1:n_paths, min(sample_size, n_paths))

  # Convert to data frame for ggplot
  plot_data <- data.frame()

  for (i in sample_paths) {
    path_data <- data.frame(
      period = 1:n_periods,
      time = (1:n_periods) / 12,
      rate = rate_matrix[i, ] * 100,  # Convert to percentage
      path = paste("Path", i)
    )

    plot_data <- rbind(plot_data, path_data)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = rate, color = path, group = path)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = paste(maturity, "Year Treasury Rate Paths"),
      x = "Time (years)",
      y = "Rate (%)",
      color = "Path"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  return(p)
}

#' @title Plot Yield Curves
#' @description Creates plots of yield curves at different time points.
#'
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param time_points Numeric vector. Time points (in years) to plot
#' @param path_idx Integer. Which path to plot
#'
#' @return A ggplot object
#'
#' @export
plot_yield_curves <- function(rate_paths, time_points = c(0, 1, 5, 10), path_idx = 1) {

  # Parameter validation
  checkmate::assert_list(rate_paths)
  checkmate::assert_numeric(time_points)
  checkmate::assert_int(path_idx, lower = 1)

  # Get available maturities and convert to numeric
  maturities <- as.numeric(names(rate_paths$yield_paths))

  # Get sorted maturity names
  maturity_keys <- names(rate_paths$yield_paths)[order(maturities)]

  # Check if path_idx is valid
  first_matrix <- rate_paths$yield_paths[[maturity_keys[1]]]
  n_paths <- nrow(first_matrix)

  if (path_idx > n_paths) {
    stop("path_idx (", path_idx, ") exceeds number of paths (", n_paths, ")")
  }

  # Get number of periods
  n_periods <- ncol(first_matrix)

  # Convert time points to period indices
  period_indices <- round(time_points * 12)
  period_indices <- period_indices[period_indices >= 1 & period_indices <= n_periods]

  if (length(period_indices) == 0) {
    stop("All time_points are outside the valid range (0 to ", n_periods/12, " years)")
  }

  # Create data frame for plotting
  plot_data <- data.frame()

  for (period in period_indices) {
    for (mat_key in maturity_keys) {
      maturity <- as.numeric(mat_key)

      # Extract rate for this maturity, path, and period
      rate <- rate_paths$yield_paths[[mat_key]][path_idx, period]

      # Add to data frame
      curve_data <- data.frame(
        maturity = maturity,
        rate = rate * 100,  # Convert to percentage
        time_point = paste("Year", period/12)
      )

      plot_data <- rbind(plot_data, curve_data)
    }
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = maturity, y = rate, color = time_point, group = time_point)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = paste("Yield Curves at Different Time Points (Path", path_idx, ")"),
      x = "Maturity (years)",
      y = "Yield (%)",
      color = "Time Point"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}

#' @title Plot Prepayment Rates
#' @description Creates plots of prepayment rates from OAS analysis.
#'
#' @param oas_result An object of class "oas_result"
#' @param asset_idx Integer or NULL. Specific asset index to plot, NULL for average
#' @param type Character. Type of prepayment to plot: "prepay", "default", "both"
#' @param sample_paths Integer. Number of paths to sample
#' @param seed Integer. Optional seed for the path sample, so the plot reproduces
#'
#' @return A ggplot object or list of ggplot objects
#'
#' @export
plot_prepayment_rates <- function(oas_result, asset_idx = NULL,
                                  type = "both", sample_paths = 5, seed = NULL) {
  # Fix the sampled paths when asked, leaving the caller's random stream as it
  # was found.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      saved_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }
  # Parameter validation
  checkmate::assert_class(oas_result, "oas_result")
  if (!is.null(asset_idx)) {
    checkmate::assert_int(asset_idx, lower = 1, upper = nrow(oas_result$asset_data))
  }
  checkmate::assert_choice(type, c("prepay", "default", "both"))
  checkmate::assert_int(sample_paths, lower = 1)

  # Extract credit vectors
  credit_vectors <- oas_result$credit_vectors

  # Get dimensions
  n_paths <- dim(credit_vectors$prepay_rates)[1]
  n_periods <- dim(credit_vectors$prepay_rates)[2]
  n_assets <- dim(credit_vectors$prepay_rates)[3]

  # Initialize list to store plots
  plots <- list()

  # Sample paths
  sample_path_indices <- sample(1:n_paths, min(sample_paths, n_paths))

  # Process prepayment rates
  if (type == "prepay" || type == "both") {
    # Convert to data frame for ggplot
    prepay_data <- data.frame()

    if (is.null(asset_idx)) {
      # Average across all assets
      for (p in sample_path_indices) {
        # Calculate average across assets
        avg_prepay <- rowMeans(matrix(credit_vectors$prepay_rates[p, , ], nrow = n_periods))

        # FIX: Ensure dimensions match
        period_seq <- seq_len(length(avg_prepay))
        time_seq <- period_seq / 12

        # Add to data frame
        path_data <- data.frame(
          period = period_seq,
          time = time_seq,
          rate = avg_prepay * 100,  # Convert to percentage (SMM)
          path = paste("Path", p)
        )

        prepay_data <- rbind(prepay_data, path_data)
      }

      # Also calculate the mean across all paths and assets
      mean_across_paths <- numeric(n_periods)
      for (t in 1:n_periods) {
        mean_across_paths[t] <- mean(credit_vectors$prepay_rates[, t, ])
      }

      # FIX: Ensure dimensions match
      period_seq <- seq_len(length(mean_across_paths))
      time_seq <- period_seq / 12

      # Add to data frame
      mean_data <- data.frame(
        period = period_seq,
        time = time_seq,
        rate = mean_across_paths * 100,  # Convert to percentage
        path = "Mean"
      )

      prepay_data <- rbind(prepay_data, mean_data)

      # Create plot
      p_prepay <- ggplot2::ggplot(prepay_data, ggplot2::aes(x = time, y = rate, color = path, group = path)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "Prepayment Rates Across All Assets",
          x = "Time (years)",
          y = "SMM (%)",
          color = "Path"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

    } else {
      # Specific asset
      for (p in sample_path_indices) {
        # Extract prepayment rates for this asset
        asset_prepay <- credit_vectors$prepay_rates[p, , asset_idx]

        # FIX: Ensure dimensions match
        period_seq <- seq_len(length(asset_prepay))
        time_seq <- period_seq / 12

        # Add to data frame
        path_data <- data.frame(
          period = period_seq,
          time = time_seq,
          rate = asset_prepay * 100,  # Convert to percentage
          path = paste("Path", p)
        )

        prepay_data <- rbind(prepay_data, path_data)
      }

      # Also calculate the mean across all paths for this asset
      mean_across_paths <- numeric(n_periods)
      for (t in 1:n_periods) {
        mean_across_paths[t] <- mean(credit_vectors$prepay_rates[, t, asset_idx])
      }

      # FIX: Ensure dimensions match
      period_seq <- seq_len(length(mean_across_paths))
      time_seq <- period_seq / 12

      # Add to data frame
      mean_data <- data.frame(
        period = period_seq,
        time = time_seq,
        rate = mean_across_paths * 100,  # Convert to percentage
        path = "Mean"
      )

      prepay_data <- rbind(prepay_data, mean_data)

      # Create plot
      p_prepay <- ggplot2::ggplot(prepay_data, ggplot2::aes(x = time, y = rate, color = path, group = path)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = paste("Prepayment Rates for Asset", asset_idx),
          x = "Time (years)",
          y = "SMM (%)",
          color = "Path"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    }

    plots$prepay <- p_prepay
  }

  # Process default rates (same fix needed here)
  if (type == "default" || type == "both") {
    # Convert to data frame for ggplot
    default_data <- data.frame()

    if (is.null(asset_idx)) {
      # Average across all assets
      for (p in sample_path_indices) {
        # Calculate average across assets
        avg_default <- rowMeans(matrix(credit_vectors$default_rates[p, , ], nrow = n_periods))

        # FIX: Ensure dimensions match
        period_seq <- seq_len(length(avg_default))
        time_seq <- period_seq / 12

        # Add to data frame
        path_data <- data.frame(
          period = period_seq,
          time = time_seq,
          rate = avg_default * 100,  # Convert to percentage (MDR)
          path = paste("Path", p)
        )

        default_data <- rbind(default_data, path_data)
      }

      # Also calculate the mean across all paths and assets
      mean_across_paths <- numeric(n_periods)
      for (t in 1:n_periods) {
        mean_across_paths[t] <- mean(credit_vectors$default_rates[, t, ])
      }

      # FIX: Ensure dimensions match
      period_seq <- seq_len(length(mean_across_paths))
      time_seq <- period_seq / 12

      # Add to data frame
      mean_data <- data.frame(
        period = period_seq,
        time = time_seq,
        rate = mean_across_paths * 100,  # Convert to percentage
        path = "Mean"
      )

      default_data <- rbind(default_data, mean_data)

      # Create plot
      p_default <- ggplot2::ggplot(default_data, ggplot2::aes(x = time, y = rate, color = path, group = path)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "Default Rates Across All Assets",
          x = "Time (years)",
          y = "MDR (%)",
          color = "Path"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

    } else {
      # Specific asset
      for (p in sample_path_indices) {
        # Extract default rates for this asset
        asset_default <- credit_vectors$default_rates[p, , asset_idx]

        # FIX: Ensure dimensions match
        period_seq <- seq_len(length(asset_default))
        time_seq <- period_seq / 12

        # Add to data frame
        path_data <- data.frame(
          period = period_seq,
          time = time_seq,
          rate = asset_default * 100,  # Convert to percentage
          path = paste("Path", p)
        )

        default_data <- rbind(default_data, path_data)
      }

      # Also calculate the mean across all paths for this asset
      mean_across_paths <- numeric(n_periods)
      for (t in 1:n_periods) {
        mean_across_paths[t] <- mean(credit_vectors$default_rates[, t, asset_idx])
      }

      # FIX: Ensure dimensions match
      period_seq <- seq_len(length(mean_across_paths))
      time_seq <- period_seq / 12

      # Add to data frame
      mean_data <- data.frame(
        period = period_seq,
        time = time_seq,
        rate = mean_across_paths * 100,  # Convert to percentage
        path = "Mean"
      )

      default_data <- rbind(default_data, mean_data)

      # Create plot
      p_default <- ggplot2::ggplot(default_data, ggplot2::aes(x = time, y = rate, color = path, group = path)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = paste("Default Rates for Asset", asset_idx),
          x = "Time (years)",
          y = "MDR (%)",
          color = "Path"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    }

    plots$default <- p_default
  }

  # Return appropriate plot(s)
  if (type == "both") {
    return(plots)
  } else if (type == "prepay") {
    return(plots$prepay)
  } else {
    return(plots$default)
  }
}

#' @title Create OAS Report
#' @description Creates a comprehensive report of OAS analysis results.
#'
#' @param oas_result An object of class "oas_result"
#' @param output_format Character. Output format: "html", "pdf", or "word"
#' @param output_file Character. Output file name
#' @param title Character. Report title
#' @param author Character. Report author
#'
#' @return The path to the generated report file
#'
#' @examples
#' \dontrun{
#' oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)
#' report <- create_oas_report(oas_result, output_format = "html", output_file = "oas_report.html")
#' }
#'
#' @export
create_oas_report <- function(oas_result, output_format = "html",
                             output_file = NULL, title = "OAS Analysis Report",
                             author = "") {

  # Parameter validation
  checkmate::assert_class(oas_result, "oas_result")
  checkmate::assert_choice(output_format, c("html", "pdf", "word"))

  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required to create reports. Please install it.")
  }

  # Create a temporary Rmd file
  rmd_file <- tempfile(fileext = ".Rmd")

  # Generate report content
  report_content <- generate_report_content(oas_result, title, author)

  # Write to file
  writeLines(report_content, rmd_file)

  # Set output format
  output_format_fn <- switch(output_format,
                             html = rmarkdown::html_document,
                             pdf = rmarkdown::pdf_document,
                             word = rmarkdown::word_document)

  # Generate output file name if not provided
  if (is.null(output_file)) {
    output_file <- paste0("oas_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", output_format)
  }

  # Render the report
  output_dir <- dirname(output_file)
  if (output_dir == ".") {
    output_dir <- getwd()
  }

  report_file <- rmarkdown::render(
    input = rmd_file,
    output_dir = output_dir,
    output_format = output_format_fn(
      toc = TRUE,
      toc_depth = 3,
      number_sections = TRUE,
      fig_width = 10,
      fig_height = 7
    ),
    output_file = basename(output_file),
    envir = new.env()
  )

  # Return the path to the generated report
  return(report_file)
}

# Helper function to generate report content
generate_report_content <- function(oas_result, title, author) {
  # Create YAML header
  yaml_header <- paste0("---\n",
                        "title: \"", title, "\"\n",
                        "author: \"", author, "\"\n",
                        "date: \"", format(Sys.Date(), "%B %d, %Y"), "\"\n",
                        "output: html_document\n",
                        "---\n\n")

  # Create setup chunk
  setup_chunk <- paste0("```{r setup, include=FALSE}\n",
                        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
                        "library(oasr)\n",
                        "library(ggplot2)\n",
                        "library(knitr)\n",
                        "library(dplyr)\n",
                        "```\n\n")

  # Save the result, then point the chunk at the path actually written. The
  # path is bound once and reused; recovering it from the chunk text by regex
  # is what used to strand a trailing quote on the filename.
  data_file <- tempfile(fileext = ".rds")
  saveRDS(oas_result, data_file)

  oas_chunk <- paste0("```{r load_data}\n",
                      "oas_result <- readRDS(",
                      encodeString(data_file, quote = "'"), ")\n",
                      "```\n\n")

  # Create sections
  summary_section <- paste0("# Executive Summary\n\n",
                           "This report presents the results of an Option-Adjusted Spread (OAS) analysis.\n\n",
                           "```{r summary_table}\n",
                           "summary_df <- summary(oas_result)$overall\n",
                           "kable(summary_df, caption = 'OAS Analysis Results')\n",
                           "```\n\n")

  input_section <- paste0("# Input Data\n\n",
                         "## Asset Characteristics\n\n",
                         "```{r asset_table}\n",
                         "kable(oas_result$asset_data, caption = 'Asset Characteristics')\n",
                         "```\n\n",
                         "## Prepayment Model\n\n",
                         "```{r prepay_model}\n",
                         "prepay_model <- oas_result$prepay_model\n",
                         "# One row per parameter. sapply() here returned a list,\n",
                         "# because seasonal_factors is twelve long while every other\n",
                         "# element is one, and data.frame() then expanded that list\n",
                         "# into columns and demanded twelve rows against nine names.\n",
                         "prepay_df <- data.frame(\n",
                         "  Parameter = names(prepay_model),\n",
                         "  Value = vapply(prepay_model, function(x) {\n",
                         "    if (is.atomic(x)) paste(format(x), collapse = ', ')\n",
                         "    else paste(class(x), collapse = ', ')\n",
                         "  }, character(1)),\n",
                         "  stringsAsFactors = FALSE\n",
                         ")\n",
                         "kable(prepay_df[1:min(nrow(prepay_df), 10), ], caption = 'Prepayment Model Parameters')\n",
                         "```\n\n")

  results_section <- paste0("# OAS Results\n\n",
                           "## Cash Flows\n\n",
                           "```{r cashflow_plot}\n",
                           "plot_cashflows(oas_result)\n",
                           "```\n\n",
                           "## Prepayment Rates\n\n",
                           "```{r prepay_plot}\n",
                           "plot_prepayment_rates(oas_result, type = 'prepay')\n",
                           "```\n\n",
                           "## Default Rates\n\n",
                           "```{r default_plot}\n",
                           "plot_prepayment_rates(oas_result, type = 'default')\n",
                           "```\n\n",
                           "## Rate Paths\n\n",
                           "```{r rate_plot}\n",
                           "plot_rate_paths(oas_result$rate_paths)\n",
                           "```\n\n")

  analytics_section <- paste0("# Risk Analytics\n\n",
                              "## Weighted Average Life\n\n",
                              "```{r wal}\n",
                              "if (is.list(oas_result$avg_life$avg_lives) && 'overall_wal' %in% names(oas_result$avg_life$avg_lives)) {\n",
                              "  wal <- oas_result$avg_life$avg_lives$overall_wal\n",
                              "} else if (is.list(oas_result$avg_life) && 'overall_wal' %in% names(oas_result$avg_life)) {\n",
                              "  wal <- oas_result$avg_life$overall_wal\n",
                              "} else {\n",
                              "  wal <- mean(oas_result$avg_life$avg_lives)\n",
                              "}\n",
                              "cat('Weighted average life:', round(wal, 2), 'years\\n')\n",
                              "```\n\n")

  # Combine all sections
  report_content <- paste0(yaml_header, setup_chunk, oas_chunk,
                          summary_section, input_section, results_section,
                          analytics_section)

  return(report_content)
}

#' @title Plot Yield Surface
#' @description Creates a 3D yield surface plot.
#'
#' @param rate_paths List. The result from \code{\link{simulate_rates}}
#' @param interactive Logical. Whether to create an interactive 3D plot
#'
#' @return A plot object
#'
#' @examples
#' \dontrun{
#' rate_paths <- simulate_rates(dns_params, n_paths = 100)
#' plot_yield_surface(rate_paths)
#' }
#'
#' @export
plot_yield_surface <- function(rate_paths, interactive = TRUE) {

  # Parameter validation
  checkmate::assert_list(rate_paths)

  # Check if required packages are available
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("The 'plotly' package is required for interactive 3D plots. Using static plot instead.")
    interactive <- FALSE
  }

  if (!interactive && !requireNamespace("lattice", quietly = TRUE)) {
    stop("The 'lattice' package is required for static 3D plots. Please install it.")
  }

  # Get path 1 as representative
  maturities <- as.numeric(names(rate_paths$yield_paths))
  n_periods <- ncol(rate_paths$yield_paths[[as.character(maturities[1])]])

  # Sample time points to avoid overcrowding
  if (n_periods > 50) {
    time_points <- round(seq(1, n_periods, length.out = 50))
  } else {
    time_points <- 1:n_periods
  }

  # Convert to years
  time_years <- time_points / 12

  # Create a matrix of yields [maturity, time]
  yield_matrix <- matrix(NA, nrow = length(maturities), ncol = length(time_points))
  rownames(yield_matrix) <- maturities
  colnames(yield_matrix) <- time_points

  # Fill the matrix with average yields across paths
  for (i in seq_along(maturities)) {
    mat <- maturities[i]
    mat_key <- as.character(mat)

    # Extract yield path
    yield_path <- rate_paths$yield_paths[[mat_key]]

    # Calculate mean across paths
    mean_path <- colMeans(yield_path)

    # Store in matrix
    yield_matrix[i, ] <- mean_path[time_points] * 100  # Convert to percentage
  }

  # Create data frame for plotting
  plot_data <- expand.grid(
    maturity = maturities,
    time = time_years
  )

  # Add yield values
  yield_values <- as.vector(yield_matrix)
  plot_data$yield <- yield_values

  # Create plot
  if (interactive) {
    # Create interactive 3D plot with plotly
    p <- plotly::plot_ly(
      x = ~maturity,
      y = ~time,
      z = ~yield,
      data = plot_data,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,
        color = ~yield,
        colorscale = "Viridis",
        opacity = 0.8
      )
    )
    p <- plotly::add_surface(p)
    p <- plotly::layout(
      p,
      title = "Yield Curve Surface",
      scene = list(
        xaxis = list(title = "Maturity (years)"),
        yaxis = list(title = "Time (years)"),
        zaxis = list(title = "Yield (%)")
      )
    )
  } else {
    # Create static 3D plot with lattice
    p <- lattice::wireframe(
      yield ~ maturity * time,
      data = plot_data,
      scales = list(arrows = FALSE),
      drape = TRUE,
      colorkey = TRUE,
      screen = list(z = 30, x = -60),
      xlab = "Maturity (years)",
      ylab = "Time (years)",
      zlab = "Yield (%)",
      main = "Yield Curve Surface",
      shade = TRUE
    )
  }

  return(p)
}
