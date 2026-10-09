context("Rate Modeling Functions")

# Setup mock DNS parameters
setup_dns_params <- function() {
  list(
    params = list(
      kappa = c(0.02, 0.3, 0.5),     # Mean-reversion speeds
      theta = c(0.04, -0.02, 0.01),  # Long-run means
      sigma = c(0.005, 0.01, 0.02),  # Volatilities
      names = c("Level", "Slope", "Curvature"),
      lambda = 0.7308
    ),
    initial_factors = c(0.035, -0.015, 0.005),
    # Per year, as `maturities` is: Diebold and Li's per-month 0.0609 * 12
    lambda = 0.7308
  )
}

test_that("simulate_rates produces expected output structure", {
  # Skip if dnsr package is not available
  skip_if_not_installed("dnsr")
  
  # Setup test parameters with reduced dimensions for speed
  dns_params <- setup_dns_params()
  n_paths <- 3
  n_periods <- 12
  maturities <- c(1, 5, 10)
  
  # Run simulation
  result <- simulate_rates(dns_params, n_paths = n_paths, n_periods = n_periods, 
                         maturities = maturities, seed = 123)
  
  # Check structure
  expect_is(result, "list")
  expect_true(all(c("yield_paths", "factors", "params", "maturities") %in% names(result)))
  
  # Check yield_paths structure
  expect_is(result$yield_paths, "list")
  expect_equal(length(result$yield_paths), length(maturities))
  expect_true(all(as.character(maturities) %in% names(result$yield_paths)))
  
  # Check dimensions of first yield path
  first_maturity <- as.character(maturities[1])
  expect_equal(dim(result$yield_paths[[first_maturity]]), c(n_paths, n_periods))
  
  # Check factor dimensions
  expect_equal(dim(result$factors)[1], n_periods)
  expect_equal(dim(result$factors)[2], n_paths)
  expect_equal(dim(result$factors)[3], 3) # Level, Slope, Curvature
  
  # Check maturities
  expect_equal(result$maturities, maturities)
})

test_that("extract_reference_rates extracts correct rates", {
  # Skip if dnsr package is not available
  skip_if_not_installed("dnsr")
  
  # Setup test parameters with reduced dimensions for speed
  dns_params <- setup_dns_params()
  n_paths <- 3
  n_periods <- 12
  maturities <- c(1, 5, 10)
  
  # Run simulation
  rate_paths <- simulate_rates(dns_params, n_paths = n_paths, n_periods = n_periods, 
                             maturities = maturities, seed = 123)
  
  # Extract reference rates
  ref_rates <- extract_reference_rates(rate_paths, "5")
  
  # Check dimensions and values
  expect_equal(dim(ref_rates), c(n_paths, n_periods))
  expect_equal(ref_rates, rate_paths$yield_paths$`5`)
  
  # Test error handling
  expect_error(extract_reference_rates(rate_paths, "20"), 
               "Reference rate key '20' not found in rate paths")
})

test_that("get_discount_factors calculates correct discount factors", {
  # Skip if dnsr package is not available
  skip_if_not_installed("dnsr")
  
  # Setup test parameters with reduced dimensions
  dns_params <- setup_dns_params()
  n_paths <- 2
  n_periods <- 12
  maturities <- c(1, 5, 10)
  
  # Run simulation
  rate_paths <- simulate_rates(dns_params, n_paths = n_paths, n_periods = n_periods, 
                             maturities = maturities, seed = 123)
  
  # Get discount factors with 100 bps spread
  spread <- 0.01  # 100 bps
  times <- c(0.5, 1, 2, 3)
  df_result <- get_discount_factors(rate_paths, "5", spread, times)
  
  # Check structure
  expect_is(df_result, "list")
  expect_true(all(c("discount_factors", "times", "reference_rates", "spread") %in% names(df_result)))
  expect_equal(length(df_result$discount_factors), n_paths)
  expect_equal(length(df_result$discount_factors[[1]]), length(times))
  
  # Check that discount factors are in appropriate range (0,1)
  for (i in 1:n_paths) {
    expect_true(all(df_result$discount_factors[[i]] > 0))
    expect_true(all(df_result$discount_factors[[i]] < 1))
  }
  
  # Manually calculate discount factors for verification
  ref_rates <- extract_reference_rates(rate_paths, "5")
  manually_calculated_df <- numeric(length(times))
  
  # Check first path's first discount factor
  period_idx <- min(round(times[1] * 12), n_periods)
  rate <- ref_rates[1, period_idx] + spread
  expected_df <- exp(-rate * times[1])
  
  # Allow small numerical differences
  expect_equal(df_result$discount_factors[[1]][1], expected_df, tolerance = 1e-10)
})
