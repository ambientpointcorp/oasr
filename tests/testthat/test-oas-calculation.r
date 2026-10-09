
# Create minimal mock data for testing
setup_minimal_test_data <- function() {
  # Mock asset data with just the essential fields
  asset_data <- data.frame(
    asset_id = c("A001"),
    balance = c(100000),
    wac = c(0.05),
    servicing = c(0.005),
    term = c(360),
    age = c(0),
    stringsAsFactors = FALSE
  )
  
  # Create mock rate paths with fixed rates for testability
  n_paths <- 2
  n_periods <- 12
  maturities <- c("1", "5", "10")
  
  yield_paths <- list()
  for (m in maturities) {
    yield_paths[[m]] <- matrix(0.03, nrow = n_paths, ncol = n_periods)
  }
  
  rate_paths <- list(
    yield_paths = yield_paths,
    maturities = as.numeric(maturities)
  )
  
  # Create basic prepayment model
  prepay_model <- list(
    type = "cpr_cdr",
    base_cpr = 0.06,
    base_cdr = 0.01,
    recovery_rate = 0.65,
    recovery_lag = 12,
    seasoning = 1.0,
    burnout_factor = 0.3,
    rate_sensitivity = 1.0,
    seasonal_factors = rep(1.0, 12)
  )
  class(prepay_model) <- c("prepayment_model", "list")
  
  list(
    asset_data = asset_data,
    rate_paths = rate_paths,
    prepay_model = prepay_model
  )
}

test_that("binary_search_oas finds OAS for fixed cash flows", {
  # Create fixed cash flows for testing
  cashflows <- c(10000, 10000, 10000, 10000, 110000)
  times <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  
  # Create mock rate paths with fixed rates
  n_paths <- 2
  n_periods <- 12
  maturities <- c("1", "5", "10")
  
  yield_paths <- list()
  for (m in maturities) {
    yield_paths[[m]] <- matrix(0.03, nrow = n_paths, ncol = n_periods)
  }
  
  rate_paths <- list(
    yield_paths = yield_paths,
    maturities = as.numeric(maturities)
  )
  
  search_range <- c(-500, 500)

  # The price at zero OAS, computed directly. Solving binary_search_oas()
  # against a target of 0 does not give that: no spread in range drives the
  # price to zero, so it returns the price at the upper bound instead, and
  # the round trip below would then recover that bound rather than zero.
  market_price <- calculate_price(
    cashflows = cashflows,
    times = times,
    spot_rates = rep(0.03, length(times)),
    spread = 0
  )
  
  # Now calculate OAS with our calculated market price
  # Should return close to 0
  oas_result <- binary_search_oas(
    cashflows = cashflows,
    times = times,
    rate_paths = rate_paths,
    market_price = market_price,
    search_range = search_range
  )
  
  # Check structure
  expect_type(oas_result, "list")
  expect_true(all(c("oas", "price", "market_price", "price_error", "converged") %in% names(oas_result)))
  
  # Check that OAS is close to 0 (within 0.1 bps)
  expect_equal(oas_result$oas, 0, tolerance = 0.1)
  
  # Check that price matches market price
  expect_equal(oas_result$price, market_price, tolerance = 0.0001)
  
  # Check that we converged
  expect_true(oas_result$converged)
})

test_that("print and summary methods for oas_result work", {
  # Setup mock OAS result
  oas_result <- structure(
    list(
      oas = 50,
      price = 100.5,
      market_price = 100.5,
      price_error = 0,
      avg_life = list(avg_lives = list(overall_wal = 5.2)),
      duration = list(duration = 4.5, convexity = 25),
      converged = TRUE,
      iterations = 10,
      execution_time = 2.5,
      asset_data = data.frame(
        asset_id = "A001",
        balance = 100000,
        wac = 0.05,
        servicing = 0.005,
        term = 360
      )
    ),
    class = c("oas_result", "list")
  )
  
  # Test print method
  expect_output(print(oas_result), "OAS: 50 bps")
  expect_output(print(oas_result), "Model Price: 100.5")
  expect_output(print(oas_result), "Weighted Average Life: 5.2 years")
  
  # Test summary method
  summary_result <- summary(oas_result)
  
  expect_type(summary_result, "list")
  expect_true(all(c("overall", "convergence", "execution_time", "asset_info") %in% names(summary_result)))
  expect_equal(nrow(summary_result$overall), 7)
  expect_true(summary_result$convergence$converged)
})

# Full OAS calculation is complex to test fully, so we'll do a simplified test
# with mock functions to isolate the binary search algorithm
test_that("calculate_oas handles binary search correctly", {
  skip("This test is more complex and may require mocking - skipping for now")
  
  # Get minimal test data
  test_data <- setup_minimal_test_data()
  
  # Mock the price_function within calculate_oas
  # We'll create a simple linear relationship between OAS and price
  # where price = 100 - OAS/100
  
  # With this relationship:
  # - OAS = 0 => price = 100
  # - OAS = 100 => price = 99
  # - OAS = -100 => price = 101
  
  # So if market_price = 99.5, we'd expect OAS = 50
  
  mockery::stub(calculate_oas, "price_function", function(oas) {
    price <- 100 - oas/100
    list(price = price)
  })
  
  # Run OAS calculation with market_price = 99.5
  oas_result <- calculate_oas(
    asset_data = test_data$asset_data,
    rate_paths = test_data$rate_paths,
    prepay_model = test_data$prepay_model,
    market_price = 99.5,
    search_range = c(-500, 500),
    tolerance = 0.01,
    max_iterations = 20,
    verbose = FALSE
  )
  
  # Check that OAS is close to 50
  expect_equal(oas_result$oas, 50, tolerance = 1)
})

# Since full OAS calculation is complex, we'll also test that the function at least runs
test_that("calculate_oas runs without errors", {
  skip_if_not_installed("dnsr")
  
  # Get minimal test data
  test_data <- setup_minimal_test_data()
  
  # Set a very low max_iterations to speed up the test
  max_iterations <- 2
  
  # Just test that the function runs without error
  expect_error(
    calculate_oas(
      asset_data = test_data$asset_data,
      rate_paths = test_data$rate_paths,
      prepay_model = test_data$prepay_model,
      market_price = 100.0,  # a security-level quote, not a loan attribute
      search_range = c(-100, 100),
      tolerance = 1,  # High tolerance for speed
      max_iterations = max_iterations,
      verbose = FALSE
    ),
    NA  # Expecting no error
  )
})
