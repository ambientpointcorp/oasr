
# Create mock OAS result for testing
setup_mock_oas_result <- function() {
  # Create mock asset data
  asset_data <- data.frame(
    asset_id = c("A001"),
    balance = c(100000),
    wac = c(0.05),
    wam = c(360),
    age = c(0),
    market_price = c(100.0),
    stringsAsFactors = FALSE
  )
  
  # Create mock rate paths
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
  
  # Create mock credit vectors
  n_assets <- nrow(asset_data)
  prepay_rates <- array(0.005, dim = c(n_paths, n_periods, n_assets))
  default_rates <- array(0.001, dim = c(n_paths, n_periods, n_assets))
  recovery_rates <- array(0, dim = c(n_paths, n_periods, n_assets))
  
  credit_vectors <- list(
    prepay_rates = prepay_rates,
    default_rates = default_rates,
    recovery_rates = recovery_rates
  )
  
  # Create mock cashflows
  cashflows <- list()
  for (p in 1:n_paths) {
    # 8 components: [1] remaining_balance, [2] scheduled_principal, [3] prepayment, 
    #               [4] default, [5] loss, [6] recovery, [7] interest, [8] total
    cf_array <- array(0, dim = c(n_periods, 8, n_assets))
    
    # Set initial balances
    cf_array[1, 1, ] <- asset_data$balance
    
    # Create simple declining balance and cash flows
    for (a in 1:n_assets) {
      initial_balance <- asset_data$balance[a]
      wac <- asset_data$wac[a]
      wam <- asset_data$wam[a] - asset_data$age[a]  # Remaining term
      
      # Monthly payment
      pmt <- initial_balance * (wac/12) / (1 - (1 + wac/12)^(-wam))
      
      # Set cash flows
      remaining_balance <- initial_balance
      
      for (t in 1:n_periods) {
        if (remaining_balance <= 0) break
        
        # Interest portion
        interest <- remaining_balance * wac / 12
        
        # Principal portion
        principal <- min(remaining_balance, pmt - interest)
        
        # Set cash flow components
        cf_array[t, 1, a] <- remaining_balance - principal  # New remaining balance
        cf_array[t, 2, a] <- principal                      # Scheduled principal
        cf_array[t, 7, a] <- interest                       # Interest
        cf_array[t, 8, a] <- principal + interest           # Total
        
        # Update remaining balance
        remaining_balance <- remaining_balance - principal
      }
    }
    
    cashflows[[p]] <- cf_array
  }
  
  # Create mock duration result
  duration <- 4.5
  convexity <- 25
  
  # Create mock WAL result
  avg_life <- list(
    avg_lives = list(overall_wal = 5.2),
    avg_lives_by_path = matrix(5.2, nrow = n_paths, ncol = n_assets)
  )
  
  # Create mock OAS result
  oas_result <- list(
    oas = 50,
    price = 100.5,
    market_price = 100.5,
    price_error = 0,
    converged = TRUE,
    iterations = 10,
    execution_time = 2.5,
    asset_data = asset_data,
    rate_paths = rate_paths,
    prepay_model = prepay_model,
    credit_vectors = credit_vectors,
    cashflows = cashflows,
    duration = list(
      duration = duration,
      convexity = convexity,
      price_up = 98.0,
      price_base = 100.5,
      price_down = 103.0
    ),
    avg_life = avg_life
  )
  
  class(oas_result) <- c("oas_result", "list")
  
  oas_result
}

# Create mock calculate_oas function for testing
mock_calculate_oas <- function(asset_data, rate_paths, prepay_model, market_price, ...) {
  # Create a deterministic OAS based on inputs to make the test predictable
  
  # Check if this is a shifted rate path (look at the first value)
  base_rate <- 0.03  # Our base rate in the mock data
  first_rate <- rate_paths$yield_paths$`10`[1, 1]
  rate_shift <- first_rate - base_rate
  rate_shift_bps <- rate_shift * 10000  # Convert to basis points
  
  # Determine if prepay model is modified
  prepay_multiplier <- 1.0
  if (prepay_model$type == "cpr_cdr") {
    prepay_multiplier <- prepay_model$base_cpr / 0.06  # Default is 0.06
  }
  
  # Calculate OAS based on rate shift and prepay multiplier
  # For simplicity: OAS = 50 - rate_shift_bps/2 + (prepay_multiplier - 1) * 100
  oas <- 50 - rate_shift_bps/2 + (prepay_multiplier - 1) * 100
  
  # Calculate price (should be close to market_price for valid test)
  price <- market_price
  
  # Calculate WAL - higher rates or faster prepayments should reduce WAL
  wal <- 5.2 - rate_shift_bps/200 - (prepay_multiplier - 1) * 1
  
  # Create mock result
  oas_result <- setup_mock_oas_result()
  oas_result$oas <- oas
  oas_result$price <- price
  oas_result$avg_life$avg_lives$overall_wal <- wal
  
  # Add duration effects
  duration <- 4.5 - rate_shift_bps/100 - (prepay_multiplier - 1) * 0.5
  oas_result$duration$duration <- duration
  
  oas_result
}

# Mock test for run_oas_sensitivity using mocking framework
test_that("run_oas_sensitivity produces valid output with mock calculate_oas", {
  # This test requires mockery package
  skip_if_not_installed("mockery")
  
  # Create mock OAS result
  oas_result <- setup_mock_oas_result()
  
  # Define expected behavior of calculate_oas
  # run_oas_sensitivity now re-solves through .resolve_oas, so that is where
  # the call to intercept lives.
  mockery::stub(run_oas_sensitivity, ".resolve_oas",
                function(oas_result, asset_data, rate_paths, prepay_model,
                         market_price, max_iterations) {
                  mock_calculate_oas(asset_data, rate_paths, prepay_model, market_price)
                })
  
  # Run sensitivity analysis with minimal parameters
  sensitivity <- run_oas_sensitivity(
    oas_result = oas_result,
    sensitivity_type = "both",
    rate_shifts = c(-100, 0, 100),
    prepay_multipliers = c(0.5, 1.0, 1.5),
    max_iterations = 1,
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(sensitivity, "oas_sensitivity")
  expect_type(sensitivity, "list")
  expect_true(all(c("rate_sensitivity", "prepay_sensitivity", "base_oas", "base_price") %in% names(sensitivity)))
  
  # Check rate sensitivity structure
  expect_s3_class(sensitivity$rate_sensitivity, "data.frame")
  expect_equal(nrow(sensitivity$rate_sensitivity), 3)  # Three rate shifts
  expect_true(all(c("shift", "oas", "price", "wal") %in% names(sensitivity$rate_sensitivity)))
  
  # Check prepay sensitivity structure
  expect_s3_class(sensitivity$prepay_sensitivity, "data.frame")
  expect_equal(nrow(sensitivity$prepay_sensitivity), 3)  # Three prepay multipliers
  expect_true(all(c("multiplier", "oas", "price", "wal") %in% names(sensitivity$prepay_sensitivity)))
  
  # Check trends in results (OAS should decrease as rates increase) 
  expect_true(sensitivity$rate_sensitivity$oas[1] > sensitivity$rate_sensitivity$oas[3])
  
  # Check trends in results (WAL should decrease as rates increase)
  expect_true(sensitivity$rate_sensitivity$wal[1] > sensitivity$rate_sensitivity$wal[3])
  
  # Check trends in results (WAL should decrease as prepay multiplier increases)
  expect_true(sensitivity$prepay_sensitivity$wal[1] > sensitivity$prepay_sensitivity$wal[3])
})

test_that("print and plot methods for oas_sensitivity work", {
  # Create mock sensitivity result
  sensitivity <- list(
    rate_sensitivity = data.frame(
      shift = c(-100, 0, 100),
      oas = c(100, 50, 0),
      price = c(101, 100, 99),
      wal = c(6, 5, 4),
      duration = c(5, 4.5, 4)
    ),
    prepay_sensitivity = data.frame(
      multiplier = c(0.5, 1.0, 1.5),
      oas = c(0, 50, 100),
      price = c(99, 100, 101),
      wal = c(6, 5, 4),
      duration = c(5, 4.5, 4)
    ),
    base_oas = 50,
    base_price = 100,
    base_wal = 5,
    base_duration = 4.5,
    base_convexity = 25
  )
  
  class(sensitivity) <- c("oas_sensitivity", "list")
  
  # Test print method
  expect_output(print(sensitivity), "Base OAS: 50 bps")
  expect_output(print(sensitivity), "Interest Rate Sensitivity:")
  expect_output(print(sensitivity), "Prepayment Sensitivity:")
  
  # Test plot method
  expect_error(plot(sensitivity), NA)  # Expecting no error
})

test_that("create_oas_surface produces valid output", {
  skip("This test requires mocking and is complex - skipping for now")
  
  # Create mock OAS result
  oas_result <- setup_mock_oas_result()
  
  # Define expected behavior of calculate_oas
  mockery::stub(create_oas_surface, "calculate_oas", mock_calculate_oas)
  
  # Create OAS surface with minimal parameters
  surface <- create_oas_surface(
    oas_result = oas_result,
    rate_shifts = c(-100, 0, 100),
    prepay_multipliers = c(0.5, 1.0, 1.5),
    max_iterations = 1,
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(surface, "oas_surface")
  expect_type(surface, "list")
  expect_true(all(c("surface_data", "oas_matrix", "price_matrix", "wal_matrix") %in% names(surface)))
  
  # Check dimensions
  expect_equal(dim(surface$oas_matrix), c(3, 3))  # 3 rate shifts, 3 prepay multipliers
  expect_equal(dim(surface$price_matrix), c(3, 3))
  expect_equal(dim(surface$wal_matrix), c(3, 3))
  
  # Check that surface_data has the right structure
  expect_s3_class(surface$surface_data, "data.frame")
  expect_equal(nrow(surface$surface_data), 9)  # 3x3 combinations
  expect_true(all(c("shift", "multiplier", "oas", "price", "wal") %in% names(surface$surface_data)))
  
  # Test plot method
  expect_error(plot(surface, type = "oas"), NA)  # Expecting no error
})
