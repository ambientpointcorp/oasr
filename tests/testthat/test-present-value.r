context("Present Value Calculation Functions")

# Setup mock cashflows and rate paths
setup_test_data <- function() {
  # Mock asset data
  asset_data <- data.frame(
    asset_id = c("A001", "A002"),
    balance = c(200000, 150000),
    wac = c(0.055, 0.048),
    wam = c(300, 280),
    age = c(60, 40),
    stringsAsFactors = FALSE
  )
  
  # Create mock rate paths
  n_paths <- 2
  n_periods <- 24
  maturities <- c("1", "5", "10")
  
  yield_paths <- list()
  for (m in maturities) {
    # Create predictable rates for testing
    yield_paths[[m]] <- matrix(0.03, nrow = n_paths, ncol = n_periods)
  }
  
  rate_paths <- list(
    yield_paths = yield_paths,
    maturities = as.numeric(maturities)
  )
  
  # Create mock cashflows
  n_assets <- nrow(asset_data)
  
  # 8 components: [1] remaining_balance, [2] scheduled_principal, [3] prepayment, 
  #               [4] default, [5] loss, [6] recovery, [7] interest, [8] total
  cashflows <- list()
  
  for (p in 1:n_paths) {
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
  
  list(
    asset_data = asset_data,
    rate_paths = rate_paths,
    cashflows = cashflows
  )
}

test_that("calculate_present_value produces valid output", {
  # Get test data
  test_data <- setup_test_data()
  cashflows <- test_data$cashflows
  rate_paths <- test_data$rate_paths
  
  # Calculate present values with 0 OAS
  oas <- 0
  pv_result <- calculate_present_value(
    cashflows = cashflows,
    rate_paths = rate_paths,
    oas = oas,
    reference_key = "10",
    asset_level = TRUE,
    total_idx = 8
  )
  
  # Check structure
  expect_is(pv_result, "list")
  expect_true(all(c("present_values", "mean_pvs", "pv_stats") %in% names(pv_result)))
  
  # Get dimensions
  n_paths <- length(cashflows)
  n_assets <- dim(cashflows[[1]])[3]
  
  # Check dimensions
  expect_equal(dim(pv_result$present_values), c(n_paths, n_assets))
  expect_equal(length(pv_result$mean_pvs), n_assets)
  expect_equal(nrow(pv_result$pv_stats), n_assets)
  
  # Test with different OAS
  oas <- 100  # 100 bps
  pv_result2 <- calculate_present_value(
    cashflows = cashflows,
    rate_paths = rate_paths,
    oas = oas,
    reference_key = "10"
  )
  
  # Higher OAS should result in lower present values
  expect_true(all(pv_result2$mean_pvs < pv_result$mean_pvs))
})

test_that("calculate_duration_convexity produces valid output", {
  # Get test data
  test_data <- setup_test_data()
  cashflows <- test_data$cashflows
  rate_paths <- test_data$rate_paths
  
  # Calculate duration and convexity
  oas <- 0
  duration_result <- calculate_duration_convexity(
    cashflows = cashflows,
    rate_paths = rate_paths,
    oas = oas,
    shock = 100,  # 100 bps
    reference_key = "10",
    asset_level = TRUE,
    total_idx = 8
  )
  
  # Check structure
  expect_is(duration_result, "list")
  expect_true(all(c("duration", "convexity", "price_up", "price_base", "price_down") %in% names(duration_result)))
  
  # Get dimensions
  n_assets <- dim(cashflows[[1]])[3]
  
  # Check dimensions
  expect_equal(length(duration_result$duration), n_assets)
  expect_equal(length(duration_result$convexity), n_assets)
  
  # Duration should be positive for standard assets
  expect_true(all(duration_result$duration > 0))
  
  # For standard mortgages, price should be lower when rates are higher
  expect_true(all(duration_result$price_down > duration_result$price_up))
})

test_that("calculate_z_spread produces valid output", {
  # Create fixed cash flows for testing
  cashflows <- c(10000, 10000, 10000, 10000, 110000)
  times <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  spot_rates <- c(0.02, 0.025, 0.03, 0.035, 0.04)
  
  # Calculate bond price at a specific z-spread
  actual_spread <- 100  # 100 bps = 0.01
  price <- calculate_price(cashflows, times, spot_rates, actual_spread)
  
  # Now calculate z-spread from price - should recover the original spread
  z_result <- calculate_z_spread(cashflows, times, price, spot_rates)
  
  # Check structure
  expect_is(z_result, "list")
  expect_true(all(c("z_spread", "price", "error") %in% names(z_result)))
  
  # Check that we recovered the original spread
  expect_equal(z_result$z_spread, actual_spread, tolerance = 0.1)  # Allow slight numerical differences
  
  # Check that calculated price matches input price
  expect_equal(z_result$price, price, tolerance = 0.0001)
})

test_that("calculate_option_cost produces valid output", {
  z_spread <- 200  # 200 bps
  oas <- 150       # 150 bps
  
  option_cost <- calculate_option_cost(z_spread, oas)
  
  # Option cost should be the difference between z-spread and OAS
  expect_equal(option_cost, z_spread - oas)
})

test_that("calculate_price produces valid output", {
  # Create fixed cash flows for testing
  cashflows <- c(10000, 10000, 10000, 10000, 110000)
  times <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  spot_rates <- c(0.02, 0.025, 0.03, 0.035, 0.04)
  
  # Calculate price with 0 spread
  price1 <- calculate_price(cashflows, times, spot_rates, 0)
  
  # Calculate price with 100 bps spread
  price2 <- calculate_price(cashflows, times, spot_rates, 100)
  
  # Price with spread should be lower
  expect_true(price2 < price1)
  
  # Calculate expected price manually for first case
  expected_price <- sum(cashflows * exp(-spot_rates * times))
  
  # Check that calculated price matches expected
  expect_equal(price1, expected_price)
})
