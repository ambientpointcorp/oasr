
# Setup mock asset data and rate paths
setup_test_data <- function() {
  # Mock asset data
  asset_data <- data.frame(
    asset_id = c("A001", "A002"),
    balance = c(200000, 150000),
    wac = c(0.055, 0.048),
    wam = c(300, 280),
    age = c(60, 40),
    ltv = c(0.75, 0.68),
    fico = c(720, 760),
    state = c("CA", "NY"),
    property_type = c("SFR", "SFR"),
    occupancy_type = c("PRIMARY", "PRIMARY"),
    stringsAsFactors = FALSE
  )
  
  # Mock rate paths (simplified for testing)
  # Create a 3D array for yield paths: [path, time, maturity]
  n_paths <- 2
  n_periods <- 12
  maturities <- c("1", "5", "10")
  
  yield_paths <- list()
  for (m in maturities) {
    yield_paths[[m]] <- matrix(runif(n_paths * n_periods, 0.01, 0.05), 
                              nrow = n_paths, ncol = n_periods)
  }
  
  rate_paths <- list(
    yield_paths = yield_paths,
    maturities = as.numeric(maturities)
  )
  
  list(asset_data = asset_data, rate_paths = rate_paths)
}

test_that("prepayment_model creates valid model structure", {
  # Test basic CPR/CDR model
  model1 <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01, recovery_rate = 0.65)
  
  expect_s3_class(model1, "prepayment_model")
  expect_type(model1, "list")
  expect_equal(model1$type, "cpr_cdr")
  expect_equal(model1$base_cpr, 0.06)
  expect_equal(model1$base_cdr, 0.01)
  expect_equal(model1$recovery_rate, 0.65)
  
  # Test PSA model
  model2 <- psa_prepayment_model(psa_multiplier = 1.5, base_cdr = 0.01, recovery_rate = 0.65)
  
  expect_s3_class(model2, "prepayment_model")
  expect_equal(model2$type, "psa")
  expect_equal(model2$psa_multiplier, 1.5)
  expect_equal(model2$base_cdr, 0.01)
  
  # Test survival model
  model3 <- survival_prepayment_model(prepay_hazard = 0.005, default_hazard = 0.001)
  
  expect_s3_class(model3, "prepayment_model")
  expect_equal(model3$type, "survival")
  expect_equal(model3$prepay_hazard, 0.005)
  expect_equal(model3$default_hazard, 0.001)
  
  # Test custom model
  custom_fn <- function(asset, reference_rates, n_paths, n_periods) {
    # Create arrays with fixed rates for testing
    prepay_rates <- array(0.005, dim = c(n_paths, n_periods))
    default_rates <- array(0.001, dim = c(n_paths, n_periods))
    recovery_rates <- array(0, dim = c(n_paths, n_periods))
    
    list(
      prepay_rates = prepay_rates,
      default_rates = default_rates,
      recovery_rates = recovery_rates
    )
  }
  
  model4 <- custom_prepayment_model(custom_fn, param1 = "test", param2 = 1.0)
  
  expect_s3_class(model4, "prepayment_model")
  expect_equal(model4$type, "custom")
  expect_type(model4$custom_fn, "closure")
  expect_equal(model4$param1, "test")
  expect_equal(model4$param2, 1.0)
})

test_that("calculate_credit_vectors produces valid output", {
  # Get test data
  test_data <- setup_test_data()
  asset_data <- test_data$asset_data
  rate_paths <- test_data$rate_paths
  
  # Create a basic CPR/CDR model
  prepay_model <- prepayment_model(
    type = "cpr_cdr", 
    base_cpr = 0.06, 
    base_cdr = 0.01,
    recovery_rate = 0.65
  )
  
  # Calculate credit vectors
  result <- calculate_credit_vectors(asset_data, rate_paths, prepay_model, reference_key = "10")
  
  # Check structure
  expect_type(result, "list")
  expect_true(all(c("prepay_rates", "default_rates", "recovery_rates") %in% names(result)))
  
  # Check dimensions
  n_paths <- dim(rate_paths$yield_paths$`10`)[1]
  n_periods <- dim(rate_paths$yield_paths$`10`)[2]
  n_assets <- nrow(asset_data)
  
  expect_equal(dim(result$prepay_rates), c(n_paths, n_periods, n_assets))
  expect_equal(dim(result$default_rates), c(n_paths, n_periods, n_assets))
  expect_equal(dim(result$recovery_rates), c(n_paths, n_periods, n_assets))
  
  # Check value ranges
  expect_true(all(result$prepay_rates >= 0))
  expect_true(all(result$prepay_rates <= 1))
  expect_true(all(result$default_rates >= 0))
  expect_true(all(result$default_rates <= 1))
  expect_true(all(result$recovery_rates >= 0))
  expect_true(all(result$recovery_rates <= 1))
  
  # Test PSA model
  psa_model <- psa_prepayment_model(psa_multiplier = 1.0, base_cdr = 0.01)
  psa_result <- calculate_credit_vectors(asset_data, rate_paths, psa_model)
  
  expect_equal(dim(psa_result$prepay_rates), c(n_paths, n_periods, n_assets))
  expect_true(all(psa_result$prepay_rates >= 0))
  expect_true(all(psa_result$prepay_rates <= 1))
  
  # Test survival model
  surv_model <- survival_prepayment_model(prepay_hazard = 0.005, default_hazard = 0.001)
  surv_result <- calculate_credit_vectors(asset_data, rate_paths, surv_model)
  
  expect_equal(dim(surv_result$prepay_rates), c(n_paths, n_periods, n_assets))
  expect_true(all(surv_result$prepay_rates >= 0))
  expect_true(all(surv_result$prepay_rates <= 1))
})

test_that("custom prepayment model works correctly", {
  # Get test data
  test_data <- setup_test_data()
  asset_data <- test_data$asset_data
  rate_paths <- test_data$rate_paths
  
  # Create custom function with fixed rates for testability
  custom_fn <- function(asset, reference_rates, n_paths, n_periods) {
    # Create arrays with fixed rates for testing
    prepay_rates <- array(0.005, dim = c(n_paths, n_periods))
    default_rates <- array(0.001, dim = c(n_paths, n_periods))
    recovery_rates <- array(0, dim = c(n_paths, n_periods))
    
    list(
      prepay_rates = prepay_rates,
      default_rates = default_rates,
      recovery_rates = recovery_rates
    )
  }
  
  # Create custom model
  custom_model <- custom_prepayment_model(custom_fn)
  
  # Calculate credit vectors
  result <- calculate_credit_vectors(asset_data, rate_paths, custom_model)
  
  # Check that all values match our fixed rates
  n_paths <- dim(rate_paths$yield_paths$`10`)[1]
  n_periods <- dim(rate_paths$yield_paths$`10`)[2]
  
  expect_true(all(result$prepay_rates == 0.005))
  expect_true(all(result$default_rates == 0.001))
  expect_true(all(result$recovery_rates == 0))
})
