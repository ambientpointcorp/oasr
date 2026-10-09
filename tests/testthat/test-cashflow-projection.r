
# Setup mock asset data, credit vectors, and deal structure
setup_test_data <- function() {
  # Mock asset data
  asset_data <- data.frame(
    asset_id = c("A001", "A002"),
    balance = c(200000, 150000),
    wac = c(0.055, 0.048),
    servicing = c(0.005, 0.004),
    term = c(360, 320),
    age = c(60, 40),
    ltv = c(0.75, 0.68),
    fico = c(720, 760),
    state = c("CA", "NY"),
    property_type = c("SFR", "SFR"),
    occupancy_type = c("PRIMARY", "PRIMARY"),
    stringsAsFactors = FALSE
  )
  
  # Create mock credit vectors
  n_paths <- 2
  n_periods <- 24
  n_assets <- nrow(asset_data)
  
  # Fixed rates for testability
  prepay_rates <- array(0.005, dim = c(n_paths, n_periods, n_assets))
  default_rates <- array(0.001, dim = c(n_paths, n_periods, n_assets))
  recovery_rates <- array(0, dim = c(n_paths, n_periods, n_assets))
  
  credit_vectors <- list(
    prepay_rates = prepay_rates,
    default_rates = default_rates,
    recovery_rates = recovery_rates
  )
  
  # Mock deal structure
  bonds <- data.frame(
    bond_id = c("A", "B"),
    balance = c(250000, 100000),
    coupon = c(0.04, 0.06)
  )
  
  waterfall <- list(
    principal = "sequential",
    interest = "sequential",
    losses = "reverse_sequential"
  )
  
  deal_structure <- list(
    bonds = bonds,
    waterfall = waterfall
  )
  
  list(
    asset_data = asset_data,
    credit_vectors = credit_vectors,
    deal_structure = deal_structure
  )
}

test_that("project_cashflows produces valid output", {
  # Get test data
  test_data <- setup_test_data()
  asset_data <- test_data$asset_data
  credit_vectors <- test_data$credit_vectors
  
  # Project cash flows
  cashflows <- project_cashflows(asset_data, credit_vectors)
  
  # Check structure
  expect_type(cashflows, "list")
  
  # Get dimensions
  n_paths <- dim(credit_vectors$prepay_rates)[1]
  n_periods <- dim(credit_vectors$prepay_rates)[2]
  n_assets <- dim(credit_vectors$prepay_rates)[3]
  n_components <- 9  # Components in cashflow array
  
  # Check dimensions of first path's cashflow array
  expect_equal(dim(cashflows[[1]]), c(n_periods, n_components, n_assets))
  
  # Check number of paths
  expect_equal(length(cashflows), n_paths)
  
  # Test with specific n_periods
  max_periods <- 12
  cashflows2 <- project_cashflows(asset_data, credit_vectors, n_periods = max_periods)
  
  # Check dimensions of first path's cashflow array with max_periods
  expect_equal(dim(cashflows2[[1]]), c(max_periods, n_components, n_assets))
  
  # Check that cash flows are reasonable
  path1_cf <- cashflows[[1]]
  
  # Check that remaining balance decreases over time
  for (a in 1:n_assets) {
    balances <- path1_cf[, 1, a] # Component 1 is remaining balance
    non_zero_indices <- which(balances > 0)
    if (length(non_zero_indices) > 1) {
      # Check that balance is non-increasing
      balance_changes <- diff(balances[non_zero_indices])
      expect_true(all(balance_changes <= 0))
    }
  }
  
  # Check that first payment has the right components
  # For a standard mortgage, the first payment should have:
  # - Positive scheduled principal
  # - Positive interest
  # - Potentially positive prepayment if prepay_rate > 0
  for (a in 1:n_assets) {
    expect_true(path1_cf[1, 2, a] > 0)  # Scheduled principal
    expect_true(path1_cf[1, 7, a] > 0)  # Interest
    
    # Total payment is cash to the investor, so servicing (component 8) is a
    # deduction, not an addition.
    components_sum <- path1_cf[1, 2, a] + path1_cf[1, 3, a] + path1_cf[1, 6, a] +
      path1_cf[1, 7, a] - path1_cf[1, 8, a]
    expect_equal(path1_cf[1, 9, a], components_sum)
  }
})

test_that("aggregate_cashflows correctly handles deal structure", {
  # Skip test if data.table package is not available
  skip_if_not_installed("dplyr")
  
  # Get test data
  test_data <- setup_test_data()
  asset_data <- test_data$asset_data
  credit_vectors <- test_data$credit_vectors
  deal_structure <- test_data$deal_structure
  
  # Project cash flows
  cashflows <- project_cashflows(asset_data, credit_vectors)
  
  # Aggregate cash flows
  bond_cashflows <- aggregate_cashflows(cashflows, asset_data, deal_structure)
  
  # Check structure
  expect_type(bond_cashflows, "list")
  
  # Get dimensions
  n_paths <- dim(credit_vectors$prepay_rates)[1]
  n_periods <- dim(credit_vectors$prepay_rates)[2]
  n_bonds <- nrow(deal_structure$bonds)
  n_components <- 5  # Components in bond cashflow array
  
  # Check dimensions of first path's bond cashflow array
  expect_equal(dim(bond_cashflows[[1]]), c(n_periods, n_components, n_bonds))
  
  # Check number of paths
  expect_equal(length(bond_cashflows), n_paths)
  
  # Test sequential waterfall logic
  path1_bond_cf <- bond_cashflows[[1]]
  
  # In sequential payment, bond A should receive payments before bond B
  # Check that principal is allocated to bond A first
  principal_a <- path1_bond_cf[, 2, 1]  # Principal for bond A
  principal_b <- path1_bond_cf[, 2, 2]  # Principal for bond B
  
  # If bond A is not fully paid off, bond B should not receive principal
  for (t in 1:n_periods) {
    if (sum(principal_a[1:t]) < deal_structure$bonds$balance[1]) {
      # Bond A is not fully paid off
      if (t < n_periods) {
        # Bond B should not receive principal in the next period
        # (unless there's enough principal to pay both in the same period)
        if (principal_a[t] < deal_structure$bonds$balance[1] - sum(principal_a[1:(t-1)])) {
          expect_equal(principal_b[t], 0)
        }
      }
    }
  }
  
  # In reverse sequential losses, bond B should receive losses before bond A
  # This is more complicated to test directly, but we can check that
  # losses are allocated to some bond
  losses_a <- path1_bond_cf[, 4, 1]  # Losses for bond A
  losses_b <- path1_bond_cf[, 4, 2]  # Losses for bond B
  
  # At least one of the bonds should have losses if default rates > 0
  if (any(credit_vectors$default_rates > 0)) {
    expect_true(sum(losses_a) > 0 || sum(losses_b) > 0)
  }
})

test_that("calculate_average_life works correctly", {
  # Get test data
  test_data <- setup_test_data()
  asset_data <- test_data$asset_data
  credit_vectors <- test_data$credit_vectors
  
  # Project cash flows
  cashflows <- project_cashflows(asset_data, credit_vectors)
  
  # Calculate WAL
  wal_result <- calculate_average_life(cashflows, asset_level = TRUE)
  
  # Check structure
  expect_type(wal_result, "list")
  expect_true(all(c("avg_lives", "avg_lives_by_path") %in% names(wal_result)))
  
  # Get dimensions
  n_paths <- dim(credit_vectors$prepay_rates)[1]
  n_assets <- dim(credit_vectors$prepay_rates)[3]
  
  # Check dimensions
  expect_equal(length(wal_result$avg_lives$by_asset), n_assets)
  expect_equal(dim(wal_result$avg_lives_by_path), c(n_paths, n_assets))
  
  # Check that WAL values are reasonable
  # For a standard mortgage, WAL should be less than the original term
  for (a in 1:n_assets) {
    original_term <- (asset_data$term[a] - asset_data$age[a]) / 12  # remaining, in years
    expect_true(wal_result$avg_lives$by_asset[a] < original_term)
  }
  
  # Test with bond-level cashflows
  deal_structure <- test_data$deal_structure
  bond_cashflows <- aggregate_cashflows(cashflows, asset_data, deal_structure)
  bond_wal <- calculate_average_life(bond_cashflows, asset_level = FALSE, principal_idx = 2)
  
  # Check structure for bond WAL
  expect_type(bond_wal, "list")
  expect_true(all(c("avg_lives", "avg_lives_by_path") %in% names(bond_wal)))
  
  # Get dimensions
  n_bonds <- nrow(deal_structure$bonds)
  
  # Check dimensions
  expect_equal(length(bond_wal$avg_lives$by_asset), n_bonds)
  expect_equal(dim(bond_wal$avg_lives_by_path), c(n_paths, n_bonds))
})
