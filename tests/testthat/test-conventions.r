# Phase 4 conventions. The point of these is that each option does what it
# claims and that the legacy settings reproduce the earlier behaviour exactly.


component_total <- function(cf, j) {
  sum(vapply(cf, function(a) sum(a[, j, ]), numeric(1))) / length(cf)
}

test_that("simulate_rates labels its periods and can drop t = 0", {
  f <- oasr_fixture(n_periods = 24)
  m <- f$rate_paths$yield_paths[["10"]]
  expect_equal(ncol(m), 25)
  expect_equal(colnames(m)[1], "0")
  # the t = 0 column is deterministic, which is why it is labelled
  expect_equal(length(unique(m[, 1])), 1L)

  dns_params <- list(
    params = list(kappa = c(0.02, 0.3, 0.5), theta = c(0.04, -0.02, 0.01),
                  sigma = c(0.005, 0.01, 0.02),
                  names = c("Level", "Slope", "Curvature"), lambda = 0.7308),
    initial_factors = c(0.035, -0.015, 0.005), lambda = 0.7308
  )
  dropped <- simulate_rates(dns_params, n_paths = 4, n_periods = 24,
                            maturities = c(0.25, 1, 5, 10), seed = 31,
                            engine = "exact", include_t0 = FALSE)
  expect_equal(ncol(dropped$yield_paths[["10"]]), 24)
  expect_equal(colnames(dropped$yield_paths[["10"]])[1], "1")
  expect_equal(dim(dropped$factors)[1], 24)
})

test_that("30/360 reproduces the scalar arithmetic exactly", {
  f <- oasr_fixture()
  no_col <- f$asset_data[, setdiff(names(f$asset_data), "accrual_method")]
  legacy <- project_cashflows(no_col, f$credit)
  explicit <- project_cashflows(no_col, f$credit, accrual_method = "30/360",
                                start_date = as.Date("2026-01-31"))
  expect_identical(legacy, explicit)

  # an ACT convention with no date anchor warns and falls back, also exactly
  act <- no_col
  act$accrual_method <- "ACT/365"
  expect_warning(project_cashflows(act, f$credit), "date anchor")
  expect_identical(suppressWarnings(project_cashflows(act, f$credit)), legacy)
})

test_that("ACT conventions accrue on actual days", {
  f <- oasr_fixture()
  no_col <- f$asset_data[, setdiff(names(f$asset_data), "accrual_method")]
  anchor <- as.Date("2026-01-31")
  base <- project_cashflows(no_col, f$credit)
  a365 <- project_cashflows(no_col, f$credit, accrual_method = "ACT/365",
                            start_date = anchor)
  a360 <- project_cashflows(no_col, f$credit, accrual_method = "ACT/360",
                            start_date = anchor)

  i0 <- component_total(base, 7)
  # ACT/365 and 30/360 accrue the same total over a non-leap year, so the
  # difference is timing: small. ACT/360 accrues 365/360 of it: a level change.
  expect_lt(abs(component_total(a365, 7) / i0 - 1), 0.001)
  expect_gt(component_total(a360, 7) / i0, 1.01)

  # the suite's own column spelling is honoured
  alias <- no_col
  alias$Accrual <- "ACT/360"
  expect_equal(component_total(project_cashflows(alias, f$credit, start_date = anchor), 7),
               component_total(a360, 7))

  expect_error(project_cashflows(
    transform(no_col, accrual_method = "ACT/ACT"), f$credit), "Unsupported accrual_method")
})

test_that("a short horizon warns and can return the stub", {
  f <- oasr_fixture(n_periods = 24)
  expect_warning(project_cashflows(f$asset_data, f$credit), "shorter than the longest")
  expect_error(
    project_cashflows(f$asset_data, f$credit, on_short_horizon = "error"),
    "shorter than the longest")
  expect_silent(project_cashflows(f$asset_data, f$credit, on_short_horizon = "silent"))

  kept <- project_cashflows(f$asset_data, f$credit, on_short_horizon = "silent",
                            terminal_balance = "return")
  dropped <- project_cashflows(f$asset_data, f$credit, on_short_horizon = "silent",
                               terminal_balance = "drop")
  pool <- sum(f$asset_data$balance)
  expect_lt(component_total(dropped, 2), 0.2 * pool)
  expect_gt(component_total(kept, 2), 0.8 * pool)
})

test_that("the defaults recovery basis delivers the model's recovery rate", {
  f <- oasr_fixture()
  bal <- project_cashflows(f$asset_data, f$credit, recovery_basis = "balance")
  dfl <- project_cashflows(f$asset_data, f$credit, recovery_basis = "defaults")
  rate <- f$prepay_model$recovery_rate

  # on the defaulted amount the realised ratio lands on the stated rate; on the
  # whole balance it does not
  expect_lt(abs(component_total(dfl, 6) / component_total(dfl, 4) - rate), 0.02)
  expect_gt(abs(component_total(bal, 6) / component_total(bal, 4) - rate), 0.05)
})

test_that("path discounting differs from spot and both solve", {
  f <- oasr_fixture()
  cf <- project_cashflows(f$asset_data, f$credit)
  pv_path <- calculate_present_value(cf, f$rate_paths, oas = 100, discounting = "path")
  pv_spot <- calculate_present_value(cf, f$rate_paths, oas = 100, discounting = "spot")
  expect_false(isTRUE(all.equal(pv_path$mean_pvs, pv_spot$mean_pvs)))
  expect_true(all(is.finite(pv_path$mean_pvs)))

  for (d in c("path", "spot")) {
    r <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model,
                       market_price = 101.5, verbose = FALSE, discounting = d)
    expect_true(r$converged)
    expect_equal(r$options$discounting, d)
  }
})

test_that("reproject makes duration respond to prepayments", {
  f <- oasr_fixture()
  pm <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01,
                         recovery_rate = 0.65, rate_sensitivity = 4,
                         burnout_factor = 0.3)
  cv <- calculate_credit_vectors(f$asset_data, f$rate_paths, pm)
  cf <- project_cashflows(f$asset_data, cv)
  proj <- function(paths) {
    project_cashflows(f$asset_data,
                      calculate_credit_vectors(f$asset_data, paths, pm))
  }
  static <- calculate_duration_convexity(cf, f$rate_paths, oas = 100)
  effective <- calculate_duration_convexity(cf, f$rate_paths, oas = 100,
                                            reproject = proj)
  expect_false(isTRUE(all.equal(mean(static$convexity), mean(effective$convexity))))
})

test_that("a sensitivity re-solves on the base result's conventions", {
  f <- oasr_fixture()
  r <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model, market_price = 101.5,
                     verbose = FALSE, discounting = "spot", terminal_balance = "drop",
                     recovery_basis = "balance")
  expect_equal(r$options$discounting, "spot")
  expect_equal(r$options$recovery_basis, "balance")

  sens_spot <- run_oas_sensitivity(r, sensitivity_type = "rates",
                                   rate_shifts = c(0), max_iterations = 30,
                                   verbose = FALSE)

  r_path <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model,
                          market_price = 101.5, verbose = FALSE,
                          discounting = "path")
  sens_path <- run_oas_sensitivity(r_path, sensitivity_type = "rates",
                                   rate_shifts = c(0), max_iterations = 30,
                                   verbose = FALSE)

  # Each zero-shift row sits beside its own base solve rather than beside the
  # other one: the conventions travelled with the result. The two bases differ
  # by well over a hundred basis points, so an inherited setting is the only
  # thing that could keep each pair together. They are not asserted equal
  # because the sensitivity re-solves under its own iteration budget.
  expect_lt(abs(sens_spot$rate_sensitivity$oas[1] - r$oas), 2)
  expect_lt(abs(sens_path$rate_sensitivity$oas[1] - r_path$oas), 2)
  expect_gt(abs(r_path$oas - r$oas), 50)
})

test_that("plot sampling is reproducible when seeded", {
  f <- oasr_fixture(n_periods = 60)
  a <- plot_rate_paths(f$rate_paths, seed = 7)$data
  b <- plot_rate_paths(f$rate_paths, seed = 7)$data
  expect_equal(a, b)
})
