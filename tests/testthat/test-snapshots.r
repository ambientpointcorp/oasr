# Two snapshots, doing different jobs.
#
# The structure snapshot records element names, classes and array shapes and
# nothing else. It fails on a renamed field or a reshaped array -- the changes
# that break a caller -- and stays silent when a number moves.
#
# The figures snapshot records the headline numbers so an intended change
# shows up as a reviewed diff rather than slipping through. It deliberately
# avoids the solved OAS: bisection stops on a tolerance, so a difference in
# the last bit of a present value can move it by more than the figure is
# worth, and that would make the snapshot fragile rather than informative.

test_that("the returned structure of every entry point is unchanged", {
  f <- oasr_fixture()

  credit <- calculate_credit_vectors(f$asset_data, f$rate_paths, f$prepay_model)
  cf <- project_cashflows(f$asset_data, credit)
  cf_io <- project_cashflows_with_io(f$asset_data, credit)
  bond_cf <- aggregate_cashflows(cf, f$asset_data, f$deal_structure)
  res <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model,
                       market_price = 101.5, verbose = FALSE)

  shape <- c(
    "## calculate_credit_vectors", oasr_describe(credit),
    "## project_cashflows[[1]]", oasr_describe(cf[[1]]),
    "## project_cashflows_with_io[[1]]", oasr_describe(cf_io[[1]]),
    "## aggregate_cashflows[[1]]", oasr_describe(bond_cf[[1]]),
    "## calculate_present_value",
    oasr_describe(calculate_present_value(cf, f$rate_paths, oas = 50)),
    "## calculate_average_life", oasr_describe(calculate_average_life(cf)),
    "## calculate_duration_convexity",
    oasr_describe(calculate_duration_convexity(cf, f$rate_paths, oas = 50)),
    "## calculate_oas",
    oasr_describe(res[setdiff(names(res), c("cashflows", "credit_vectors",
                                            "rate_paths", "asset_data",
                                            "prepay_model", "execution_time"))]),
    "## summary.oas_result", oasr_describe(summary(res))
  )
  expect_snapshot(cat(shape, sep = "\n"))
})

test_that("headline figures are unchanged", {
  f <- oasr_fixture()
  credit <- calculate_credit_vectors(f$asset_data, f$rate_paths, f$prepay_model)
  cf <- project_cashflows(f$asset_data, credit)
  dc <- calculate_duration_convexity(cf, f$rate_paths, oas = 100)
  pv <- calculate_present_value(cf, f$rate_paths, oas = 100)
  wal <- calculate_average_life(cf)

  totals <- function(j) round(sum(vapply(cf, function(a) sum(a[, j, ]), numeric(1))) / length(cf))
  comp <- cashflow_components("asset")

  figures <- data.frame(
    metric = c("scheduled principal", "prepayment", "default", "recovery",
               "interest", "servicing", "total", "mean PV at 100bp",
               "WAL (years)", "duration", "convexity"),
    value = c(totals(comp[["scheduled_principal"]]), totals(comp[["prepayment"]]),
              totals(comp[["default"]]), totals(comp[["recovery"]]),
              totals(comp[["interest"]]), totals(comp[["servicing"]]),
              totals(comp[["total"]]), round(sum(pv$mean_pvs)),
              round(wal$avg_lives$overall_wal, 3),
              round(mean(dc$duration), 4), round(mean(dc$convexity), 3))
  )
  expect_snapshot(print(figures, row.names = FALSE))
})

test_that("the legacy conventions still reproduce the pre-Phase-4 figures", {
  f <- oasr_fixture()
  legacy <- oasr_legacy_options()
  cf <- do.call(project_cashflows,
                c(list(f$asset_data, f$credit),
                  legacy[c("terminal_balance", "recovery_basis", "accrual_method")]))
  pv <- calculate_present_value(cf, f$rate_paths, oas = 100, discounting = "spot")
  wal <- calculate_average_life(cf)
  comp <- cashflow_components("asset")
  totals <- function(j) round(sum(vapply(cf, function(a) sum(a[, j, ]), numeric(1))) / length(cf))

  figures <- data.frame(
    metric = c("scheduled principal", "recovery", "interest", "total",
               "mean PV at 100bp, spot", "WAL (years)"),
    value = c(totals(comp[["scheduled_principal"]]), totals(comp[["recovery"]]),
              totals(comp[["interest"]]), totals(comp[["total"]]),
              round(sum(pv$mean_pvs)), round(wal$avg_lives$overall_wal, 3))
  )
  expect_snapshot(print(figures, row.names = FALSE))
})
