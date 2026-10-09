# The interface the downstream suite actually calls, exercised with the
# argument names those call sites use. This file is the compatibility contract:
# it asserts names, types and shapes, never values, so a deliberate change to a
# number passes while a renamed field or a reshaped array does not.


test_that("the cash flow component layout is unchanged", {
  expect_equal(cashflow_components("asset"),
               c(balance = 1L, scheduled_principal = 2L, prepayment = 3L,
                 default = 4L, loss = 5L, recovery = 6L, interest = 7L,
                 servicing = 8L, total = 9L))
  expect_equal(cashflow_components("bond"),
               c(balance = 1L, principal = 2L, interest = 3L, loss = 4L,
                 total = 5L))
  expect_equal(cashflow_components("asset_io")[["total"]], 10L)
})

test_that("projection and aggregation keep their array shapes", {
  f <- oasr_fixture()
  n_assets <- nrow(f$asset_data)

  credit <- calculate_credit_vectors(asset_data = f$asset_data,
                                     rate_paths = f$rate_paths,
                                     prepay_model = f$prepay_model)
  expect_true(all(c("prepay_rates", "default_rates", "recovery_rates") %in% names(credit)))
  n_periods <- dim(credit$prepay_rates)[2]
  expect_equal(dim(credit$prepay_rates), c(4, n_periods, n_assets))

  cf <- project_cashflows(asset_data = f$asset_data, credit_vectors = credit)
  expect_length(cf, 4)
  expect_equal(dim(cf[[1]]), c(n_periods, 9, n_assets))

  cf_io <- project_cashflows_with_io(asset_data = f$asset_data,
                                     credit_vectors = credit)
  expect_equal(dim(cf_io[[1]]), c(n_periods, 10, n_assets))

  bond_cf <- aggregate_cashflows(cashflows = cf, asset_data = f$asset_data,
                                 deal_structure = f$deal_structure)
  expect_equal(dim(bond_cf[[1]]),
               c(n_periods, 5, nrow(f$deal_structure$bonds)))

  # a one-asset pool must survive the same path
  one <- f$asset_data[1, , drop = FALSE]
  credit1 <- calculate_credit_vectors(one, f$rate_paths, f$prepay_model)
  cf1 <- project_cashflows(one, credit1)
  expect_equal(dim(cf1[[1]]), c(n_periods, 9, 1))
  expect_silent(aggregate_cashflows(cf1, one, f$deal_structure))
})

test_that("valuation entry points keep their returned element names", {
  f <- oasr_fixture()
  credit <- calculate_credit_vectors(f$asset_data, f$rate_paths, f$prepay_model)
  cf <- project_cashflows(f$asset_data, credit)

  pv <- calculate_present_value(cashflows = cf, rate_paths = f$rate_paths,
                                oas = 50)
  expect_true(all(c("present_values", "mean_pvs", "pv_stats") %in% names(pv)))

  wal <- calculate_average_life(cashflows = cf, asset_level = TRUE)
  expect_true(all(c("avg_lives", "avg_lives_by_path") %in% names(wal)))
  expect_true("overall_wal" %in% names(wal$avg_lives))

  dc <- calculate_duration_convexity(cashflows = cf, rate_paths = f$rate_paths,
                                     oas = 50, shock = 100,
                                     reference_key = "10", asset_level = TRUE,
                                     total_idx = 9)
  expect_true(all(c("duration", "convexity", "price_up", "price_base",
                    "price_down") %in% names(dc)))

  times <- seq_len(60) / 12
  flows <- c(rep(1000, 59), 101000)
  z <- calculate_z_spread(cashflows = flows, times = times,
                          market_price = 100000,
                          spot_rates = rep(0.03, 60), initial_spread = 100)
  expect_true(all(c("z_spread", "price", "error") %in% names(z)))
  expect_type(calculate_option_cost(z_spread = z$z_spread, oas = 50), "double")

  bs <- binary_search_oas(cashflows = flows, times = times,
                          rate_paths = f$rate_paths, market_price = 100000,
                          search_range = c(-200, 500), tolerance = 0.01)
  expect_true(all(c("oas", "price", "market_price", "price_error",
                    "converged", "iterations") %in% names(bs)))

  expect_type(get_discount_factors(f$rate_paths, "10", 0.01, times), "list")
  expect_true(is.matrix(extract_reference_rates(f$rate_paths, "10")))
})

test_that("calculate_oas keeps its result contract and S3 behaviour", {
  f <- oasr_fixture()
  res <- calculate_oas(asset_data = f$asset_data, rate_paths = f$rate_paths,
                       prepay_model = f$prepay_model, market_price = 101.5,
                       search_range = c(-200, 500), tolerance = 0.01,
                       verbose = FALSE)

  expect_s3_class(res, "oas_result")
  expect_true(all(c("oas", "price", "market_price", "price_error", "converged",
                    "iterations", "execution_time", "avg_life", "duration",
                    "cashflows", "credit_vectors", "asset_data", "rate_paths",
                    "prepay_model") %in% names(res)))
  expect_type(res$oas, "double")
  expect_type(res$converged, "logical")
  expect_type(res$iterations, "double")

  s <- summary(res)
  expect_true(all(c("overall", "convergence", "execution_time") %in% names(s)))
  expect_type(s$convergence$converged, "logical")
  expect_output(print(res), "Option-Adjusted Spread")
  expect_s3_class(plot(res, type = "cashflows"), "ggplot")

  sens <- run_oas_sensitivity(oas_result = res, sensitivity_type = "rates",
                              rate_shifts = c(-100, 0, 100),
                              max_iterations = 8, verbose = FALSE)
  expect_s3_class(sens, "oas_sensitivity")
  expect_true(all(c("shift", "oas", "price", "wal", "duration") %in%
                    names(sens$rate_sensitivity)))
  expect_output(print(sens), "OAS Sensitivity")
})

test_that("the structured path prices near par rather than far below it", {
  f <- oasr_fixture()
  credit <- calculate_credit_vectors(f$asset_data, f$rate_paths, f$prepay_model)
  cf <- project_cashflows(f$asset_data, credit)
  bond_cf <- aggregate_cashflows(cf, f$asset_data, f$deal_structure)
  pv <- calculate_present_value(bond_cf, f$rate_paths, oas = 0,
                                asset_level = FALSE,
                                total_idx = cashflow_components("bond")[["total"]])

  price <- sum(pv$mean_pvs) * 100 / sum(f$deal_structure$bonds$balance)

  # The weights-then-divide-again form returned roughly 60 on this deal, which
  # no market price could ever match.
  expect_gt(price, 80)
  expect_lt(price, 130)
})

test_that("the surface entry point and every S3 plot method work", {
  f <- oasr_fixture()
  res <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model,
                       market_price = 101.5, verbose = FALSE)

  sens <- run_oas_sensitivity(res, sensitivity_type = "both",
                              rate_shifts = c(-100, 100),
                              prepay_multipliers = c(1, 1.5),
                              max_iterations = 8, verbose = FALSE)
  sens_plots <- plot(sens)
  expect_type(sens_plots, "list")
  expect_s3_class(sens_plots$oas_rate, "ggplot")
  expect_s3_class(sens_plots$oas_prepay, "ggplot")

  surface <- create_oas_surface(res, rate_shifts = c(-100, 100),
                                prepay_multipliers = c(1, 1.5),
                                max_iterations = 8, verbose = FALSE)
  expect_s3_class(surface, "oas_surface")
  expect_true(all(c("surface_data", "oas_matrix", "price_matrix", "wal_matrix",
                    "rate_shifts", "prepay_multipliers") %in% names(surface)))
  expect_equal(dim(surface$oas_matrix), c(2, 2))
  expect_s3_class(plot(surface, type = "oas"), "ggplot")
  expect_type(plot(surface, type = "all"), "list")

  # the surface must re-solve on the conventions its base result carried
  expect_equal(surface$base_oas, res$oas)
})

test_that("the paths that used to error now work", {
  f <- oasr_fixture(n_paths = 3)

  # a one-asset pool, through both functions that collapsed on it
  one <- f$asset_data[1, , drop = FALSE]
  credit1 <- calculate_credit_vectors(one, f$rate_paths, f$prepay_model)
  cf1 <- project_cashflows(one, credit1)
  expect_silent(aggregate_cashflows(cf1, one, f$deal_structure))

  res1 <- calculate_oas(one, f$rate_paths, f$prepay_model, market_price = 100,
                        verbose = FALSE, max_iterations = 8)
  expect_s3_class(plot_prepayment_rates(res1, type = "prepay", seed = 1), "ggplot")
  expect_type(plot_prepayment_rates(res1, type = "both", seed = 1), "list")

  # the shipped sample straight into the top-level entry point
  expect_s3_class(
    calculate_oas(f$asset_data, f$rate_paths, f$prepay_model, market_price = 101.5,
                  verbose = FALSE, max_iterations = 8),
    "oas_result")

  # a time shorter than half a month used to index period zero
  df <- get_discount_factors(f$rate_paths, "10", 0.01, times = c(0.01, 0.5, 1))
  expect_length(df$discount_factors[[1]], 3)
  expect_true(all(is.finite(df$discount_factors[[1]])))

  # variable rate loans are reachable through calculate_oas
  arm <- one
  arm$is_variable_rate <- TRUE
  arm$rate_margin <- 0.0275
  arm$reset_frequency <- 12
  arm$next_reset <- 6
  expect_s3_class(
    calculate_oas(arm, f$rate_paths, f$prepay_model, market_price = 100,
                  verbose = FALSE, max_iterations = 6),
    "oas_result")
})

test_that("create_oas_report renders end to end", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  f <- oasr_fixture(n_paths = 3)
  res <- calculate_oas(f$asset_data, f$rate_paths, f$prepay_model,
                       market_price = 101.5, verbose = FALSE, max_iterations = 6)
  out <- file.path(tempdir(), "oasr-surface-report.html")
  on.exit(unlink(out), add = TRUE)

  path <- create_oas_report(res, output_format = "html", output_file = out)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
})

test_that("the names oasr shares with dnsr are the two already known", {
  skip_if_not_installed("dnsr")

  # Attaching both packages makes whichever came second win, silently, and
  # these two take different arguments in each. The vignette and README reach
  # dnsr through dnsr:: rather than attaching it. This test is an alarm for a
  # third collision appearing, not a check on the two that exist.
  collisions <- sort(intersect(getNamespaceExports("dnsr"),
                               getNamespaceExports("oasr")))
  expect_equal(collisions, c("plot_yield_curves", "plot_yield_surface"))
})

test_that("the vignette does not attach dnsr", {
  vignette_path <- system.file("doc", "introduction-to-oasr.Rmd", package = "oasr")
  if (!nzchar(vignette_path)) {
    vignette_path <- testthat::test_path("..", "..", "vignettes",
                                         "introduction-to-oasr.Rmd")
  }
  skip_if_not(file.exists(vignette_path), "vignette source not available")

  src <- readLines(vignette_path, warn = FALSE)
  expect_false(any(grepl("^\\s*library\\(dnsr\\)", src)),
               info = "attaching dnsr masks oasr's two plotting functions")
})
