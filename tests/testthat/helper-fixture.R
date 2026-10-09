# One deterministic fixture for the whole suite. Fixed seed, the exact engine
# so no optional package is needed, and nothing that reads the clock or draws
# unseeded: a snapshot taken against this has to be reproducible or it is
# worth nothing.

oasr_dns_params <- function() {
  list(
    params = list(
      kappa = c(0.02, 0.3, 0.5),
      theta = c(0.04, -0.02, 0.01),
      sigma = c(0.005, 0.01, 0.02),
      names = c("Level", "Slope", "Curvature"),
      lambda = 0.7308
    ),
    initial_factors = c(0.035, -0.015, 0.005),
    lambda = 0.7308
  )
}

oasr_fixture <- function(n_paths = 4, n_periods = 360) {
  utils::data("sample_abs_data", package = "oasr", envir = environment())
  utils::data("sample_deal_structure", package = "oasr", envir = environment())
  asset_data <- get("sample_abs_data", envir = environment())
  deal_structure <- get("sample_deal_structure", envir = environment())

  rate_paths <- simulate_rates(
    oasr_dns_params(), n_paths = n_paths, n_periods = n_periods,
    maturities = c(0.25, 1, 5, 10), seed = 31, engine = "exact"
  )

  models <- list(
    cpr_cdr = prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01,
                               recovery_rate = 0.65, recovery_lag = 12),
    psa = psa_prepayment_model(psa_multiplier = 1.5, base_cdr = 0.01,
                               recovery_rate = 0.65),
    survival = survival_prepayment_model(prepay_hazard = 0.005,
                                         default_hazard = 0.001,
                                         recovery_rate = 0.65),
    custom = custom_prepayment_model(function(asset, reference_rates, n_paths, n_periods) {
      list(
        prepay_rates = matrix(0.005, n_paths, n_periods),
        default_rates = matrix(0.001, n_paths, n_periods),
        recovery_rates = matrix(0.00065, n_paths, n_periods)
      )
    })
  )

  list(
    asset_data = asset_data,
    deal_structure = deal_structure,
    rate_paths = rate_paths,
    models = models,
    prepay_model = models$cpr_cdr,
    credit = calculate_credit_vectors(asset_data, rate_paths, models$cpr_cdr)
  )
}

# The conventions in force before Phase 4 moved the defaults. Passed to
# calculate_oas() to reproduce the earlier behaviour exactly.
oasr_legacy_options <- function() {
  list(discounting = "spot", terminal_balance = "drop", recovery_basis = "balance",
       accrual_method = "30/360")
}

# A compact, reviewable description of an object's shape: one line per leaf,
# giving its path, class and dimensions, and never its values. Structure
# snapshots built from this fail on a renamed field or a reshaped array and
# stay quiet when a number moves.
oasr_describe <- function(x, path = "") {
  if (is.list(x) && !is.data.frame(x) && is.null(dim(x))) {
    nms <- names(x)
    if (is.null(nms)) nms <- paste0("[[", seq_along(x), "]]")
    if (length(x) == 0) return(paste0(path, " : empty list"))
    unlist(lapply(seq_along(x), function(i) {
      oasr_describe(x[[i]], paste0(path, "$", nms[i]))
    }), use.names = FALSE)
  } else {
    shape <- if (!is.null(dim(x))) paste(dim(x), collapse = " x ") else length(x)
    cols <- if (is.data.frame(x)) paste0(" {", paste(names(x), collapse = ", "), "}") else ""
    paste0(path, " : ", paste(class(x), collapse = "/"), " [", shape, "]", cols)
  }
}
