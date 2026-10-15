
#' @title Shared Cash Flow Projection Engine (Internal)
#' @description The body behind \code{\link{project_cashflows}} and
#'   \code{\link{project_cashflows_with_io}}. The two differ only by the IO
#'   strip, so the loop, the validation and the parallel plumbing live here
#'   once; \code{with_io} selects the ten-component layout and deducts the
#'   strip from the total.
#'
#' @keywords internal
#' @noRd
.project_cashflows_engine <- function(asset_data, credit_vectors, n_periods = NULL,
                                      reference_rates = NULL, n_cores = NULL,
                                      run_parallel = FALSE, with_io = FALSE,
                                      accrual_method = "30/360", start_date = NULL,
                                      on_short_horizon = c("warn", "silent", "error"),
                                      terminal_balance = c("return", "drop"),
                                      recovery_basis = c("defaults", "balance")) {

  on_short_horizon <- match.arg(on_short_horizon)
  terminal_balance <- match.arg(terminal_balance)
  recovery_basis <- match.arg(recovery_basis)

  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(credit_vectors)
  checkmate::assert_logical(run_parallel, len = 1)
  if (!is.null(n_cores)) {
    checkmate::assert_int(n_cores, lower = 1)
  }

  has_reference_rates <- !is.null(reference_rates)
  if (has_reference_rates) {
    checkmate::assert_matrix(reference_rates)
  }

  # Fail with a message that names the cause. A missing `term` used to surface
  # as "missing value where TRUE/FALSE needed" from inside the amortisation
  # branch, which points nowhere near the actual problem.
  required_cols <- c("balance", "wac", "servicing", "term")
  if (with_io) {
    required_cols <- c(required_cols, "io_strip")
  }
  missing_cols <- setdiff(required_cols, names(asset_data))
  if (length(missing_cols) > 0) {
    stop("asset_data is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Required: ", paste(required_cols, collapse = ", "),
         "; `term` is the original amortisation term in months.")
  }

  prepay_rates <- credit_vectors$prepay_rates
  default_rates <- credit_vectors$default_rates
  recovery_rates <- credit_vectors$recovery_rates

  n_paths <- dim(prepay_rates)[1]
  if (is.null(n_periods)) {
    n_periods <- dim(prepay_rates)[2]
  } else {
    n_periods <- min(n_periods, dim(prepay_rates)[2])
  }
  n_assets <- dim(prepay_rates)[3]
  n_components <- if (with_io) 10L else 9L

  if (has_reference_rates) {
    if (nrow(reference_rates) != n_paths || ncol(reference_rates) < n_periods) {
      stop("reference_rates dimensions must match [n_paths, n_periods]")
    }
  }

  if (nrow(asset_data) != n_assets) {
    stop("Number of assets in asset_data (", nrow(asset_data),
         ") does not match credit_vectors (", n_assets, ")")
  }

  # Accrual convention and date anchor, resolved per loan: the loan's own
  # column first, then the suite's `Accrual` spelling, then the argument.
  accrual_col <- .accrual_column(asset_data)
  has_anchor_col <- "accrual_start_date" %in% names(asset_data)

  asset_methods <- character(n_assets)
  asset_dcf <- vector("list", n_assets)
  anchor_missing <- FALSE

  for (a in seq_len(n_assets)) {
    method <- .normalise_accrual(
      if (is.na(accrual_col)) accrual_method else asset_data[[accrual_col]][a]
    )
    anchor <- if (has_anchor_col) as.Date(asset_data$accrual_start_date[a]) else start_date
    if (.needs_anchor(method, anchor)) {
      anchor_missing <- TRUE
    }
    asset_methods[a] <- method
    # NULL signals 30/360, where the legacy scalar arithmetic is used verbatim.
    # Assigned through `[` with a wrapped NULL: `[[a]] <- NULL` would delete
    # the element rather than set it, shortening the list.
    asset_dcf[a] <- if (method == "30/360" || .needs_anchor(method, anchor)) {
      list(NULL)
    } else {
      list(as.numeric(diff(.add_months(anchor, 0:n_periods))) /
             if (method == "ACT/360") 360 else 365)
    }
  }

  if (anchor_missing) {
    warning("accrual_method is an ACT convention but no date anchor was given, ",
            "so 30/360 is used. Supply start_date, or an accrual_start_date ",
            "column, to accrue on actual days.", call. = FALSE)
  }

  # A horizon shorter than the longest remaining term truncates the pool.
  if (!is.na(match("term", names(asset_data)))) {
    ages <- if ("age" %in% names(asset_data)) asset_data$age else rep(0, n_assets)
    longest <- max(asset_data$term - ages)
    if (longest > n_periods) {
      msg <- paste0("n_periods (", n_periods, ") is shorter than the longest ",
                    "remaining term (", longest, "). ",
                    if (terminal_balance == "return")
                      "The balance outstanding at the horizon is returned as a final cash flow."
                    else
                      "The balance outstanding at the horizon is dropped.")
      if (on_short_horizon == "error") stop(msg, call. = FALSE)
      if (on_short_horizon == "warn") warning(msg, call. = FALSE)
    }
  }

  results <- .project_asset_grid(
    n_paths = n_paths, n_assets = n_assets, n_periods = n_periods,
    n_components = n_components, with_io = with_io,
    asset_data = asset_data, prepay_rates = prepay_rates,
    default_rates = default_rates, recovery_rates = recovery_rates,
    reference_rates = reference_rates, has_reference_rates = has_reference_rates,
    run_parallel = run_parallel, n_cores = n_cores,
    asset_dcf = asset_dcf, terminal_balance = terminal_balance,
    recovery_basis = recovery_basis,
    recovery_rate = credit_vectors$recovery_rate,
    recovery_lag = credit_vectors$recovery_lag
  )

  results
}

#' @title Project One Asset on One Path (Internal)
#' @description Top-level so it carries no closure environment and serialises
#'   cleanly to socket workers.
#'
#' @keywords internal
#' @noRd
.project_single_asset <- function(path_idx, asset_idx, asset_data, prepay_rates,
                                  default_rates, recovery_rates, reference_rates,
                                  has_reference_rates, n_periods, n_components,
                                  with_io, dcf = NULL, terminal_balance = "drop",
                                  recovery_basis = "balance", recovery_rate = NULL,
                                  recovery_lag = NULL) {
  asset <- asset_data[asset_idx, ]

  current_balance <- asset$balance
  servicing <- asset$servicing
  wac <- asset$wac
  original_term <- asset$term
  io_strip <- if (with_io) asset$io_strip else 0

  age <- if ("age" %in% names(asset)) asset$age else 0

  if ("original_balance" %in% names(asset)) {
    original_balance <- asset$original_balance
  } else {
    original_balance <- current_balance
  }

  is_variable_rate <- FALSE
  rate_margin <- 0
  reset_frequency <- 12
  next_reset <- 0

  if (all(c("is_variable_rate", "rate_margin") %in% names(asset))) {
    is_variable_rate <- isTRUE(asset$is_variable_rate)
    rate_margin <- if (is_variable_rate) asset$rate_margin else 0
    reset_frequency <- if ("reset_frequency" %in% names(asset) && is_variable_rate)
      asset$reset_frequency else 12
    next_reset <- if ("next_reset" %in% names(asset) && is_variable_rate)
      asset$next_reset else 0
  }

  if (is_variable_rate && !has_reference_rates) {
    stop("Variable rate loans require reference_rates parameter")
  }

  remaining_term <- original_term - age

  # dcf NULL means 30/360, where the period fraction is exactly one twelfth and
  # the legacy scalar expressions are used verbatim. Anything else accrues on
  # the day count for the period.
  flat <- is.null(dcf)
  servicing_rate <- servicing / 12
  io_strip_rate <- io_strip / 12
  current_annual_rate <- wac
  monthly_rate <- wac / 12

  # The scheduled payment is the contractual level payment, set from the note
  # rate; the accrual convention changes how each payment splits between
  # interest and principal, not the size of the payment.
  payment_monthly_rate <- wac / 12
  if (payment_monthly_rate > 0 && original_term > 0) {
    scheduled_payment <- original_balance * payment_monthly_rate *
      (1 + payment_monthly_rate)^original_term /
      ((1 + payment_monthly_rate)^original_term - 1)
  } else {
    scheduled_payment <- original_balance / original_term
  }

  remaining_balance <- current_balance
  months_to_reset <- next_reset

  cf_matrix <- matrix(0, nrow = n_periods, ncol = n_components)
  total_idx <- n_components
  track_defaults <- identical(recovery_basis, "defaults")
  default_history <- if (track_defaults) numeric(n_periods) else NULL
  last_t <- 0L

  # A loan already at or past its amortisation term has no remaining horizon;
  # seq_len over a clamped count returns no periods, where 1:horizon would have
  # counted down through zero and indexed the rate arrays out of range.
  horizon <- max(0, min(n_periods, remaining_term))
  for (t in seq_len(horizon)) {
    if (remaining_balance <= 0.01) {
      break
    }
    last_t <- t

    if (!flat) {
      monthly_rate <- current_annual_rate * dcf[t]
    }

    if (is_variable_rate) {
      if (months_to_reset <= 0) {
        current_annual_rate <- reference_rates[path_idx, t] + rate_margin
        if (flat) {
          # Kept as two divisions, which is what this has always computed.
          index_rate <- reference_rates[path_idx, t] / 12
          monthly_rate <- index_rate + (rate_margin / 12)
        } else {
          monthly_rate <- current_annual_rate * dcf[t]
        }

        if (monthly_rate > 0 && remaining_term - t + 1 > 0) {
          scheduled_payment <- remaining_balance * monthly_rate *
            (1 + monthly_rate)^(remaining_term - t + 1) /
            ((1 + monthly_rate)^(remaining_term - t + 1) - 1)
        } else {
          scheduled_payment <- remaining_balance / (remaining_term - t + 1)
        }
        months_to_reset <- reset_frequency
      } else {
        months_to_reset <- months_to_reset - 1
      }
    }

    smm <- prepay_rates[path_idx, t, asset_idx]
    mdr <- default_rates[path_idx, t, asset_idx]

    servicing_fees <- remaining_balance * if (flat) servicing_rate else servicing * dcf[t]
    interest_payment <- remaining_balance * monthly_rate
    principal_payment <- min(remaining_balance, scheduled_payment - interest_payment)
    if (principal_payment < 0) principal_payment <- 0

    balance_after_scheduled <- remaining_balance - principal_payment
    prepayment <- balance_after_scheduled * smm * (1 - mdr)
    default_amount <- balance_after_scheduled * mdr
    loss_amount <- default_amount

    if (track_defaults) {
      default_history[t] <- default_amount
      src <- t - recovery_lag
      recovery_amount <- if (src >= 1) default_history[src] * recovery_rate else 0
    } else {
      recovery_amount <- balance_after_scheduled * recovery_rates[path_idx, t, asset_idx]
    }

    new_balance <- remaining_balance - principal_payment - prepayment - default_amount

    cf_matrix[t, 1] <- new_balance
    cf_matrix[t, 2] <- principal_payment
    cf_matrix[t, 3] <- prepayment
    cf_matrix[t, 4] <- default_amount
    cf_matrix[t, 5] <- loss_amount
    cf_matrix[t, 6] <- recovery_amount
    cf_matrix[t, 7] <- interest_payment
    cf_matrix[t, 8] <- servicing_fees

    if (with_io) {
      io_fees <- remaining_balance * if (flat) io_strip_rate else io_strip * dcf[t]
      cf_matrix[t, 9] <- io_fees
      cf_matrix[t, 10] <- principal_payment + prepayment + interest_payment -
        servicing_fees - io_fees + recovery_amount
    } else {
      cf_matrix[t, 9] <- principal_payment + prepayment + interest_payment -
        servicing_fees + recovery_amount
    }

    remaining_balance <- new_balance
  }

  # Whatever is still outstanding when the projection stops. Dropping it prices
  # a long pool against a short horizon at a fraction of its worth, silently.
  if (identical(terminal_balance, "return") && last_t > 0 && remaining_balance > 0.01) {
    cf_matrix[last_t, 1] <- 0
    cf_matrix[last_t, 2] <- cf_matrix[last_t, 2] + remaining_balance
    cf_matrix[last_t, total_idx] <- cf_matrix[last_t, total_idx] + remaining_balance
  }

  cf_matrix
}

#' @title Raise the First Worker Error From a Parallel Projection (Internal)
#' @description Both parallel backends return a worker's error in band rather
#'   than raising it, and the per-task result is written into a slice of the
#'   cash flow array. A condition object recycles across that slice and turns
#'   it to character, so an unchecked error becomes a corrupt projection and a
#'   bare "all scheduled cores encountered errors in user code" warning. Raise
#'   the first one instead, naming the path and asset it came from.
#' @keywords internal
#' @noRd
.stop_on_task_error <- function(results, task_indices) {
  failed <- which(vapply(results, function(r) {
    inherits(r, "try-error") || inherits(r, "error") || inherits(r, "condition")
  }, logical(1)))
  if (length(failed) == 0L) return(invisible(NULL))

  k <- failed[1L]
  cond <- attr(results[[k]], "condition")
  msg <- if (inherits(cond, "condition")) {
    conditionMessage(cond)
  } else if (inherits(results[[k]], "condition")) {
    conditionMessage(results[[k]])
  } else {
    trimws(as.character(results[[k]]))
  }
  stop(sprintf(
    "Cash flow projection failed for %d of %d path-asset tasks. First failure (path %d, asset %d): %s",
    length(failed), length(results),
    task_indices$path_idx[k], task_indices$asset_idx[k], msg
  ), call. = FALSE)
}

#' @title Run the Projection Over Every Path and Asset (Internal)
#' @keywords internal
#' @noRd
.project_asset_grid <- function(n_paths, n_assets, n_periods, n_components, with_io,
                                asset_data, prepay_rates, default_rates, recovery_rates,
                                reference_rates, has_reference_rates,
                                run_parallel, n_cores, asset_dcf = NULL,
                                terminal_balance = "drop", recovery_basis = "balance",
                                recovery_rate = NULL, recovery_lag = NULL) {

  if (is.null(asset_dcf)) asset_dcf <- vector("list", n_assets)

  # Falls back to the balance basis when the credit vectors predate the
  # scalars it needs, so an externally built credit_vectors list still works.
  if (recovery_basis == "defaults" && (is.null(recovery_rate) || is.null(recovery_lag))) {
    recovery_basis <- "balance"
  }

  one <- function(path_idx, asset_idx) {
    .project_single_asset(path_idx, asset_idx, asset_data, prepay_rates,
                          default_rates, recovery_rates, reference_rates,
                          has_reference_rates, n_periods, n_components, with_io,
                          dcf = asset_dcf[[asset_idx]],
                          terminal_balance = terminal_balance,
                          recovery_basis = recovery_basis,
                          recovery_rate = recovery_rate,
                          recovery_lag = recovery_lag)
  }

  cashflows <- vector("list", n_paths)
  for (i in seq_len(n_paths)) {
    cashflows[[i]] <- array(0, dim = c(n_periods, n_components, n_assets))
  }

  if (run_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    task_indices <- expand.grid(path_idx = seq_len(n_paths), asset_idx = seq_len(n_assets))
    n_tasks <- nrow(task_indices)
    n_cores <- min(n_cores, n_tasks)

    if (.Platform$OS.type == "unix") {
      results <- parallel::mclapply(seq_len(n_tasks), function(k) {
        one(task_indices$path_idx[k], task_indices$asset_idx[k])
      }, mc.cores = n_cores)
      .stop_on_task_error(results, task_indices)
    } else {
      # Sent to the workers by value with a base environment, so the socket
      # cluster needs neither the package namespace nor a ::: call to reach it.
      worker_fn <- .project_single_asset
      environment(worker_fn) <- baseenv()

      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(
        cl,
        varlist = c("worker_fn", "asset_data", "prepay_rates", "default_rates",
                    "recovery_rates", "reference_rates", "has_reference_rates",
                    "n_periods", "n_components", "with_io", "task_indices",
                    "asset_dcf", "terminal_balance", "recovery_basis",
                    "recovery_rate", "recovery_lag"),
        envir = environment()
      )
      results <- parallel::parLapply(cl, seq_len(n_tasks), function(k) {
        worker_fn(
          task_indices$path_idx[k], task_indices$asset_idx[k], asset_data,
          prepay_rates, default_rates, recovery_rates, reference_rates,
          has_reference_rates, n_periods, n_components, with_io,
          asset_dcf[[task_indices$asset_idx[k]]], terminal_balance,
          recovery_basis, recovery_rate, recovery_lag
        )
      })
      .stop_on_task_error(results, task_indices)
    }

    for (k in seq_len(n_tasks)) {
      cashflows[[task_indices$path_idx[k]]][, , task_indices$asset_idx[k]] <- results[[k]]
    }

  } else {
    for (i in seq_len(n_paths)) {
      for (a in seq_len(n_assets)) {
        cashflows[[i]][, , a] <- one(i, a)
      }
    }
  }

  cashflows
}

#' @title Project Cash Flows for ABS/MBS Securities
#' @description Projects cash flows for asset-backed securities based on prepayment and default rates.
#' Supports parallel processing over paths and assets for improved performance.
#'
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param credit_vectors List. The result from \code{\link{calculate_credit_vectors}}
#' @param n_periods Integer. Number of periods to project
#' @param reference_rates Matrix. Optional reference rate matrix with dimensions `[n_paths, n_periods]`
#'        for variable rate loans, typically from \code{\link{extract_reference_rates}}.
#'        If NULL, fixed rates are used.
#' @param parallel Logical. Whether to use parallel processing. Default is FALSE.
#'        Internally aliased to \code{run_parallel} to avoid shadowing the
#'        \code{parallel} package. Both names are accepted.
#' @param run_parallel Logical. Alias for \code{parallel}. Preferred in new code.
#' @param n_cores Integer. Number of cores to use for parallel processing.
#'        If NULL, uses all available cores minus 1.
#' @param accrual_method Character. Day-count convention used when the asset
#'        data carries no \code{accrual_method} and no \code{Accrual} column of
#'        its own: one of \code{"30/360"} (the default), \code{"ACT/365"} or
#'        \code{"ACT/360"}. Resolution order is the loan's
#'        \code{accrual_method} column, then its \code{Accrual} column, then
#'        this argument. \code{"30/360"} is computed as one twelfth per period,
#'        which is what it means for a monthly-pay amortising loan, and is
#'        never derived from a calendar.
#' @param start_date Date. Anchor for the payment schedule, required by the ACT
#'        conventions and ignored by \code{"30/360"}. A per-loan
#'        \code{accrual_start_date} column takes precedence. With an ACT
#'        convention and no anchor the projection warns once and accrues on
#'        30/360.
#' @param on_short_horizon Character. What to do when \code{n_periods} is
#'        shorter than the longest remaining term: \code{"warn"} (the
#'        default), \code{"silent"} or \code{"error"}.
#' @param terminal_balance Character. \code{"return"}, the default, pays the
#'        balance outstanding when the projection stops as a final cash flow.
#'        \code{"drop"} discards it, which is the earlier behaviour.
#' @param recovery_basis Character. \code{"defaults"}, the default, takes
#'        recovery on the amount that defaulted \code{recovery_lag} months
#'        earlier. \code{"balance"} takes it on the balance standing at the
#'        time, which is the earlier behaviour.
#'
#' @return A list of cash flow arrays for each path, with dimensions `[time, component, asset]`
#'   where component is:
#'   \item{1}{Remaining balance}
#'   \item{2}{Scheduled principal}
#'   \item{3}{Prepayment}
#'   \item{4}{Default}
#'   \item{5}{Loss}
#'   \item{6}{Recovery}
#'   \item{7}{Interest}
#'   \item{8}{Servicing}
#'   \item{9}{Total payment}
#'
#' @export
project_cashflows <- function(asset_data, credit_vectors, n_periods = NULL, reference_rates = NULL,
                              parallel = FALSE, n_cores = NULL, run_parallel = parallel,
                              accrual_method = "30/360", start_date = NULL,
                              on_short_horizon = c("warn", "silent", "error"),
                              terminal_balance = c("return", "drop"),
                              recovery_basis = c("defaults", "balance")) {
  .project_cashflows_engine(
    asset_data = asset_data, credit_vectors = credit_vectors,
    n_periods = n_periods, reference_rates = reference_rates,
    n_cores = n_cores, run_parallel = run_parallel, with_io = FALSE,
    accrual_method = accrual_method, start_date = start_date,
    on_short_horizon = on_short_horizon, terminal_balance = terminal_balance,
    recovery_basis = recovery_basis
  )
}

#' @title Aggregate Cash Flows by Deal Structure
#' @description Aggregates loan-level cash flows into bond-level cash flows
#' based on a deal structure with specified waterfall rules.
#'
#' @param cashflows List. The result from \code{\link{project_cashflows}}
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param deal_structure List. Deal structure specification with waterfall rules
#' @param verbose Logical. Whether to print verbose output
#' @param reference_rates Matrix. Optional reference rate matrix with dimensions `[n_paths, n_periods]`
#'        for variable rate bonds, typically from \code{\link{extract_reference_rates}}.
#'        If NULL, fixed rates are used.
#'
#' @return A list of bond-level cash flow arrays for each path
#'
#' @examples
#' \dontrun{
#' # Simple sequential pay structure with 2 bonds
#' deal_structure <- list(
#'   bonds = data.frame(
#'     bond_id = c("A", "B"),
#'     balance = c(800000, 200000),
#'     coupon = c(0.04, 0.06),
#'     is_floating = c(TRUE, FALSE),
#'     spread = c(0.015, 0),     # 150bps spread for floating rate bond
#'     reset_frequency = c(1, 0) # Monthly reset for floating rate bond
#'   ),
#'   waterfall = list(
#'     principal = "sequential",   # sequential or pro_rata
#'     interest = "sequential",    # sequential or pro_rata
#'     losses = "reverse_sequential"  # reverse_sequential or pro_rata
#'   )
#' )
#'
#' # Use reference_rates extracted from simulated interest rates
#' dns_params <- dnsr::get_calibrated_dns_params()
#' rate_paths <- simulate_rates(dns_params, n_paths = 1, n_periods = 120)
#' reference_rates <- extract_reference_rates(rate_paths, "1")  # 1-year rate index
#'
#' bond_cashflows <- aggregate_cashflows(cashflows, asset_data, deal_structure,
#'                                       reference_rates = reference_rates)
#' }
#'
#' @export
aggregate_cashflows <- function(cashflows, asset_data, deal_structure, verbose = FALSE, reference_rates = NULL) {

  # Parameter validation
  checkmate::assert_list(cashflows)
  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(deal_structure)

  # Check for reference_rates - optional for backward compatibility
  has_reference_rates <- !is.null(reference_rates)
  if (has_reference_rates) {
    checkmate::assert_matrix(reference_rates)
  }

  # Validate deal structure
  if (!all(c("bonds", "waterfall") %in% names(deal_structure))) {
    stop("deal_structure must include 'bonds' and 'waterfall' components")
  }

  if (!all(c("bond_id", "balance", "coupon") %in% names(deal_structure$bonds))) {
    stop("deal_structure$bonds must include columns: bond_id, balance, coupon")
  }

  if (!all(c("principal", "interest", "losses") %in% names(deal_structure$waterfall))) {
    stop("deal_structure$waterfall must include rules for principal, interest, and losses")
  }

  # Get dimensions
  n_paths <- length(cashflows)
  n_periods <- dim(cashflows[[1]])[1]
  n_components <- dim(cashflows[[1]])[2]
  n_assets <- dim(cashflows[[1]])[3]
  n_bonds <- nrow(deal_structure$bonds)

  # Validate reference_rates dimensions if provided
  if (has_reference_rates) {
    if (nrow(reference_rates) != n_paths || ncol(reference_rates) < n_periods) {
      stop("reference_rates dimensions must match [n_paths, n_periods]")
    }
  }

  # Check if we have variable rate bonds
  has_floating_bonds <- FALSE
  if ("is_floating" %in% names(deal_structure$bonds)) {
    has_floating_bonds <- any(deal_structure$bonds$is_floating)
  }

  if (has_floating_bonds && !has_reference_rates) {
    stop("Floating rate bonds require reference_rates parameter")
  }

  # Initialize list to store bond-level cash flows
  bond_cashflows <- vector("list", n_paths)

  # Process each path
  for (i in 1:n_paths) {
    if (verbose && i %% 10 == 0) {
      cat("Processing path", i, "of", n_paths, "\n")
    }

    # Initialize bond cash flow array
    # Dimensions: [time, component, bond]
    # Components: [1] remaining_balance, [2] principal, [3] interest, [4] losses, [5] total
    bond_cf_array <- array(0, dim = c(n_periods, 5, n_bonds))

    # Set initial bond balances
    bond_balances <- deal_structure$bonds$balance

    # Initialize rate reset tracking for floating rate bonds
    bond_coupons <- deal_structure$bonds$coupon
    months_to_reset <- rep(0, n_bonds)

    if (has_floating_bonds) {
      # Extract floating rate bond properties
      is_floating <- deal_structure$bonds$is_floating
      spread <- rep(0, n_bonds)
      reset_frequency <- rep(1, n_bonds)  # Default to monthly reset

      if ("spread" %in% names(deal_structure$bonds)) {
        spread <- deal_structure$bonds$spread
      }

      if ("reset_frequency" %in% names(deal_structure$bonds)) {
        reset_frequency <- deal_structure$bonds$reset_frequency
        reset_frequency[reset_frequency <= 0] <- 1  # Default to 1 for any invalid values
      }

      # Initialize countdown to next reset for each bond
      months_to_reset <- ifelse(is_floating, 0, -1)  # -1 means never reset (fixed rate)
    }

    # Process each time period
    for (t in 1:n_periods) {
      # Skip if all bonds are paid off
      if (sum(bond_balances) <= 0.01) {
        break
      }

      # Update floating rate bond coupons if needed
      if (has_floating_bonds) {
        for (b in 1:n_bonds) {
          if (deal_structure$bonds$is_floating[b]) {
            if (months_to_reset[b] <= 0) {
              # Reset the coupon rate based on index value + spread
              index_rate <- reference_rates[i, t]
              bond_coupons[b] <- index_rate + spread[b]

              # Reset countdown to next reset
              months_to_reset[b] <- reset_frequency[b]
            } else {
              months_to_reset[b] <- months_to_reset[b] - 1
            }
          }
        }
      }

      # Aggregate loan-level cash flows for this time period. Reshaped
      # explicitly because `[t, , ]` collapses to a vector for a one-asset pool.
      loan_cf <- matrix(cashflows[[i]][t, , ], nrow = n_components, ncol = n_assets)

      # Sum across all loans
      total_principal <- sum(loan_cf[2, ]) + sum(loan_cf[3, ])  # Scheduled + prepayments
      total_interest <- sum(loan_cf[7, ]) - sum(loan_cf[8, ])  # Interest - servicing
      total_losses <- sum(loan_cf[5, ]) - sum(loan_cf[6, ])  # Losses - recoveries

      # Allocate principal based on waterfall rule
      principal_allocation <- numeric(n_bonds)

      if (deal_structure$waterfall$principal == "sequential") {
        # Sequential pay: pay down bonds in order
        remaining_principal <- total_principal

        for (b in 1:n_bonds) {
          principal_allocation[b] <- min(remaining_principal, bond_balances[b])
          remaining_principal <- remaining_principal - principal_allocation[b]

          if (remaining_principal <= 0) {
            break
          }
        }
      } else if (deal_structure$waterfall$principal == "pro_rata") {
        # Pro-rata: allocate proportionally to current balance
        total_bond_balance <- sum(bond_balances)
        if (total_bond_balance > 0) {
          principal_allocation <- total_principal * (bond_balances / total_bond_balance)

          # Ensure we don't allocate more than remaining balance
          principal_allocation <- pmin(principal_allocation, bond_balances)
        }
      } else {
        stop("Unknown principal waterfall rule: ", deal_structure$waterfall$principal)
      }

      # Allocate interest based on waterfall rule
      interest_allocation <- numeric(n_bonds)

      # Calculate interest due on each bond using current coupon rates
      interest_due <- bond_balances * (bond_coupons / 12)

      if (deal_structure$waterfall$interest == "sequential") {
        # Sequential pay: pay interest in order
        remaining_interest <- total_interest

        for (b in 1:n_bonds) {
          interest_allocation[b] <- min(remaining_interest, interest_due[b])
          remaining_interest <- remaining_interest - interest_allocation[b]

          if (remaining_interest <= 0) {
            break
          }
        }
      } else if (deal_structure$waterfall$interest == "pro_rata") {
        # Pro-rata: allocate proportionally to interest due
        total_interest_due <- sum(interest_due)
        if (total_interest_due > 0) {
          interest_allocation <- total_interest * (interest_due / total_interest_due)
        }
      } else {
        stop("Unknown interest waterfall rule: ", deal_structure$waterfall$interest)
      }

      # Allocate losses based on waterfall rule
      loss_allocation <- numeric(n_bonds)

      if (deal_structure$waterfall$losses == "reverse_sequential") {
        # Reverse sequential: allocate losses to lowest bonds first
        remaining_losses <- total_losses

        for (b in n_bonds:1) {
          loss_allocation[b] <- min(remaining_losses, bond_balances[b])
          remaining_losses <- remaining_losses - loss_allocation[b]

          if (remaining_losses <= 0) {
            break
          }
        }
      } else if (deal_structure$waterfall$losses == "pro_rata") {
        # Pro-rata: allocate proportionally to current balance
        total_bond_balance <- sum(bond_balances)
        if (total_bond_balance > 0) {
          loss_allocation <- total_losses * (bond_balances / total_bond_balance)

          # Ensure we don't allocate more than remaining balance
          loss_allocation <- pmin(loss_allocation, bond_balances)
        }
      } else {
        stop("Unknown losses waterfall rule: ", deal_structure$waterfall$losses)
      }

      # Update bond balances
      new_bond_balances <- bond_balances - principal_allocation - loss_allocation

      # Store bond cash flows
      for (b in 1:n_bonds) {
        bond_cf_array[t, 1, b] <- new_bond_balances[b]  # Remaining balance
        bond_cf_array[t, 2, b] <- principal_allocation[b]  # Principal
        bond_cf_array[t, 3, b] <- interest_allocation[b]  # Interest
        bond_cf_array[t, 4, b] <- loss_allocation[b]  # Losses
        bond_cf_array[t, 5, b] <- principal_allocation[b] + interest_allocation[b]  # Total
      }

      # Update for next period
      bond_balances <- new_bond_balances
    }

    # Store bond cash flows for this path
    bond_cashflows[[i]] <- bond_cf_array
  }

  return(bond_cashflows)
}

#' @title Calculate Weighted Average Life (WAL)
#' @description Calculates the weighted average life of cash flows.
#'
#' @param cashflows List. The result from \code{\link{project_cashflows}} or \code{\link{aggregate_cashflows}}
#' @param asset_level Logical. Whether the cashflows are at the asset level or bond level
#' @param balance_idx Integer. Index of the remaining balance component in cashflows
#' @param principal_idx Integer or vector. Index(es) of the principal component(s) in cashflows
#'
#' @return A list containing average lives by asset/bond and path
#'
#' @export
calculate_average_life <- function(cashflows, asset_level = TRUE,
                                 balance_idx = 1, principal_idx = c(2, 3)) {

  # Parameter validation
  checkmate::assert_list(cashflows)
  checkmate::assert_logical(asset_level)

  # Get dimensions
  n_paths <- length(cashflows)
  n_periods <- dim(cashflows[[1]])[1]
  n_items <- dim(cashflows[[1]])[3]  # Assets or bonds

  # Initialize matrix for average lives (paths x items)
  avg_lives_matrix <- matrix(0, n_paths, n_items)

  # Process each path
  for (i in 1:n_paths) {
    cf_array <- cashflows[[i]]

    # Process each asset/bond
    for (j in 1:n_items) {
      # Read only the principal columns, rather than materialising the whole
      # period-by-component matrix to reach them. Same values.
      principal_cf <- numeric(n_periods)
      for (p_idx in principal_idx) {
        principal_cf <- principal_cf + cf_array[, p_idx, j]
      }

      # Every non-zero principal movement, so a writedown or writeup carries its
      # own sign into the average rather than being discarded.
      non_zero_idx <- which(principal_cf != 0)
      principal_total <- sum(principal_cf[non_zero_idx])

      if (length(non_zero_idx) > 0 && principal_total != 0) {
        # Calculate weighted average time
        times <- non_zero_idx / 12  # Convert to years
        avg_lives_matrix[i, j] <- sum(principal_cf[non_zero_idx] * times) / principal_total
      }
    }
  }

  # Calculate average across paths for each item
  avg_lives <- colMeans(avg_lives_matrix)

  # Calculate weighted average life for the overall portfolio
  # Use equal weights if no other information is available
  overall_wal <- mean(avg_lives)

  return(list(
    avg_lives = list(
      by_asset = avg_lives,
      overall_wal = overall_wal
    ),
    avg_lives_by_path = avg_lives_matrix
  ))
}

#' @title Project Cash Flows for ABS/MBS Securities with IO Strip
#' @description Projects cash flows for asset-backed securities with IO strip based on prepayment and default rates.
#' Supports parallel processing over paths and assets for improved performance.
#'
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param credit_vectors List. The result from \code{\link{calculate_credit_vectors}}
#' @param n_periods Integer. Number of periods to project
#' @param reference_rates Matrix. Optional reference rate matrix with dimensions `[n_paths, n_periods]`
#'        for variable rate loans, typically from \code{\link{extract_reference_rates}}.
#'        If NULL, fixed rates are used.
#' @param parallel Logical. Whether to use parallel processing. Default is FALSE.
#'        Internally aliased to \code{run_parallel} to avoid shadowing the
#'        \code{parallel} package. Both names are accepted.
#' @param run_parallel Logical. Alias for \code{parallel}. Preferred in new code.
#' @param n_cores Integer. Number of cores to use for parallel processing.
#'        If NULL, uses all available cores minus 1.
#' @param accrual_method Character. Day-count convention used when the asset
#'        data carries no \code{accrual_method} and no \code{Accrual} column of
#'        its own: one of \code{"30/360"} (the default), \code{"ACT/365"} or
#'        \code{"ACT/360"}. Resolution order is the loan's
#'        \code{accrual_method} column, then its \code{Accrual} column, then
#'        this argument. \code{"30/360"} is computed as one twelfth per period,
#'        which is what it means for a monthly-pay amortising loan, and is
#'        never derived from a calendar.
#' @param start_date Date. Anchor for the payment schedule, required by the ACT
#'        conventions and ignored by \code{"30/360"}. A per-loan
#'        \code{accrual_start_date} column takes precedence. With an ACT
#'        convention and no anchor the projection warns once and accrues on
#'        30/360.
#' @param on_short_horizon Character. What to do when \code{n_periods} is
#'        shorter than the longest remaining term: \code{"warn"} (the
#'        default), \code{"silent"} or \code{"error"}.
#' @param terminal_balance Character. \code{"return"}, the default, pays the
#'        balance outstanding when the projection stops as a final cash flow.
#'        \code{"drop"} discards it, which is the earlier behaviour.
#' @param recovery_basis Character. \code{"defaults"}, the default, takes
#'        recovery on the amount that defaulted \code{recovery_lag} months
#'        earlier. \code{"balance"} takes it on the balance standing at the
#'        time, which is the earlier behaviour.
#'
#' @return A list of cash flow arrays for each path, with dimensions `[time, component, asset]`
#'   where component is:
#'   \item{1}{Remaining balance}
#'   \item{2}{Scheduled principal}
#'   \item{3}{Prepayment}
#'   \item{4}{Default}
#'   \item{5}{Loss}
#'   \item{6}{Recovery}
#'   \item{7}{Interest}
#'   \item{8}{Servicing}
#'   \item{9}{IO strip}
#'   \item{10}{Total payment}
#'
#' @export
project_cashflows_with_io <- function(asset_data, credit_vectors, n_periods = NULL, reference_rates = NULL,
                                      parallel = FALSE, n_cores = NULL, run_parallel = parallel,
                                      accrual_method = "30/360", start_date = NULL,
                                      on_short_horizon = c("warn", "silent", "error"),
                                      terminal_balance = c("return", "drop"),
                                      recovery_basis = c("defaults", "balance")) {
  .project_cashflows_engine(
    asset_data = asset_data, credit_vectors = credit_vectors,
    n_periods = n_periods, reference_rates = reference_rates,
    n_cores = n_cores, run_parallel = run_parallel, with_io = TRUE,
    accrual_method = accrual_method, start_date = start_date,
    on_short_horizon = on_short_horizon, terminal_balance = terminal_balance,
    recovery_basis = recovery_basis
  )
}
