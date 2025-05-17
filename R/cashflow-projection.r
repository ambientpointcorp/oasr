#' @title Project Cash Flows for ABS/MBS Securities
#' @description Projects cash flows for asset-backed securities based on prepayment and default rates.
#'
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param credit_vectors List. The result from \code{\link{calculate_credit_vectors}}
#' @param n_periods Integer. Number of periods to project
#' @param reference_rates Matrix. Optional reference rate matrix with dimensions [n_paths, n_periods]
#'        for variable rate loans, typically from \code{\link{extract_reference_rates}}. 
#'        If NULL, fixed rates are used.
#'
#' @return A list of cash flow arrays for each path, with dimensions [time, component, asset]
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
project_cashflows <- function(asset_data, credit_vectors, n_periods = NULL, reference_rates = NULL) {

  # Parameter validation
  checkmate::assert_data_frame(asset_data)
  checkmate::assert_list(credit_vectors)
  
  # Check for reference_rates - optional for backward compatibility
  has_reference_rates <- !is.null(reference_rates) 
  if (has_reference_rates) {
    checkmate::assert_matrix(reference_rates)
  }

  # Extract credit vectors
  prepay_rates <- credit_vectors$prepay_rates
  default_rates <- credit_vectors$default_rates
  recovery_rates <- credit_vectors$recovery_rates

  # Get dimensions
  n_paths <- dim(prepay_rates)[1]
  if (is.null(n_periods)) {
    n_periods <- dim(prepay_rates)[2]
  } else {
    n_periods <- min(n_periods, dim(prepay_rates)[2])
  }
  n_assets <- dim(prepay_rates)[3]
  
  # Validate reference_rates dimensions if provided
  if (has_reference_rates) {
    if (nrow(reference_rates) != n_paths || ncol(reference_rates) < n_periods) {
      stop("reference_rates dimensions must match [n_paths, n_periods]")
    }
  }

  # Check that asset_data has the right number of rows
  if (nrow(asset_data) != n_assets) {
    stop("Number of assets in asset_data (", nrow(asset_data),
         ") does not match credit_vectors (", n_assets, ")")
  }

  # Initialize list to store cash flows
  cashflows <- vector("list", n_paths)

  # Process each path
  for (i in 1:n_paths) {
    # Initialize cashflow array for this path
    # Dimensions: [time, component, asset]
    # Components: [1] remaining_balance, [2] scheduled_principal, [3] prepayment,
    #             [4] default, [5] loss, [6] recovery, [7] interest, [8] servicing, [9] total
    cf_array <- array(0, dim = c(n_periods, 9, n_assets))

    # Process each asset
    for (a in 1:n_assets) {
      # Get asset-specific data
      asset <- asset_data[a, ]

      # Extract required loan characteristics
      if (!all(c("balance", "servicing", "wac", "wam") %in% names(asset))) {
        stop("asset_data must include columns: balance, servicing, wac, wam")
      }

      original_balance <- asset$balance
      servicing <- asset$servicing
      wac <- asset$wac
      wam <- asset$wam

      # Extract optional loan characteristics
      age <- if ("age" %in% names(asset)) asset$age else 0
      
      # Extract variable rate characteristics if available
      is_variable_rate <- FALSE
      rate_margin <- 0
      reset_frequency <- 0
      next_reset <- 0
      
      if (all(c("is_variable_rate", "rate_margin") %in% names(asset))) {
        is_variable_rate <- isTRUE(asset$is_variable_rate)
        rate_margin <- if (is_variable_rate) asset$rate_margin else 0
        
        # Optional reset parameters
        reset_frequency <- if ("reset_frequency" %in% names(asset) && is_variable_rate) 
                              asset$reset_frequency else 12  # Default to annual reset
        next_reset <- if ("next_reset" %in% names(asset) && is_variable_rate) 
                        asset$next_reset else 0  # Default to immediate reset
      }
      
      # Variable rate requires reference_rates
      if (is_variable_rate && !has_reference_rates) {
        stop("Variable rate loans require reference_rates parameter")
      }

      # Calculate remaining term
      remaining_term <- wam - age

      # Calculate monthly servicing rate
      servicing_rate <- servicing / 12

      # Calculate initial monthly interest rate
      monthly_rate <- wac / 12
      
      # For variable rate loans, calculate payment based on fully-indexed rate at origination
      # But store current monthly rate for interest calculations
      payment_monthly_rate <- monthly_rate

      # Calculate scheduled monthly payment (fixed for the life of the loan)
      # P = L * r * (1 + r)^n / ((1 + r)^n - 1)
      if (payment_monthly_rate > 0 && remaining_term > 0) {
        scheduled_payment <- original_balance * payment_monthly_rate * (1 + payment_monthly_rate)^remaining_term /
          ((1 + payment_monthly_rate)^remaining_term - 1)
      } else {
        # Handle edge case where rate is 0 or term is 0
        scheduled_payment <- original_balance / remaining_term
      }

      # Set initial balance
      remaining_balance <- original_balance
      
      # Track months until next reset
      months_to_reset <- next_reset

      # Project cash flows for this asset
      for (t in 1:min(n_periods, remaining_term)) {
        # Skip if loan is fully paid off
        if (remaining_balance <= 0.01) {
          break
        }
        
        # Handle rate reset for variable rate loans
        if (is_variable_rate) {
          if (months_to_reset <= 0) {
            # Reset the rate based on index value + margin
            index_rate <- reference_rates[i, t] / 12  # Convert annual to monthly
            monthly_rate <- index_rate + (rate_margin / 12)
            
            # Reset countdown to next reset
            months_to_reset <- reset_frequency
          } else {
            months_to_reset <- months_to_reset - 1
          }
        }

        # Get prepayment and default rates for this path, time, asset
        smm <- prepay_rates[i, t, a]
        mdr <- default_rates[i, t, a]

        # Calculate servicing fees
        servicing_fees <- remaining_balance * servicing_rate

        # Calculate interest based on current rate
        interest_payment <- remaining_balance * monthly_rate

        # Calculate scheduled principal (payment amount is fixed)
        principal_payment <- min(remaining_balance, scheduled_payment - interest_payment)
        
        # Handle edge case where interest exceeds scheduled payment (negative amortization)
        if (principal_payment < 0) {
          principal_payment <- 0
        }

        # Calculate remaining balance after scheduled payment
        balance_after_scheduled <- remaining_balance - principal_payment

        # Calculate prepayment amount
        prepayment <- balance_after_scheduled * smm * (1 - mdr)

        # Calculate default amount
        default_amount <- balance_after_scheduled * mdr

        # Calculate loss amount (default minus recovery)
        loss_amount <- default_amount

        # Recovery is accounted for separately in recovery_rates
        recovery_amount <- recovery_rates[i, t, a] * remaining_balance

        # Update remaining balance
        new_balance <- remaining_balance - principal_payment - prepayment - default_amount

        # Store cash flow components
        cf_array[t, 1, a] <- new_balance                   # Remaining balance
        cf_array[t, 2, a] <- principal_payment             # Scheduled principal
        cf_array[t, 3, a] <- prepayment                    # Prepayment
        cf_array[t, 4, a] <- default_amount                # Default
        cf_array[t, 5, a] <- loss_amount                   # Loss
        cf_array[t, 6, a] <- recovery_amount               # Recovery
        cf_array[t, 7, a] <- interest_payment              # Interest
        cf_array[t, 8, a] <- servicing_fees                # Servicing
        cf_array[t, 9, a] <- principal_payment + prepayment + interest_payment - servicing_fees + recovery_amount  # Total payment

        # Update for next period
        remaining_balance <- new_balance
      }
    }

    # Store cash flows for this path
    cashflows[[i]] <- cf_array
  }

  return(cashflows)
}

#' @title Aggregate Cash Flows by Deal Structure
#' @description Aggregates loan-level cash flows into bond-level cash flows
#' based on a deal structure with specified waterfall rules.
#'
#' @param cashflows List. The result from \code{\link{project_cashflows}}
#' @param asset_data Data frame. Asset data with loan characteristics
#' @param deal_structure List. Deal structure specification with waterfall rules
#' @param verbose Logical. Whether to print verbose output
#' @param reference_rates Matrix. Optional reference rate matrix with dimensions [n_paths, n_periods]
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

      # Aggregate loan-level cash flows for this time period
      loan_cf <- cashflows[[i]][t, , ]

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
      # Extract cash flows for this item
      if (asset_level) {
        # Asset-level cash flows
        cf <- cf_array[, , j]
      } else {
        # Bond-level cash flows
        cf <- cf_array[, , j]
      }

      # Find periods with non-zero principal payments
      principal_cf <- numeric(n_periods)
      for (p_idx in principal_idx) {
        principal_cf <- principal_cf + cf[, p_idx]
      }

      non_zero_idx <- which(principal_cf > 0)

      if (length(non_zero_idx) > 0) {
        # Calculate weighted average time
        times <- non_zero_idx / 12  # Convert to years
        avg_lives_matrix[i, j] <- sum(principal_cf[non_zero_idx] * times) / sum(principal_cf[non_zero_idx])
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

# Remove the extract_rate_matrix function since we're using extract_reference_rates directly
