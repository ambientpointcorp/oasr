# Project Cash Flows for ABS/MBS Securities with IO Strip

Projects cash flows for asset-backed securities with IO strip based on
prepayment and default rates. Supports parallel processing over paths
and assets for improved performance.

## Usage

``` r
project_cashflows_with_io(
  asset_data,
  credit_vectors,
  n_periods = NULL,
  reference_rates = NULL,
  parallel = FALSE,
  n_cores = NULL,
  run_parallel = parallel,
  accrual_method = "30/360",
  start_date = NULL,
  on_short_horizon = c("warn", "silent", "error"),
  terminal_balance = c("return", "drop"),
  recovery_basis = c("defaults", "balance")
)
```

## Arguments

- asset_data:

  Data frame. Asset data with loan characteristics

- credit_vectors:

  List. The result from
  [`calculate_credit_vectors`](https://ambientpointcorp.github.io/oasr/reference/calculate_credit_vectors.md)

- n_periods:

  Integer. Number of periods to project

- reference_rates:

  Matrix. Optional reference rate matrix with dimensions
  `[n_paths, n_periods]` for variable rate loans, typically from
  [`extract_reference_rates`](https://ambientpointcorp.github.io/oasr/reference/extract_reference_rates.md).
  If NULL, fixed rates are used.

- parallel:

  Logical. Whether to use parallel processing. Default is FALSE.
  Internally aliased to `run_parallel` to avoid shadowing the `parallel`
  package. Both names are accepted.

- n_cores:

  Integer. Number of cores to use for parallel processing. If NULL, uses
  all available cores minus 1.

- run_parallel:

  Logical. Alias for `parallel`. Preferred in new code.

- accrual_method:

  Character. Day-count convention used when the asset data carries no
  `accrual_method` and no `Accrual` column of its own: one of `"30/360"`
  (the default), `"ACT/365"` or `"ACT/360"`. Resolution order is the
  loan's `accrual_method` column, then its `Accrual` column, then this
  argument. `"30/360"` is computed as one twelfth per period, which is
  what it means for a monthly-pay amortising loan, and is never derived
  from a calendar.

- start_date:

  Date. Anchor for the payment schedule, required by the ACT conventions
  and ignored by `"30/360"`. A per-loan `accrual_start_date` column
  takes precedence. With an ACT convention and no anchor the projection
  warns once and accrues on 30/360.

- on_short_horizon:

  Character. What to do when `n_periods` is shorter than the longest
  remaining term: `"warn"` (the default), `"silent"` or `"error"`.

- terminal_balance:

  Character. `"return"`, the default, pays the balance outstanding when
  the projection stops as a final cash flow. `"drop"` discards it, which
  is the earlier behaviour.

- recovery_basis:

  Character. `"defaults"`, the default, takes recovery on the amount
  that defaulted `recovery_lag` months earlier. `"balance"` takes it on
  the balance standing at the time, which is the earlier behaviour.

## Value

A list of cash flow arrays for each path, with dimensions
`[time, component, asset]` where component is:

- 1:

  Remaining balance

- 2:

  Scheduled principal

- 3:

  Prepayment

- 4:

  Default

- 5:

  Loss

- 6:

  Recovery

- 7:

  Interest

- 8:

  Servicing

- 9:

  IO strip

- 10:

  Total payment
