# Calculate Prepayment and Default Rates (Vectorized & Parallelized)

Calculates prepayment and default rates for cash flow projection based
on the prepayment model, asset characteristics, and interest rate paths.
This version replaces nested for-loops with vectorized matrix operations
and supports optional parallel processing across assets via the
`parallel` package.

## Usage

``` r
calculate_credit_vectors(
  asset_data,
  rate_paths,
  prepay_model,
  reference_key = "10",
  parallel = FALSE,
  n_cores = NULL,
  run_parallel = parallel
)
```

## Arguments

- asset_data:

  Data frame. Asset data with characteristics

- rate_paths:

  List. Result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- prepay_model:

  List. Prepayment model from
  [`prepayment_model`](https://ambientpointcorp.github.io/oasr/reference/prepayment_model.md)

- reference_key:

  Character. Key for the reference rate to use

- parallel:

  Logical. Deprecated spelling of `run_parallel`, kept because existing
  callers pass it. Prefer `run_parallel`.

- n_cores:

  Integer. Number of cores to use for parallel processing. If NULL, uses
  all available cores minus 1.

- run_parallel:

  Logical. Whether to use parallel processing across assets. Default is
  FALSE. Note: this parameter is named `run_parallel` (not `parallel`)
  to avoid shadowing the `parallel` package namespace. For backward
  compatibility, `parallel` is also accepted.

## Value

A list containing:

- prepay_rates:

  Array of prepayment rates with dimensions `[path, time, asset]`

- default_rates:

  Array of default rates with dimensions `[path, time, asset]`

- recovery_rates:

  Array of recovery rates with dimensions `[path, time, asset]`
