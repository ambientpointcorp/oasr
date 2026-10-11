# Calculate Present Value of Cash Flows

Calculates the present value of cash flows using a reference rate plus a
spread.

## Usage

``` r
calculate_present_value(
  cashflows,
  rate_paths,
  oas,
  reference_key = "10",
  asset_level = TRUE,
  total_idx = 9,
  discounting = c("path", "spot")
)
```

## Arguments

- cashflows:

  List. The result from
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  or
  [`aggregate_cashflows`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- oas:

  Numeric. Option-adjusted spread in basis points

- reference_key:

  Character. Key for the reference rate to use

- asset_level:

  Logical. Whether the cashflows are at the asset level or bond level

- total_idx:

  Integer. Index of the total cash flow component in cashflows

- discounting:

  Character. `"path"` compounds the short rate along each simulated
  path, which is the Monte Carlo definition of an option-adjusted
  spread. `"spot"` is the earlier behaviour: each cash flow discounted
  at the reference rate standing in its own period, taken as a zero rate
  to that maturity.

## Value

A list containing:

- present_values:

  Matrix of present values for each path and asset/bond

- mean_pvs:

  Vector of mean present values for each asset/bond

- pv_stats:

  Data frame with statistics about present values
