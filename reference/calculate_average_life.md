# Calculate Weighted Average Life (WAL)

Calculates the weighted average life of cash flows.

## Usage

``` r
calculate_average_life(
  cashflows,
  asset_level = TRUE,
  balance_idx = 1,
  principal_idx = c(2, 3)
)
```

## Arguments

- cashflows:

  List. The result from
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  or
  [`aggregate_cashflows`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)

- asset_level:

  Logical. Whether the cashflows are at the asset level or bond level

- balance_idx:

  Integer. Index of the remaining balance component in cashflows

- principal_idx:

  Integer or vector. Index(es) of the principal component(s) in
  cashflows

## Value

A list containing average lives by asset/bond and path
