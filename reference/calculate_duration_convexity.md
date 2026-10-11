# Calculate Effective Duration and Convexity

Calculates the effective duration and convexity of cash flows by
shocking the interest rates.

## Usage

``` r
calculate_duration_convexity(
  cashflows,
  rate_paths,
  oas,
  shock = 100,
  reference_key = "10",
  asset_level = TRUE,
  total_idx = 9,
  discounting = c("path", "spot"),
  reproject = NULL
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

- shock:

  Numeric. Interest rate shock in basis points

- reference_key:

  Character. Key for the reference rate to use

- asset_level:

  Logical. Whether the cashflows are at the asset level or bond level

- total_idx:

  Integer. Index of the total cash flow component in cashflows

- discounting:

  Character. Passed to
  [`calculate_present_value`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md).

- reproject:

  Function or NULL. Given a shocked `rate_paths`, returns the cash flows
  projected under it. Supplying one makes this a true effective
  duration, with prepayments responding to the shock. Left NULL the same
  fixed cash flows are repriced at both shocks, which is a spread
  duration; it stays the default because re-projecting roughly triples
  the cost, not because it is the better measure.

## Value

A list containing:

- duration:

  Vector of price sensitivities, one per asset or bond, computed as
  `-(P_down - P_up) / (2 * P_base * shock)`. This is the signed
  logarithmic derivative `(1/P) dP/dy`, so it is negative for an
  instrument whose price falls as yields rise. Traditional fixed income
  negates this to print a positive number; the signed form is kept here
  because it remains correct for instruments whose price rises with
  yield, such as IO strips and inverse floaters. Negate it if you want
  the conventional reading.

- convexity:

  Vector of effective convexities for each asset/bond

- price_up:

  Vector of prices with upward rate shock

- price_base:

  Vector of prices with base rates

- price_down:

  Vector of prices with downward rate shock
