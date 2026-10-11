# Get Discount Factors from Rate Paths

Calculates discount factors for each path and time point based on a
reference rate plus a spread.

## Usage

``` r
get_discount_factors(
  rate_paths,
  reference_key = "10",
  spread = 0,
  times = NULL
)
```

## Arguments

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- reference_key:

  Character. The key for the reference rate to use

- spread:

  Numeric. Spread to add to the reference rate (in decimal form)

- times:

  Numeric vector. Time points (in years) for which to calculate discount
  factors

## Value

A list with one matrix per path, each containing discount factors for
the specified times

## Examples

``` r
if (FALSE) { # \dontrun{
rate_paths <- simulate_rates(dns_params, n_paths = 100)
times <- seq(1/12, 30, by = 1/12)
discount_factors <- get_discount_factors(rate_paths, "10", 0.01, times)
} # }
```
