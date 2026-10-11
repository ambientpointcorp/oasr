# Binary Search OAS with Fixed Cash Flows

Performs a binary search to find the OAS that matches a target price for
a fixed set of cash flows.

## Usage

``` r
binary_search_oas(
  cashflows,
  times,
  rate_paths,
  market_price,
  search_range = c(-500, 500),
  tolerance = 0.01,
  max_iterations = 50
)
```

## Arguments

- cashflows:

  Numeric vector. Cash flows

- times:

  Numeric vector. Times (in years) for each cash flow

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- market_price:

  Numeric. Market price of the security

- search_range:

  Numeric vector. Range of OAS values to search (in basis points)

- tolerance:

  Numeric. Tolerance for price convergence

- max_iterations:

  Integer. Maximum number of iterations for the search

## Value

A list containing the OAS calculation results
