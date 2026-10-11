# Calculate Price from Cash Flows

Calculates the price of a security from its cash flows using spot rates
plus a spread.

## Usage

``` r
calculate_price(cashflows, times, spot_rates, spread = 0)
```

## Arguments

- cashflows:

  Numeric vector. Cash flows

- times:

  Numeric vector. Times (in years) for each cash flow

- spot_rates:

  Numeric vector. Spot rates for each time point

- spread:

  Numeric. Spread in basis points

## Value

The calculated price
