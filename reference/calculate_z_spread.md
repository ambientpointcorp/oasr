# Calculate Z-Spread

Calculates the Z-spread (zero-volatility spread) for fixed cash flows.

## Usage

``` r
calculate_z_spread(
  cashflows,
  times,
  market_price,
  spot_rates,
  initial_spread = 100
)
```

## Arguments

- cashflows:

  Numeric vector. Fixed cash flows

- times:

  Numeric vector. Times (in years) for each cash flow

- market_price:

  Numeric. Market price of the security

- spot_rates:

  Numeric vector. Spot rates for each time point

- initial_spread:

  Numeric. Initial guess for the Z-spread in basis points

## Value

A list containing:

- z_spread:

  The calculated Z-spread in basis points

- price:

  The calculated price using the Z-spread

- error:

  The pricing error
