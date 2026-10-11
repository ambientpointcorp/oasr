# Plot Yield Curves

Creates plots of yield curves at different time points.

## Usage

``` r
plot_yield_curves(rate_paths, time_points = c(0, 1, 5, 10), path_idx = 1)
```

## Arguments

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- time_points:

  Numeric vector. Time points (in years) to plot

- path_idx:

  Integer. Which path to plot

## Value

A ggplot object
