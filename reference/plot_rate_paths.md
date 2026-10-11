# Plot Rate Paths

Creates plots of interest rate paths.

## Usage

``` r
plot_rate_paths(rate_paths, maturity = "10", sample_size = 10, seed = NULL)
```

## Arguments

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- maturity:

  Character. Which maturity to plot (e.g., "10" for 10-year)

- sample_size:

  Integer. Number of paths to sample

- seed:

  Integer. Optional seed for the path sample, so the plot reproduces

## Value

A ggplot object
