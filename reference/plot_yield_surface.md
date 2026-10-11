# Plot Yield Surface

Creates a 3D yield surface plot.

## Usage

``` r
plot_yield_surface(rate_paths, interactive = TRUE)
```

## Arguments

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- interactive:

  Logical. Whether to create an interactive 3D plot

## Value

A plot object

## Examples

``` r
if (FALSE) { # \dontrun{
rate_paths <- simulate_rates(dns_params, n_paths = 100)
plot_yield_surface(rate_paths)
} # }
```
