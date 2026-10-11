# Extract Reference Rates from Simulated Rate Paths

Extracts a specific reference rate (e.g., 1-year, 10-year) from the
simulated rate paths for use in cash flow projection.

## Usage

``` r
extract_reference_rates(rate_paths, reference_key = "10")
```

## Arguments

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- reference_key:

  Character. The key for the reference rate to extract (e.g., "10" for
  10-year)

## Value

A matrix of reference rates with dimensions `[path, time]`

## Examples

``` r
if (FALSE) { # \dontrun{
rate_paths <- simulate_rates(dns_params, n_paths = 100)
reference_rates <- extract_reference_rates(rate_paths, "10")
} # }
```
