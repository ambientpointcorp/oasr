# Simulate Interest Rate Paths for OAS Analysis

Interfaces with the dnsr package to simulate interest rate paths for use
in option-adjusted spread analysis.

## Usage

``` r
simulate_rates(
  dns_params,
  n_paths = 100,
  n_periods = 360,
  maturities = c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30),
  frequency = "monthly",
  seed = NULL,
  engine = c("yuima", "exact"),
  include_t0 = TRUE
)
```

## Arguments

- dns_params:

  List. Model parameters from dnsr package, typically from
  [`dnsr::get_calibrated_dns_params()`](https://rdrr.io/pkg/dnsr/man/get_calibrated_dns_params.html).
  Its `lambda` element is the Nelson-Siegel decay rate, in the same
  units as `maturities` – that is, per year. Diebold and Li's
  conventional 0.0609 is per month, so the equivalent here is
  `0.0609 * 12 = 0.7308`. Passing the per-month value against a grid in
  years puts the curvature hump 29 years out, which leaves the slope and
  curvature loadings correlated -0.97 over a standard 3M-30Y grid;
  `simulate_rates()` warns when the loadings come out that close
  together. See
  [`dnsr::ns_lambda()`](https://rdrr.io/pkg/dnsr/man/ns_lambda.html) to
  express a decay rate by the maturity at which the curvature loading
  peaks.

- n_paths:

  Integer. Number of simulation paths

- n_periods:

  Integer. Number of time periods (typically months)

- maturities:

  Numeric vector. Maturities (in years) for which to calculate yields

- frequency:

  Character. Time frequency, one of "monthly" (default), "quarterly", or
  "annual"

- seed:

  Integer. Random seed for reproducibility

- engine:

  Character. Simulation engine passed to
  [`dnsr::simulate_dns_factors()`](https://rdrr.io/pkg/dnsr/man/simulate_dns_factors.html):
  `"yuima"` (the dnsr default, which needs the yuima package) or
  `"exact"`, which samples the exact transition law and needs no extra
  package. Defaults to dnsr's own choice.

- include_t0:

  Logical. `dnsr` returns the deterministic t = 0 curve followed by one
  column per simulated step, so the result has `n_periods + 1` columns
  and column 1 is identical on every path. `TRUE`, the default, keeps
  it: path-consistent discounting needs it, because the rate prevailing
  over month 1 is the one observed at time 0. `FALSE` drops it, giving
  exactly `n_periods` columns with column `t` being month `t`. Either
  way the columns are named by their period, so the convention is
  visible in the object.

## Value

A list containing:

- yield_paths:

  A list of matrices, one for each maturity, with dimensions
  `[path, time]`

- factors:

  The simulated DNS factors

- params:

  The DNS parameters used for simulation

- maturities:

  The maturities for which yields were calculated

## Examples

``` r
if (FALSE) { # \dontrun{
dns_params <- dnsr::get_calibrated_dns_params()
rate_paths <- simulate_rates(dns_params, n_paths = 100, n_periods = 360,
                             maturities = c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30))
} # }
```
