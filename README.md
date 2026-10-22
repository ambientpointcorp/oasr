# oasr: Option-Adjusted Spread Analysis in R

<!-- badges: start -->
[![R-CMD-check](https://github.com/ambientpointcorp/oasr/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/ambientpointcorp/oasr/actions/workflows/R-CMD-check.yaml)
[![parallel-paths](https://github.com/ambientpointcorp/oasr/actions/workflows/parallel-paths.yaml/badge.svg?branch=main)](https://github.com/ambientpointcorp/oasr/actions/workflows/parallel-paths.yaml)
[![pkgdown](https://github.com/ambientpointcorp/oasr/actions/workflows/pkgdown.yaml/badge.svg?branch=main)](https://github.com/ambientpointcorp/oasr/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
<!-- badges: end -->

Reference documentation: <https://ambientpointcorp.github.io/oasr>

The second badge is the Windows socket-cluster check: the parallel branches in
`calculate_credit_vectors()` and `project_cashflows()` are only reached off
Unix, so no run on a developer machine touches them. It asserts they agree
with the sequential path.

## Overview

`oasr` is a comprehensive R package for performing option-adjusted spread (OAS) analysis on fixed income securities, particularly focusing on asset-backed securities (ABS) and mortgage-backed securities (MBS). The package leverages the `dnsr` package for interest rate modeling and yield curve simulation.

## Features

- **Flexible OAS Analysis**: Calculate option-adjusted spreads for fixed income securities with embedded options
- **Multiple Prepayment Models**: Support for CPR, CDR, and time-to-event survival models
- **Default Modeling**: Incorporate credit risk with default rates and loss recovery assumptions
- **Cash Flow Projection**: Project cash flows under various interest rate scenarios
- **Sensitivity Analysis**: Analyze OAS sensitivity to changes in market factors
- **Visualization**: Specialized plotting functions for OAS analysis results

## Installation

Neither `oasr` nor its `dnsr` dependency is on CRAN. `oasr` declares `dnsr`
in `Remotes:`, so a single install resolves both:

```r
# install.packages("remotes")
remotes::install_github("ambientpointcorp/oasr")
```

## Usage

### Basic Example

```r
library(oasr)

# Load sample data
data(sample_abs_data)

# Calibrate DNS model from market data
dns_params <- dnsr::get_calibrated_dns_params()

# Simulate interest rate paths
rate_paths <- simulate_rates(dns_params, n_paths = 100, n_periods = 360)

# Define prepayment assumptions
prepay_model <- prepayment_model(
  type = "cpr_cdr",
  base_cpr = 0.06,
  base_cdr = 0.01,
  recovery_rate = 0.65
)

# Calculate OAS
oas_result <- calculate_oas(
  asset_data = sample_abs_data,
  rate_paths = rate_paths,
  prepay_model = prepay_model,
  market_price = 101.5
)

# Analyze results
summary(oas_result)

# Create plots
plot(oas_result, type = "cashflows")
plot(oas_result, type = "prepayments")
plot(oas_result, type = "rates")

# Run sensitivity analysis
sensitivity <- run_oas_sensitivity(oas_result)
plot(sensitivity)
```

### Structured Securities Example

```r
library(oasr)

# Load sample data
data(sample_abs_data)
data(sample_deal_structure)

# Simulate interest rate paths
dns_params <- dnsr::get_calibrated_dns_params()
rate_paths <- simulate_rates(dns_params, n_paths = 100)

# Define prepayment model
prepay_model <- prepayment_model(
  type = "cpr_cdr",
  base_cpr = 0.06,
  base_cdr = 0.01,
  recovery_rate = 0.65
)

# Calculate OAS for structured security
oas_result <- calculate_oas(
  asset_data = sample_abs_data,
  rate_paths = rate_paths,
  prepay_model = prepay_model,
  market_price = 101.0,
  deal_structure = sample_deal_structure
)

# Generate report
report_file <- create_oas_report(oas_result, output_format = "html")
```

## Prepayment Model Types

The package supports multiple prepayment model types:

1. **cpr_cdr**: Standard CPR/CDR model with refinancing incentives
2. **psa**: Public Securities Association standard prepayment model
3. **survival**: Time-to-event survival models for prepayment and default
4. **custom**: User-defined custom prepayment functions

## Integration with dnsr

The package relies on the `dnsr` package for interest rate modeling:

```r
library(oasr)

# Get Treasury yield data
yield_data <- dnsr::get_treasury_yield_combined("2020-01-01")

# Fit DNS model
dns_result <- dnsr::fit_dns_model(yield_data, n_days = 100)

# Estimate DNS dynamics
dynamics <- dnsr::estimate_dns_dynamics(dns_result$parameters)

# Convert to simulation parameters
sim_params <- dnsr::dns_dynamics_to_sim_params(dynamics)

# Simulate rate paths using oasr interface
rate_paths <- simulate_rates(sim_params, n_paths = 100)
```

## Main Functions

| Area | Functions |
| :--- | :--- |
| Rate paths | `simulate_rates()`, `extract_reference_rates()`, `get_discount_factors()` |
| Prepayment and credit | `prepayment_model()`, `psa_prepayment_model()`, `survival_prepayment_model()`, `custom_prepayment_model()`, `calculate_credit_vectors()` |
| Cashflows | `project_cashflows()`, `project_cashflows_with_io()`, `aggregate_cashflows()` |
| Valuation | `calculate_present_value()`, `calculate_price()`, `calculate_average_life()`, `cashflow_components()` |
| Spreads | `calculate_oas()`, `binary_search_oas()`, `calculate_z_spread()`, `calculate_option_cost()` |
| Risk | `calculate_duration_convexity()`, `run_oas_sensitivity()`, `create_oas_surface()` |
| Output | `create_oas_report()`, `plot_cashflows()`, `plot_prepayment_rates()`, `plot_rate_paths()`, `plot_yield_curves()`, `plot_yield_surface()` |

`oas_result`, `oas_sensitivity`, and `oas_surface` objects carry `print()`,
`plot()`, and (for `oas_result`) `summary()` methods.

### A note on attaching `dnsr`

`dnsr` exports `plot_yield_curves()` and `plot_yield_surface()` of its own,
and so does `oasr`. Attaching both with `library()` means whichever is
attached second wins, silently, and the two take different arguments:

```r
library(oasr)
library(dnsr)
plot_yield_curves(rate_paths, time_points = c(0, 1, 5))
#> Error: unused arguments (time_points = c(0, 1, 5))
```

Reach for `dnsr` through `dnsr::` rather than attaching it, or qualify the
plotting calls as `oasr::plot_yield_curves()`. Everything else in the two
packages has distinct names.

## Conventions

Several modelling conventions are settable rather than fixed. Each defaults to
the behaviour that is correct rather than the behaviour that is oldest, and the
earlier behaviour stays reachable:

| Argument | Default | Alternative |
| :--- | :--- | :--- |
| `accrual_method` | `"30/360"` | `"ACT/365"`, `"ACT/360"`, with a `start_date` or per-loan `accrual_start_date` anchor. A per-loan `accrual_method` or `Accrual` column takes precedence over the argument |
| `terminal_balance` | `"return"` | `"drop"`, discarding the balance outstanding at the horizon |
| `on_short_horizon` | `"warn"` | `"silent"`, `"error"` |
| `recovery_basis` | `"defaults"` | `"balance"`, taking recovery on the whole balance rather than the amount that defaulted |
| `discounting` | `"path"` | `"spot"`, discounting each cash flow at the reference yield in its own period |
| `reproject` | `NULL` | a projection function, making `calculate_duration_convexity()` a true effective duration |
| `include_t0` | `TRUE` | `FALSE`, returning exactly `n_periods` columns from `simulate_rates()` |

`calculate_oas()` records the conventions it ran under in `result$options`, and
`run_oas_sensitivity()` and `create_oas_surface()` re-solve on them.

Note that `duration` is the signed logarithmic derivative `(1/P) dP/dy`, not
the negated fixed-income reading, so it is negative for an instrument whose
price falls as yields rise. The signed form stays correct for instruments whose
price rises with yield; negate it for the conventional presentation.

## Citation

If you use `oasr` in your research, please cite:

```
@Manual{,
  title = {oasr: Option-Adjusted Spread Analysis for Fixed Income Securities},
  author = {Ariel R. H.},
  year = {2025},
  note = {R package version 0.1.0.9000},
  url = {https://github.com/ambientpointcorp/oasr},
}
```

## Related

- [`dnsr`](https://github.com/ambientpointcorp/dnsr) — the Dynamic
  Nelson-Siegel term structure model this package builds its rate paths on.
- *Monte Carlo Fair Value Analysis for Loan Pools* — develops the cashflow
  projection, Monte Carlo valuation, and OAS methodology this package
  implements. Its worked examples run on `oasr`; the chapter code is at
  [mc-fair-value-book](https://github.com/ambientpointcorp/mc-fair-value-book).

## License

MIT © Ambient Point Corp. `LICENSE` holds the two-field stub R expects; the
full text is in `LICENSE.md`.
