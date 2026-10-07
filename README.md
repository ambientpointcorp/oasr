# oasr: Option-Adjusted Spread Analysis in R

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

Neither `oasr` nor its `dnsr` dependency is on CRAN, and `oasr` does not
declare a `Remotes:` field, so install `dnsr` first — otherwise the `oasr`
install fails looking for `dnsr` on CRAN:

```r
# install.packages("remotes")
remotes::install_github("ambientpointcorp/dnsr")
remotes::install_github("ambientpointcorp/oasr")
```

## Usage

### Basic Example

```r
library(oasr)
library(dnsr)

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
library(dnsr)

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
library(dnsr)

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
| Valuation | `calculate_present_value()`, `calculate_price()`, `calculate_average_life()` |
| Spreads | `calculate_oas()`, `binary_search_oas()`, `calculate_z_spread()`, `calculate_option_cost()` |
| Risk | `calculate_duration_convexity()`, `run_oas_sensitivity()`, `create_oas_surface()` |
| Output | `create_oas_report()`, `plot_cashflows()`, `plot_prepayment_rates()`, `plot_rate_paths()`, `plot_yield_curves()`, `plot_yield_surface()` |

`oas_result`, `oas_sensitivity`, and `oas_surface` objects carry `print()`,
`plot()`, and (for `oas_result`) `summary()` methods.

## Citation

If you use `oasr` in your research, please cite:

```
@Manual{,
  title = {oasr: Option-Adjusted Spread Analysis for Fixed Income Securities},
  author = {Ariel R. H.},
  year = {2025},
  note = {R package version 0.1.0},
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

MIT © Ambient Point Corp. See `LICENSE` for the full text.
