# oasr: Option-Adjusted Spread Analysis for Fixed Income Securities

A comprehensive toolkit for option-adjusted spread (OAS) analysis of
fixed income securities with embedded options. Provides functions for
modeling prepayments, defaults, and interest rates to calculate accurate
option-adjusted spreads. Supports various prepayment and default models,
including CPR (Conditional Prepayment Rate), CDR (Conditional Default
Rate), and time-to-event survival models. Leverages the 'dnsr' package
for interest rate modeling and yield curve simulation.

## Details

The main entry points are:

- [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md):
  interface with dnsr for interest rate simulation

- [`prepayment_model`](https://ambientpointcorp.github.io/oasr/reference/prepayment_model.md):
  create prepayment model specifications

- [`calculate_credit_vectors`](https://ambientpointcorp.github.io/oasr/reference/calculate_credit_vectors.md):
  project prepayment, default and recovery rates

- [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md):
  project cash flows under each rate path

- [`calculate_oas`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md):
  solve for the option-adjusted spread

- [`run_oas_sensitivity`](https://ambientpointcorp.github.io/oasr/reference/run_oas_sensitivity.md):
  perform sensitivity analysis

## Author

**Maintainer**: Ariel R. H. <ariel@ambientpointcorp.com>
