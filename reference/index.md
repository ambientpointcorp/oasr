# Package index

## OAS analysis

Solve for the spread that reprices a security, and read the quantities
derived from it.

- [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  : Calculate Option-Adjusted Spread (OAS)
- [`binary_search_oas()`](https://ambientpointcorp.github.io/oasr/reference/binary_search_oas.md)
  : Binary Search OAS with Fixed Cash Flows
- [`calculate_z_spread()`](https://ambientpointcorp.github.io/oasr/reference/calculate_z_spread.md)
  : Calculate Z-Spread
- [`calculate_option_cost()`](https://ambientpointcorp.github.io/oasr/reference/calculate_option_cost.md)
  : Calculate Option Cost
- [`run_oas_sensitivity()`](https://ambientpointcorp.github.io/oasr/reference/run_oas_sensitivity.md)
  : Run OAS Sensitivity Analysis
- [`create_oas_surface()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_surface.md)
  : Create OAS Surface Analysis

## Prepayment and credit

Specify how a pool pays down early and how it defaults.

- [`prepayment_model()`](https://ambientpointcorp.github.io/oasr/reference/prepayment_model.md)
  : Create Prepayment Model for OAS Analysis
- [`psa_prepayment_model()`](https://ambientpointcorp.github.io/oasr/reference/psa_prepayment_model.md)
  : Create PSA Prepayment Model
- [`survival_prepayment_model()`](https://ambientpointcorp.github.io/oasr/reference/survival_prepayment_model.md)
  : Create Survival-Based Prepayment Model
- [`custom_prepayment_model()`](https://ambientpointcorp.github.io/oasr/reference/custom_prepayment_model.md)
  : Create Custom Prepayment Model
- [`calculate_credit_vectors()`](https://ambientpointcorp.github.io/oasr/reference/calculate_credit_vectors.md)
  : Calculate Prepayment and Default Rates (Vectorized & Parallelized)

## Rate paths

Draw the scenarios a security is priced across, and discount along them.

- [`simulate_rates()`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)
  : Simulate Interest Rate Paths for OAS Analysis
- [`extract_reference_rates()`](https://ambientpointcorp.github.io/oasr/reference/extract_reference_rates.md)
  : Extract Reference Rates from Simulated Rate Paths
- [`get_discount_factors()`](https://ambientpointcorp.github.io/oasr/reference/get_discount_factors.md)
  : Get Discount Factors from Rate Paths

## Cash flow projection

Project a pool’s cash flows under given rate and prepayment assumptions.

- [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  : Project Cash Flows for ABS/MBS Securities
- [`project_cashflows_with_io()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows_with_io.md)
  : Project Cash Flows for ABS/MBS Securities with IO Strip
- [`cashflow_components()`](https://ambientpointcorp.github.io/oasr/reference/cashflow_components.md)
  : Cash Flow Component Positions
- [`aggregate_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)
  : Aggregate Cash Flows by Deal Structure

## Valuation and risk

- [`calculate_present_value()`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md)
  : Calculate Present Value of Cash Flows
- [`calculate_price()`](https://ambientpointcorp.github.io/oasr/reference/calculate_price.md)
  : Calculate Price from Cash Flows
- [`calculate_average_life()`](https://ambientpointcorp.github.io/oasr/reference/calculate_average_life.md)
  : Calculate Weighted Average Life (WAL)
- [`calculate_duration_convexity()`](https://ambientpointcorp.github.io/oasr/reference/calculate_duration_convexity.md)
  : Calculate Effective Duration and Convexity

## Reporting

- [`create_oas_report()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_report.md)
  : Create OAS Report

## Data

- [`sample_abs_data`](https://ambientpointcorp.github.io/oasr/reference/sample_abs_data.md)
  : Sample Loan-Level Data for OAS Analysis
- [`sample_deal_structure`](https://ambientpointcorp.github.io/oasr/reference/sample_deal_structure.md)
  : Sample Structured Deal for OAS Analysis

## Visualization

- [`plot_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/plot_cashflows.md)
  : Plot Cash Flows
- [`plot_prepayment_rates()`](https://ambientpointcorp.github.io/oasr/reference/plot_prepayment_rates.md)
  : Plot Prepayment Rates
- [`plot_rate_paths()`](https://ambientpointcorp.github.io/oasr/reference/plot_rate_paths.md)
  : Plot Rate Paths
- [`plot_yield_curves()`](https://ambientpointcorp.github.io/oasr/reference/plot_yield_curves.md)
  : Plot Yield Curves
- [`plot_yield_surface()`](https://ambientpointcorp.github.io/oasr/reference/plot_yield_surface.md)
  : Plot Yield Surface

## Methods

[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) for the objects
the analysis functions return.

- [`print(`*`<oas_result>`*`)`](https://ambientpointcorp.github.io/oasr/reference/print.oas_result.md)
  : Print Method for OAS Result
- [`summary(`*`<oas_result>`*`)`](https://ambientpointcorp.github.io/oasr/reference/summary.oas_result.md)
  : Summary Method for OAS Result
- [`plot(`*`<oas_result>`*`)`](https://ambientpointcorp.github.io/oasr/reference/plot.oas_result.md)
  : Plot Method for OAS Result
- [`print(`*`<oas_sensitivity>`*`)`](https://ambientpointcorp.github.io/oasr/reference/print.oas_sensitivity.md)
  : Print Method for OAS Sensitivity Analysis
- [`plot(`*`<oas_sensitivity>`*`)`](https://ambientpointcorp.github.io/oasr/reference/plot.oas_sensitivity.md)
  : Plot Method for OAS Sensitivity Analysis
- [`plot(`*`<oas_surface>`*`)`](https://ambientpointcorp.github.io/oasr/reference/plot.oas_surface.md)
  : Plot Method for OAS Surface Analysis
