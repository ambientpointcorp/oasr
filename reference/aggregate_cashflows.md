# Aggregate Cash Flows by Deal Structure

Aggregates loan-level cash flows into bond-level cash flows based on a
deal structure with specified waterfall rules.

## Usage

``` r
aggregate_cashflows(
  cashflows,
  asset_data,
  deal_structure,
  verbose = FALSE,
  reference_rates = NULL
)
```

## Arguments

- cashflows:

  List. The result from
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)

- asset_data:

  Data frame. Asset data with loan characteristics

- deal_structure:

  List. Deal structure specification with waterfall rules

- verbose:

  Logical. Whether to print verbose output

- reference_rates:

  Matrix. Optional reference rate matrix with dimensions
  `[n_paths, n_periods]` for variable rate bonds, typically from
  [`extract_reference_rates`](https://ambientpointcorp.github.io/oasr/reference/extract_reference_rates.md).
  If NULL, fixed rates are used.

## Value

A list of bond-level cash flow arrays for each path

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple sequential pay structure with 2 bonds
deal_structure <- list(
  bonds = data.frame(
    bond_id = c("A", "B"),
    balance = c(800000, 200000),
    coupon = c(0.04, 0.06),
    is_floating = c(TRUE, FALSE),
    spread = c(0.015, 0),     # 150bps spread for floating rate bond
    reset_frequency = c(1, 0) # Monthly reset for floating rate bond
  ),
  waterfall = list(
    principal = "sequential",   # sequential or pro_rata
    interest = "sequential",    # sequential or pro_rata
    losses = "reverse_sequential"  # reverse_sequential or pro_rata
  )
)

# Use reference_rates extracted from simulated interest rates
dns_params <- dnsr::get_calibrated_dns_params()
rate_paths <- simulate_rates(dns_params, n_paths = 1, n_periods = 120)
reference_rates <- extract_reference_rates(rate_paths, "1")  # 1-year rate index

bond_cashflows <- aggregate_cashflows(cashflows, asset_data, deal_structure,
                                      reference_rates = reference_rates)
} # }
```
