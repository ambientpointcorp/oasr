# Calculate Option-Adjusted Spread (OAS)

Calculates the option-adjusted spread for a security with embedded
options.

## Usage

``` r
calculate_oas(
  asset_data,
  rate_paths,
  prepay_model,
  market_price,
  deal_structure = NULL,
  search_range = c(-500, 500),
  tolerance = 0.01,
  max_iterations = 50,
  verbose = TRUE,
  reference_key = "10",
  discounting = c("path", "spot"),
  accrual_method = "30/360",
  start_date = NULL,
  terminal_balance = c("return", "drop"),
  recovery_basis = c("defaults", "balance")
)
```

## Arguments

- asset_data:

  Data frame. Asset data with loan characteristics

- rate_paths:

  List. The result from
  [`simulate_rates`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)

- prepay_model:

  List. Prepayment model from
  [`prepayment_model`](https://ambientpointcorp.github.io/oasr/reference/prepayment_model.md)

- market_price:

  Numeric. Market price of the security (as a percentage of par)

- deal_structure:

  List. Optional deal structure for structured securities

- search_range:

  Numeric vector. Range of OAS values to search (in basis points)

- tolerance:

  Numeric. Tolerance for price convergence

- max_iterations:

  Integer. Maximum number of iterations for the search

- verbose:

  Logical. Whether to print progress information

- reference_key:

  Character. Key of the reference rate used for credit vectors,
  discounting, and as the index for any variable rate loans or floating
  rate bonds. Defaults to the 10-year.

- discounting:

  Character. Passed to
  [`calculate_present_value`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md):
  `"path"` compounds the short rate along each path, `"spot"` reproduces
  the earlier behaviour.

- accrual_method:

  Character. Day-count convention passed to
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  when the asset data carries no `accrual_method` or `Accrual` column of
  its own.

- start_date:

  Date. Anchor for the payment schedule, needed by the ACT conventions.
  Passed to
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md).

- terminal_balance:

  Character. Passed to
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md):
  whether the balance outstanding at the horizon is returned as a final
  cash flow or dropped.

- recovery_basis:

  Character. Passed to
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md):
  whether recovery is taken on the amount that defaulted or on the
  balance standing at the time.

## Value

A list containing the OAS calculation results

## Examples

``` r
if (FALSE) { # \dontrun{
# Get DNS parameters and simulate rate paths
dns_params <- dnsr::get_calibrated_dns_params()
rate_paths <- simulate_rates(dns_params, n_paths = 100)

# Define prepayment model
prepay_model <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06, base_cdr = 0.01)

# Calculate OAS
oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)
} # }
```
