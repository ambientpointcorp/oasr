# Run OAS Sensitivity Analysis

Runs sensitivity analysis for OAS by varying key parameters.

## Usage

``` r
run_oas_sensitivity(
  oas_result,
  sensitivity_type = "both",
  rate_shifts = c(-200, -100, -50, 0, 50, 100, 200),
  prepay_multipliers = c(0.5, 0.75, 1, 1.25, 1.5, 2),
  max_iterations = 20,
  verbose = TRUE
)
```

## Arguments

- oas_result:

  An object of class "oas_result"

- sensitivity_type:

  Character. Type of sensitivity to run: "rates", "prepayments", "both"

- rate_shifts:

  Numeric vector. Rate shifts in basis points to analyze

- prepay_multipliers:

  Numeric vector. Prepayment speed multipliers to analyze

- max_iterations:

  Integer. Maximum iterations for each OAS calculation

- verbose:

  Logical. Whether to print progress information

## Value

A list containing sensitivity analysis results

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate OAS
oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)

# Run sensitivity analysis
sensitivity <- run_oas_sensitivity(oas_result)

# Plot results
plot(sensitivity)
} # }
```
