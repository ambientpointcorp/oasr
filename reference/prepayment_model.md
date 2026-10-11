# Create Prepayment Model for OAS Analysis

Creates a prepayment model specification for use in cash flow
projection. Supports multiple model types including CPR/CDR-based models
and time-to-event survival models.

## Usage

``` r
prepayment_model(
  type = "cpr_cdr",
  base_cpr = 0.06,
  base_cdr = 0.01,
  recovery_rate = 0.65,
  recovery_lag = 12,
  seasoning = 0,
  custom_fn = NULL,
  burnout_factor = 0,
  rate_sensitivity = 0,
  seasonal_factors = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  ...
)
```

## Arguments

- type:

  Character. Type of prepayment model: "cpr_cdr", "psa", "survival", or
  "custom"

- base_cpr:

  Numeric. Base Conditional Prepayment Rate (CPR), annual rate in
  decimal form

- base_cdr:

  Numeric. Base Conditional Default Rate (CDR), annual rate in decimal
  form

- recovery_rate:

  Numeric. Loss recovery rate on defaulted principal (0-1)

- recovery_lag:

  Integer. Number of periods (months) until recovery is realized

- seasoning:

  Numeric. Seasoning multiplier for prepayment ramp

- custom_fn:

  Function. For "custom" type, a function that calculates prepayment and
  default rates

- burnout_factor:

  Numeric. Factor to account for burnout effect (higher values = more
  burnout)

- rate_sensitivity:

  Numeric. Sensitivity of prepayments to rate changes (refinancing
  incentive)

- seasonal_factors:

  Numeric vector. Monthly seasonal factors (length 12)

- ...:

  Additional parameters for specific model types

## Value

A list with the prepayment model specification

## Examples

``` r
# Basic CPR/CDR model
model1 <- prepayment_model(type = "cpr_cdr", base_cpr = 0.06,
                           base_cdr = 0.01, recovery_rate = 0.65)

# PSA model
model2 <- prepayment_model(type = "psa", psa_multiplier = 1.0,
                           base_cdr = 0.01, recovery_rate = 0.65)
```
