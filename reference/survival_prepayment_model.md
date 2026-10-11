# Create Survival-Based Prepayment Model

Creates a prepayment model based on survival analysis (time-to-event)
with specified hazard rates for prepayment and default.

## Usage

``` r
survival_prepayment_model(
  prepay_hazard = 0.005,
  default_hazard = 0.001,
  recovery_rate = 0.65,
  recovery_lag = 12,
  seasoning = 0
)
```

## Arguments

- prepay_hazard:

  Numeric. Monthly hazard rate for prepayment

- default_hazard:

  Numeric. Monthly hazard rate for default

- recovery_rate:

  Numeric. Loss recovery rate on defaulted principal (0-1)

- recovery_lag:

  Integer. Number of periods (months) until recovery is realized

- seasoning:

  Numeric. Seasoning factor for the hazard function

## Value

A prepayment model list with type = "survival"

## Examples

``` r
model <- survival_prepayment_model(
  prepay_hazard = 0.005,
  default_hazard = 0.001,
  recovery_rate = 0.65
)
```
