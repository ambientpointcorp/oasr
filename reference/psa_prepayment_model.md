# Create PSA Prepayment Model

Creates a prepayment model based on the Public Securities Association
(PSA) standard prepayment model. 100% PSA assumes 0.2% CPR in month 1,
increasing by 0.2% per month until month 30, then 6% afterwards.

## Usage

``` r
psa_prepayment_model(
  psa_multiplier = 1,
  base_cdr = 0.01,
  recovery_rate = 0.65,
  recovery_lag = 12
)
```

## Arguments

- psa_multiplier:

  Numeric. Multiplier for the standard PSA curve

- base_cdr:

  Numeric. Base Conditional Default Rate (CDR), annual rate in decimal
  form

- recovery_rate:

  Numeric. Loss recovery rate on defaulted principal (0-1)

- recovery_lag:

  Integer. Number of periods (months) until recovery is realized

## Value

A prepayment model list with type = "psa"

## Examples

``` r
model <- psa_prepayment_model(
  psa_multiplier = 1.5,
  base_cdr = 0.01,
  recovery_rate = 0.65
)
```
