# Create OAS Surface Analysis

Creates a surface analysis of OAS across two dimensions: interest rate
shifts and prepayment multipliers.

## Usage

``` r
create_oas_surface(
  oas_result,
  rate_shifts = seq(-200, 200, by = 100),
  prepay_multipliers = seq(0.5, 2, by = 0.5),
  max_iterations = 20,
  verbose = TRUE
)
```

## Arguments

- oas_result:

  An object of class "oas_result"

- rate_shifts:

  Numeric vector. Rate shifts in basis points to analyze

- prepay_multipliers:

  Numeric vector. Prepayment speed multipliers to analyze

- max_iterations:

  Integer. Maximum iterations for each OAS calculation

- verbose:

  Logical. Whether to print progress information

## Value

An object of class "oas_surface"
