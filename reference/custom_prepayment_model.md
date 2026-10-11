# Create Custom Prepayment Model

Creates a custom prepayment model based on a user-provided function that
calculates prepayment, default, and recovery rates.

## Usage

``` r
custom_prepayment_model(custom_fn, ...)
```

## Arguments

- custom_fn:

  Function. A function that takes asset, reference_rates, n_paths,
  n_periods and returns a list with prepay_rates, default_rates, and
  recovery_rates

- ...:

  Additional parameters to be stored in the model

## Value

A prepayment model list with type = "custom"

## Examples

``` r
if (FALSE) { # \dontrun{
# Example custom function
my_custom_fn <- function(asset, reference_rates, n_paths, n_periods) {
  # Create arrays for rates
  prepay_rates <- array(0, dim = c(n_paths, n_periods))
  default_rates <- array(0, dim = c(n_paths, n_periods))
  recovery_rates <- array(0, dim = c(n_paths, n_periods))

  # Fill arrays with custom logic
  # ...

  return(list(
    prepay_rates = prepay_rates,
    default_rates = default_rates,
    recovery_rates = recovery_rates
  ))
}

model <- custom_prepayment_model(my_custom_fn)
} # }
```
