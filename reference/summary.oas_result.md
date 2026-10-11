# Summary Method for OAS Result

Provides a summary of the OAS calculation result.

## Usage

``` r
# S3 method for class 'oas_result'
summary(object, ...)
```

## Arguments

- object:

  An object of class "oas_result"

- ...:

  Additional arguments (not used)

## Value

A list with a summary of the OAS calculation. The `convergence` element
holds `converged`, a logical, and `iterations`, an integer.
