# Sample Structured Deal for OAS Analysis

A three-tranche sequential-pay deal sized to the pool in
[`sample_abs_data`](https://ambientpointcorp.github.io/oasr/reference/sample_abs_data.md):
the bond balances sum to the pool's current balance, so the structure
can be passed to
[`aggregate_cashflows`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)
or to
[`calculate_oas`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
as `deal_structure` without rescaling.

## Usage

``` r
sample_deal_structure
```

## Format

A list with two components:

- bonds:

  A data frame of bond_id, balance and coupon, senior first

- waterfall:

  A list of the principal, interest and loss rules

## Examples

``` r
data(sample_deal_structure)
str(sample_deal_structure)
#> List of 2
#>  $ bonds    :'data.frame':   3 obs. of  3 variables:
#>   ..$ bond_id: chr [1:3] "A" "B" "C"
#>   ..$ balance: num [1:3] 2590851 504711 269179
#>   ..$ coupon : num [1:3] 0.045 0.065 0.085
#>  $ waterfall:List of 3
#>   ..$ principal: chr "sequential"
#>   ..$ interest : chr "sequential"
#>   ..$ losses   : chr "reverse_sequential"

# the tranches are sized to the pool
data(sample_abs_data)
sum(sample_deal_structure$bonds$balance) == round(sum(sample_abs_data$balance), 2)
#> [1] TRUE
```
