# Cash Flow Component Positions

Returns the component index of each cash flow array produced by this
package, by name. The arrays are indexed positionally as
`[time, component, asset]`, and the position of each component is part
of the package's interface; this function is the one place it is written
down.

## Usage

``` r
cashflow_components(level = c("asset", "asset_io", "bond"))
```

## Arguments

- level:

  Character. Which layout to describe: `"asset"` for
  [`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  (nine components), `"asset_io"` for
  [`project_cashflows_with_io`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows_with_io.md)
  (ten, with the IO strip at position 9), or `"bond"` for
  [`aggregate_cashflows`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)
  (five).

## Value

A named integer vector of component positions.

## Examples

``` r
cashflow_components("asset")
#>             balance scheduled_principal          prepayment             default 
#>                   1                   2                   3                   4 
#>                loss            recovery            interest           servicing 
#>                   5                   6                   7                   8 
#>               total 
#>                   9 
cashflow_components("asset")[["total"]]
#> [1] 9
cashflow_components("bond")
#>   balance principal  interest      loss     total 
#>         1         2         3         4         5 
```
