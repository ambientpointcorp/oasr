# Plot Prepayment Rates

Creates plots of prepayment rates from OAS analysis.

## Usage

``` r
plot_prepayment_rates(
  oas_result,
  asset_idx = NULL,
  type = "both",
  sample_paths = 5,
  seed = NULL
)
```

## Arguments

- oas_result:

  An object of class "oas_result"

- asset_idx:

  Integer or NULL. Specific asset index to plot, NULL for average

- type:

  Character. Type of prepayment to plot: "prepay", "default", "both"

- sample_paths:

  Integer. Number of paths to sample

- seed:

  Integer. Optional seed for the path sample, so the plot reproduces

## Value

A ggplot object or list of ggplot objects
