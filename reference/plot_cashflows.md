# Plot Cash Flows

Creates plots of cash flows from OAS analysis.

## Usage

``` r
plot_cashflows(
  oas_result,
  asset_idx = NULL,
  components = c("scheduled_principal", "prepayment", "interest", "recovery"),
  average_paths = TRUE,
  seed = NULL
)
```

## Arguments

- oas_result:

  An object of class "oas_result"

- asset_idx:

  Integer or NULL. Specific asset index to plot, NULL for total

- components:

  Character vector. Which cash flow components to include

- average_paths:

  Logical. Whether to average across paths

- seed:

  Integer. Optional seed for the path sample, so the plot reproduces

## Value

A ggplot object
