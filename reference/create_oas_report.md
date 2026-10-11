# Create OAS Report

Creates a comprehensive report of OAS analysis results.

## Usage

``` r
create_oas_report(
  oas_result,
  output_format = "html",
  output_file = NULL,
  title = "OAS Analysis Report",
  author = ""
)
```

## Arguments

- oas_result:

  An object of class "oas_result"

- output_format:

  Character. Output format: "html", "pdf", or "word"

- output_file:

  Character. Output file name

- title:

  Character. Report title

- author:

  Character. Report author

## Value

The path to the generated report file

## Examples

``` r
if (FALSE) { # \dontrun{
oas_result <- calculate_oas(asset_data, rate_paths, prepay_model, market_price = 101.5)
report <- create_oas_report(oas_result, output_format = "html", output_file = "oas_report.html")
} # }
```
