# Sample Loan-Level Data for OAS Analysis

A loan-level dataset of twelve fixed-rate mortgages, for demonstration
and testing. Every column is an attribute of an individual loan, so the
frame can be passed straight to
[`project_cashflows`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
and
[`calculate_oas`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
with no preparation.

## Usage

``` r
sample_abs_data
```

## Format

A data frame with 12 rows and 14 variables:

- asset_id:

  Loan identifier

- original_balance:

  Balance at origination (dollars)

- balance:

  Current balance (dollars), amortised from origination

- wac:

  Note rate (annual, decimal)

- servicing:

  Servicing fee (annual, decimal)

- io_strip:

  Interest-only strip (annual, decimal)

- accrual_method:

  Day-count convention for interest accrual. All twelve loans are
  `"30/360"`, the convention the projection engine applies today

- term:

  Original amortisation term (months)

- age:

  Months elapsed since origination

- ltv:

  Loan-to-value ratio (decimal)

- fico:

  Borrower FICO score

- state:

  State of the property

- property_type:

  SFR, CONDO or MULTI

- occupancy_type:

  PRIMARY, INVESTMENT or SECOND_HOME

## Details

`balance` is derived by amortising `original_balance` over `age` months
at `wac`, so `original_balance`, `balance`, `term` and `age` are
mutually consistent. The loans span the kinks in the default model –
four have `ltv` above 0.80 and one has `fico` below 620 – and every
remaining term (`term - age`) is at most 354 months, so a 360-month
projection captures the pool in full.

## Examples

``` r
data(sample_abs_data)
head(sample_abs_data)
#>   asset_id original_balance  balance    wac servicing io_strip accrual_method
#> 1     L001           350000 341533.6 0.0625    0.0025   0.0010         30/360
#> 2     L002           198000 186393.9 0.0550    0.0025   0.0010         30/360
#> 3     L003           425000 420781.7 0.0712    0.0030   0.0015         30/360
#> 4     L004           150000 128072.5 0.0488    0.0025   0.0010         30/360
#> 5     L005           275000 258158.2 0.0675    0.0030   0.0015         30/360
#> 6     L006           520000 438256.4 0.0425    0.0025   0.0010         30/360
#>   term age  ltv fico state property_type occupancy_type
#> 1  360  24 0.80  742    CA           SFR        PRIMARY
#> 2  360  48 0.72  768    NY           SFR        PRIMARY
#> 3  360  12 0.90  668    FL         CONDO     INVESTMENT
#> 4  180  36 0.65  790    TX           SFR        PRIMARY
#> 5  360  60 0.85  705    IL         MULTI     INVESTMENT
#> 6  360  96 0.58  781    WA           SFR        PRIMARY

# balance is the amortised balance, not a free parameter
with(sample_abs_data, all(balance <= original_balance))
#> [1] TRUE
```
