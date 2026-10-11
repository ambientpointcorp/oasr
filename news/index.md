# Changelog

## oasr 0.1.0.9000 (development)

### Breaking changes

- `sample_abs_data` is now a genuine loan-level dataset: twelve
  fixed-rate mortgages with `original_balance`, `balance`, `wac`,
  `servicing`, `io_strip`, `accrual_method`, `term`, `age`, `ltv`,
  `fico` and three descriptive columns. Each `balance` is derived by
  amortising `original_balance` over `age` months, so the four fields
  are mutually consistent. The previous version carried `wam` and
  `market_price` but no `term`, which meant
  [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  and
  [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  could not run on it at all; the README and vignette examples both
  failed. Code that reconstructed the term as `wam + age` should now
  read `term` directly.
- `sample_deal_structure` is re-tranched onto the new pool. Its three
  bonds sum to the pool’s current balance, as before.
- [`summary.oas_result()`](https://ambientpointcorp.github.io/oasr/reference/summary.oas_result.md)
  returns `convergence$converged` as a logical rather than the string
  `"Yes"`/`"No"`. [`print()`](https://rdrr.io/r/base/print.html) still
  prints `Yes`/`No`.

### Conventions

These change results by default. Each is a modelling convention that
`oasr` previously fixed with no way to vary it; the earlier behaviour
remains reachable, and setting every option below to its legacy value
reproduces the previous release bit for bit.

- `accrual_method` is read per loan, from an `accrual_method` column,
  then from an `Accrual` column (the spelling the surrounding suite
  already carries), then from the new argument of the same name on
  [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md),
  [`project_cashflows_with_io()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows_with_io.md)
  and
  [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md).
  `"30/360"` is the default and is computed exactly as before, never
  from a calendar: for a monthly-pay amortising loan it is one twelfth
  by construction. `"ACT/365"` and `"ACT/360"` accrue on actual days and
  need a date anchor, supplied as `start_date` or a per-loan
  `accrual_start_date`; without one the engine warns once and falls back
  to 30/360. `ACT/365` redistributes the same annual interest across
  months, while `ACT/360` raises the annual total by 365/360.
- `terminal_balance` defaults to `"return"`: the balance outstanding
  when the projection stops is paid as a final cash flow. Dropping it
  priced a 300-month pool against a 24-month horizon at a fraction of
  its worth, with nothing to say so. `"drop"` restores the old
  behaviour.
- `on_short_horizon` defaults to `"warn"` when `n_periods` is shorter
  than the longest remaining term. `"silent"` and `"error"` are also
  accepted.
- `recovery_basis` defaults to `"defaults"`: recovery is taken on the
  amount that actually defaulted `recovery_lag` months earlier. The old
  `"balance"` basis took it on the whole balance standing at the time,
  which delivered about 0.55 of defaults where the model asked for 0.65.
- `discounting` on
  [`calculate_present_value()`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md),
  [`calculate_duration_convexity()`](https://ambientpointcorp.github.io/oasr/reference/calculate_duration_convexity.md)
  and
  [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  defaults to `"path"`, compounding the shortest simulated rate along
  each path. That is the Monte Carlo definition of an option-adjusted
  spread. `"spot"` is the earlier behaviour, where each cash flow was
  discounted at the reference yield standing in its own period, taken as
  a zero rate to that maturity.
- [`calculate_duration_convexity()`](https://ambientpointcorp.github.io/oasr/reference/calculate_duration_convexity.md)
  gains `reproject`. Given a function that projects cash flows under a
  shocked curve, it computes a true effective duration with prepayments
  responding to the shock. It stays opt-in because re-projecting roughly
  triples the cost, not because the default is better.
- [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  records the conventions it ran under in `result$options`, and
  [`run_oas_sensitivity()`](https://ambientpointcorp.github.io/oasr/reference/run_oas_sensitivity.md)
  and
  [`create_oas_surface()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_surface.md)
  re-solve on them. A sensitivity built from a spot-discounted result no
  longer silently reverts to the defaults.
- [`simulate_rates()`](https://ambientpointcorp.github.io/oasr/reference/simulate_rates.md)
  names the columns of its yield matrices by period, so the leading t =
  0 column is visible rather than implied, and gains `include_t0`. It
  defaults to `TRUE`, keeping that column: path discounting needs it,
  because the rate prevailing over month 1 is the one observed at
  time 0. `FALSE` returns exactly `n_periods` columns.
- The three plotting functions take a `seed`, so a sampled plot
  reproduces.

### New

- [`cashflow_components()`](https://ambientpointcorp.github.io/oasr/reference/cashflow_components.md)
  returns the component position of each cash flow array by name, for
  the asset, asset-with-IO and bond layouts. The positions themselves
  are unchanged; they are now written down in one place instead of
  appearing as bare numbers across four files and the test suite.

### Bug fixes

- The structured branch of
  [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  applied the bond weights and then divided by total bond balance a
  second time. On `sample_deal_structure` that put the price near 60
  when it belonged near 103, so the structured path could not converge
  on any price near par. It now solves: 98 bps against a target of

  101. 

- [`calculate_present_value()`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md)
  and
  [`calculate_average_life()`](https://ambientpointcorp.github.io/oasr/reference/calculate_average_life.md)
  consider every non-zero flow rather than only the positive ones. A
  negative total is a real outflow, and dropping it overstated the
  present value. For cash flows this package generates the two agree,
  since those totals are non-negative; the difference shows up on
  user-supplied cash flows carrying a writedown.

- [`aggregate_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/aggregate_cashflows.md)
  and
  [`plot_prepayment_rates()`](https://ambientpointcorp.github.io/oasr/reference/plot_prepayment_rates.md)
  no longer fail on one-asset pools, where the `[t, , ]` slice collapsed
  to a vector.

- [`create_oas_report()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_report.md)
  works. Two separate faults had to clear before it could: the data file
  path was recovered from the generated chunk text by a greedy regex
  that left a trailing quote on the filename, and the parameter table
  was built with [`sapply()`](https://rdrr.io/r/base/lapply.html), which
  returns a list when `seasonal_factors` is twelve long and every other
  element is one, after which
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) expanded that
  list into columns and demanded twelve rows against nine names. The
  second only shows once the first is fixed and pandoc is present, so it
  had never been reached.

- [`create_oas_report()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_report.md)
  can find its own data file. The path was recovered from the generated
  chunk text by a greedy regex that left a trailing quote on the
  filename, so rendering always failed.

- [`get_discount_factors()`](https://ambientpointcorp.github.io/oasr/reference/get_discount_factors.md)
  and
  [`binary_search_oas()`](https://ambientpointcorp.github.io/oasr/reference/binary_search_oas.md)
  clamp period lookups into range. A time under half a month rounded to
  index 0.

- [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  passes `reference_rates` down to
  [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md),
  so pools containing variable rate loans are reachable through it. A
  new `reference_key` argument selects the index; it defaults to the
  10-year.

- [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  derives `converged` from whether the price tolerance was met rather
  than from the iteration count. A solve that converged on its last
  allowed iteration previously reported failure and emitted a warning.

- [`plot_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/plot_cashflows.md)
  maps `total` to component 9. It previously mapped it to 8, which holds
  servicing.

- [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  and
  [`project_cashflows_with_io()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows_with_io.md)
  check their required columns up front and name the missing ones.

- [`plot_yield_surface()`](https://ambientpointcorp.github.io/oasr/reference/plot_yield_surface.md)
  no longer relies on an unimported `%>%`.

### Performance and structure

- [`calculate_oas()`](https://ambientpointcorp.github.io/oasr/reference/calculate_oas.md)
  computes the credit vectors, the cash flow projection and the weighted
  average life once, before the bisection loop, rather than on every
  trial spread; none of the three takes the OAS as an input. Duration
  and convexity likewise move after the search, since they are wanted
  for the answer rather than for the solve. Only the present value stays
  in the loop. A nine-iteration solve runs roughly four to seven times
  faster depending on load, and every returned value is bit-identical to
  before.
- [`project_cashflows()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows.md)
  and
  [`project_cashflows_with_io()`](https://ambientpointcorp.github.io/oasr/reference/project_cashflows_with_io.md)
  share one engine. They differed only by the IO strip, and the loop,
  the validation and the parallel plumbing had been copied between them;
  the file is about 210 lines shorter and the socket-cluster branch now
  exists once.
- [`calculate_present_value()`](https://ambientpointcorp.github.io/oasr/reference/calculate_present_value.md)
  and
  [`calculate_average_life()`](https://ambientpointcorp.github.io/oasr/reference/calculate_average_life.md)
  read the columns they need directly instead of materialising a whole
  period-by-component matrix per asset first. Present value is about
  twice as fast; the values are unchanged.
- Removed a second, verbatim copy of the internal
  `.compute_asset_vectors()`, which shadowed an identical definition 136
  lines above it, and folded the two identical copies of the
  prepayment-model scaler in
  [`run_oas_sensitivity()`](https://ambientpointcorp.github.io/oasr/reference/run_oas_sensitivity.md)
  and
  [`create_oas_surface()`](https://ambientpointcorp.github.io/oasr/reference/create_oas_surface.md)
  into one internal helper.
- Conditional prepayment rates are capped just below 1. A sensitivity
  multiplier of 2 on a fast base speed previously made the
  annual-to-monthly conversion undefined and produced `NaN`.

### Documentation and packaging

- The vignette and README no longer attach `dnsr`. It exports
  [`plot_yield_curves()`](https://ambientpointcorp.github.io/oasr/reference/plot_yield_curves.md)
  and
  [`plot_yield_surface()`](https://ambientpointcorp.github.io/oasr/reference/plot_yield_surface.md)
  of its own, so attaching it after `oasr` masked both, and a vignette
  chunk calling `oasr`’s signature against `dnsr`’s function failed with
  `unused arguments`. Every `dnsr` call in either document was already
  `dnsr::` qualified, so nothing needed it attached. The collision is
  documented in the README, and a test fails if a third shared name ever
  appears.

- `oasr-package.R` moved into `R/`, where roxygen reads it. Its import
  directives had never been applied.

- `rmarkdown` is no longer imported into the namespace while sitting in
  Suggests, which made `R CMD check` fail at the dependency gate.
  `plotly` and `lattice` are declared in Suggests.

- `LICENSE` holds the two-field stub R expects; the full MIT text is in
  `LICENSE.md`. `VignetteBuilder: knitr` is declared, and the vignette
  runs.

- [`calculate_duration_convexity()`](https://ambientpointcorp.github.io/oasr/reference/calculate_duration_convexity.md)
  documents its sign convention: it returns the signed logarithmic
  derivative `(1/P) dP/dy`, not the negated fixed-income reading.

- The `R-CMD-check` and `parallel-paths` workflows run on push and pull
  request again. Both had been manual-only: `dnsr` was private, the
  default token is scoped to this repository alone, and so an automatic
  run failed at the dependency gate before reaching a single check.
  `dnsr` is public now and the default token resolves it, so neither
  workflow carries the personal-token fallback any longer. The README
  drops the `GITHUB_PAT` step it asked of readers for the same reason.

- The package has a pkgdown site, at
  <https://ambientpointcorp.github.io/oasr>. There was no `_pkgdown.yml`
  and no workflow to render one, so the reference index fell back to
  whatever order pkgdown guessed and the vignette was reachable only by
  building it locally. The index now groups the 36 exported topics by
  what they do, and `URL:` and `BugReports:` are declared in
  `DESCRIPTION`, which had neither.
