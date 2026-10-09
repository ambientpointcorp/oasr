#' @title Cash Flow Component Positions
#' @description Returns the component index of each cash flow array produced by
#' this package, by name. The arrays are indexed positionally as
#' \code{[time, component, asset]}, and the position of each component is part
#' of the package's interface; this function is the one place it is written
#' down.
#'
#' @param level Character. Which layout to describe: \code{"asset"} for
#'        \code{\link{project_cashflows}} (nine components), \code{"asset_io"}
#'        for \code{\link{project_cashflows_with_io}} (ten, with the IO strip
#'        at position 9), or \code{"bond"} for
#'        \code{\link{aggregate_cashflows}} (five).
#'
#' @return A named integer vector of component positions.
#'
#' @examples
#' cashflow_components("asset")
#' cashflow_components("asset")[["total"]]
#' cashflow_components("bond")
#'
#' @export
cashflow_components <- function(level = c("asset", "asset_io", "bond")) {
  level <- match.arg(level)

  switch(
    level,
    asset = c(
      balance = 1L, scheduled_principal = 2L, prepayment = 3L, default = 4L,
      loss = 5L, recovery = 6L, interest = 7L, servicing = 8L, total = 9L
    ),
    asset_io = c(
      balance = 1L, scheduled_principal = 2L, prepayment = 3L, default = 4L,
      loss = 5L, recovery = 6L, interest = 7L, servicing = 8L, io_strip = 9L,
      total = 10L
    ),
    bond = c(
      balance = 1L, principal = 2L, interest = 3L, loss = 4L, total = 5L
    )
  )
}
