#' Calculate curve number runoff
#'
#' @param curve_number Runoff curve number (0-100).
#' @param P Precipitation (inches).
#'
#' @return Runoff, in inches.
#' @export
#'
#' @examples
calc_runoff_cn <- function(curve_number, P, hawkins=TRUE) {

  # calculate Smax assuming Ia = 0.2
  smax <- 1000 / curve_number - 10

  if (hawkins) {
    # Equation 8, Hawkins and others, 2002
    smax_adj <- 1.33 * smax^1.15
  }

  # now consider runoff if Ia ~ 0.05S
  if ( P > 0.05 * smax_adj ) {
    runoff <- ( P - 0.05 * smax_adj )^2  / (P + 0.95 * smax_adj)
  } else {
    runoff <- 0.
  }

}
