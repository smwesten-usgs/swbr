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

  if (hawkins)  smax <- 1.33 * smax^1.15      # Equation 8, Hawkins and others, 2002

  # now consider runoff if Ia ~ 0.05S
  runoff <- ifelse ( P > 0.05 * smax,
                   ( P - 0.05 * smax )^2  / (P + 0.95 * smax),
                     0.
  )

}
