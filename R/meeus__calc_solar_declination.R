#' Calculate solar declination.
#'
#' @param fractional_year_radians Day of year, expressed as fraction of year in radians.
#'
#' @return Solar declination, in radians.
#' @export
#'
#' @references
#' Meeus, J.H., 1991, Astronomical algorithms: Willmann-Bell, Incorporated, 477 p.
calc_solar_declination <- function(fractional_year_radians) {

  gam <- fractional_year_radians

  delta <- 6.918E-3 - 3.99912E-1 * cos(gam) + 7.0257E-2 * sin(gam) - 6.758E-3 * cos(2*gam)
  + 9.07E-4 * sin(2*gam) - 2.697E-3 * cos(3*gam) + 1.48E-3 * sin(3*gam)

  return(delta)

}
