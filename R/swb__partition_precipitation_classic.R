#' Partition precipitation into rain fraction (SWB classic)
#'
#' @param tmin Minimum air temperature, in degrees Fahrenheit.
#' @param tmax Maximum air temperature, in degrees Fahrenheit.
#' @param tmean Mean air temperature, in degrees Fahrenheit.
#'
#' @return Fraction of precipitation that falls as rain (unitless).
#' @export
#'
#' @examples
calc_fraction_rain_classic <- function(tmin, tmax, tmean) {

  # freezing point of water, degrees F
  FREEZING_PT_F <- 32.

  FREEZING <- ifelse (tmean - (tmax-tmin) / 3.0 <= FREEZING_PT_F, TRUE, FALSE)

  fraction_rainfall <- ifelse (FREEZING, 0.0, 1.0)

  return(fraction_rainfall)

}
