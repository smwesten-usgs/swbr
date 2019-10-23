#' Calculate Thornthwaite's potential evapotranspiration for normal temperature conditions.
#'
#' This function returns a value for potential evapotranspiration as calculated by
#' Thornthwaite's potential evapotranspiration calculation method. This function is designed to
#' be applied when air temperatures fall below 26.5 degrees C. The equation used here
#' is given as equation 10 in Appendix 1 of Thornthwaite (1948). Note that the version
#' implemented here returns *millimeters*, rather than *centimeters* of PET, and therefore
#' the coefficient used here is '16' rather than '1.6'.
#'
#' @param air_temp_C Mean monthly air temperature at a station, in degrees Celcius.
#' @param heat_index_I Annual heat index \code{I} for a station.
#' @param TM_a Exponent \code{a} in Thornthwaite's PET calculation.
#'
#' @return Unadjusted potential evapotranspiration in millimeters.
#' @export
#'
#' @references
#' Thornthwaite, C.W., 1948, An Approach toward a Rational Classification of
#'  Climate: Geographical Review, v. 38, no. 1, p. 55.
calc_TM_PET <- function(air_temp_C, heat_index_I, TM_a) {

  I <- heat_index_I
  a <- TM_a

  if (air_temp_C < 26.5) {
    result <- ifelse(air_temp_C > 0, 16. * (10*air_temp_C / I)^a, 0.)
  } else {
    result <- calc_TM_e_hi_temp(air_temp_C)
  }

  return(result)

}
