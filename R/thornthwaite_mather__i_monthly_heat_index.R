#' Calculate monthly heat index \code{i} in Thornthwaite's calculation of potential evapotranspiration.
#'
#' This function returns monthly heat index \code{i} used in Thornthwaite's potential
#' evapotranspiration calculation method. The equation used here is given in the text
#' of Appendix 1 (page 89) in Thornthwaite (1948).
#'
#' @param air_temp_C Mean monthly air temperature at a station, in degrees Celcius.
#'
#' @return Monthly heat index \code{i} in Thornthwaite's PET calculation method.
#' @export
#'
#' @references
#' Thornthwaite, C.W., 1948, An Approach toward a Rational Classification of
#'  Climate: Geographical Review, v. 38, no. 1, p. 55.
calc_TM_i <- function(air_temp_C) {

  return( ifelse(air_temp_C > 0., ( air_temp_C / 5.0 )^1.514, 0.) )

}
