#' Calculate Thornthwaite's potential evapotranspiration for high air temperature conditions.
#'
#' This function returns a value for potential evapotranspiration as calculated by
#' Thornthwaite's potential evapotranspiration calculation method. This function is designed to
#' be applied when air temperatures fall between 26.5 and 38 degrees C. The data used here
#' are given in figure 13 of Appendix 1 in Thornthwaite (1948). The equation
#' summarizing these result is given in Willmott and others (1985).
#'
#' @param air_temp_C Mean monthly air temperature at a station, in degrees Celcius.
#'
#' @return Unadjusted potential evapotranspiration in millimeters.
#' @export
#'
#' @references
#' Thornthwaite, C.W., 1948, An Approach toward a Rational Classification of
#'  Climate: Geographical Review, v. 38, no. 1, p. 55.
#'
#' Willmott, C.J., Rowe, C.M., and Mintz, Y., 1985, Climatology of
#'  the terrestrial seasonal water cycle: Journal of Climatology,
#'  v. 5, no. 6, p. 589–606.

calc_TM_PE_hi_temp <- function(air_temp_C) {

#  # These values come from Fig 13, Thornthwaite and Mather (1948)
#  temp <- seq(26.5,38,0.5)
#  pe <- c(13.5,13.95,14.37,14.78,15.17,15.54,15.89,16.21,16.52,16.80,17.07,17.31,
#          17.53,17.72,17.9,18.05,18.18,18.29,18.37,18.43,18.47,18.49,18.5,18.50)

# result_list <- approx(x=temp, y=pe, xout=air_temp_C)

  result <- -415.85 + 32.24 * air_temp_C - 0.43 * air_temp_C^2


  return( result )
}
