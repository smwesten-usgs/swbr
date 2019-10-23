#' Calculate exponent 'a' in Thornthwaite's calculation of potential evapotranspiration.
#'
#' This function returns the exponent 'a' in Thornthwaite's potential evapotranspiration
#' calculation method. The version implemented here is given as equation 9 in Thornthwaite (1948).
#'
#' @param heat_index_I Annual heat index at a climate station.
#'
#' @return Exponent 'a' in Thornthwaite's PET calculation method.
#' @export
#'
#' @references
#' Thornthwaite, C.W., 1948, An Approach toward a Rational Classification of
#'  Climate: Geographical Review, v. 38, no. 1, p. 55.
calc_TM_a <- function(heat_index_I) {

  I <- heat_index_I

  return( 6.75E-7 * I^3 - 7.71E-5 * I^2 + 1.7921E-2 * I + 0.49239 )

}
