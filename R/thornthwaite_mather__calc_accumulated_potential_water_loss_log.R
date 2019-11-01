#' Calculate accumulated potential water loss.
#'
#' This function returns a calculated accumulated potential water loss value given
#' the current soil moisture and maximum soil moisture for the soil.
#'
#' The value is generated from a nonlinear equation fit to the data values given in
#' Thornthwaite and Mather (1957).
#'
#' @param max_soil_moisture_in Maximum soil moisture storage with the root zone, in inches.
#' @param soil_moisture_in Soil moisture, in inches.
#'
#' @return Accumulated potential water loss in inches.
#' @export
#'
#' @references
#' Steenhuis, T.S., and Van der Molen, W.H., 1986, The Thornthwaite-Mather procedure
#' as a simple engineering method to predict recharge: Journal of Hydrology,
#' v. 84, no. 3–4, p. 221–229.
#'
#' Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables for
#' computing potential evapotranspiration and the water balance: Publications
#' in Climatology, v. 10, no. 3, p. 1–104.
calc_TM_APWL_log <- function(max_soil_moisture_in, soil_moisture_in) {

  APWL <- -max_soil_moisture_in * log(soil_moisture_in / max_soil_moisture_in)

  return (APWL)

}
