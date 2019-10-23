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
#' Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables for
#' computing potential evapotranspiration and the water balance: Publications
#' in Climatology, v. 10, no. 3, p. 1–104.
calc_APWL <- function(max_soil_moisture_in, soil_moisture_in) {

  TM_slope_term <-  0.478769194198665
  TM_exp_term   <- -1.03678439421169

  APWL <- ( log10(max_soil_moisture_in ) - log10(soil_moisture_in)) /
                ( TM_slope_term * max_soil_moisture_in^TM_exp_term )

  return (APWL)

}
