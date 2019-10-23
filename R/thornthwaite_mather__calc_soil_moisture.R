#' Calculate soil moisture.
#'
#' This function returns a calculated soil moisture value given the current
#' accumulated potential water loss (APWL) and maximum soil moisture for the soil.
#'
#' Accumulated potential water loss is calculated separately and represents the
#' sum of the difference between the potential evapotranspiration and the
#' actual evapotranspiration.
#'
#' The value is generated from a nonlinear equation fit to the data values given in
#' Thornthwaite and Mather (1957).
#'
#' @param max_soil_moisture_in Maximum soil moisture storage with the root zone, in inches.
#' @param apwl_in Accumulated potential water loss, in inches.
#'
#' @return Soil moisture in inches.
#' @export
#'
#' @references
#' Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables for
#' computing potential evapotranspiration and the water balance: Publications
#' in Climatology, v. 10, no. 3, p. 1–104.
calc_soil_moisture <- function( max_soil_moisture_in, apwl_in) {

  TM_slope_term <- 0.478769194198665
  TM_exp_term <- -1.03678439421169

  SWC <- max_soil_moisture_in

  soil_moisture_in <- 10^( log10(SWC) -
                                       ( abs(apwl_in) * TM_slope_term * SWC^TM_exp_term  ) )

  return(soil_moisture_in)

}
