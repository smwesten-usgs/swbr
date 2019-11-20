#' Calculate potential snowmelt (SWB classic)
#'
#' @param tmax Maximum air temperature, in degrees F.
#' @param tmean Mean air temperature, in degrees F.
#'
#' @return Potential melt amount, in inches.
#' @export
#'
#' @examples
calc_potential_snowmelt_classic <- function(tmax, tmean) {

  MELT_INDEX = 1.5 /25.4 * 5/9  # inches per degree F above freezing (1.5 mm per degree C above freezing)

  # freezing point of water, degrees F
  FREEZING_PT_F <- 32.

  potential_melt <- ifelse ( tmean > FREEZING_PT_F, MELT_INDEX * ( tmax - FREEZING_PT_F ), 0.)

  return(potential_melt)

}
