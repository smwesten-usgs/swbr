#' Title
#'
#' @param month
#' @param day
#' @param year
#' @param latitude
#' @param TM_e
#'
#' @return
#' @export
#'
#' @examples
calc_TM_PET_adj <- function(month, day, year, latitude, TM_e) {

  h <- calc_daylight_hours(month, day, year, latitude)
  TM_PET_adj <- TM_e /30 * (h/12)

  return(TM_PET_adj)

}
