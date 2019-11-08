#' Partition precipitation into rain and snow (SWB classic)
#'
#' @param cel 
#'
#' @return
#' @export
#'
#' @examples
swb_partition_rain_snow <- function(cel) {
  
  FREEZING <- 32.
  MELT_INDEX <- 1.5 * 5/9 / 25.4    # 1.5 mm/degC
  
  is_freezing <- cel$tmean - (cel$tmax-cel$tmin) / 3.0 <= FREEZING
  
  cel$rainfall <- ifelse(is_freezing, 0.0, cel$gross_precip)
  cel$snowfall <- ifelse(is_freezing, cel$gross_precip, 0.0)
}
