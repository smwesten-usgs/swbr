#' Calculate running sum of precipitation, current day plus previous 4 days
#'
#' @param precip Daily precipitation value.
#'
#' @return Sum of today's precip value along with previous 4 days' worth of values.
#' @export
#'
#' @examples
#'
#' precip <- c(0.2,0.,0.1,0.5,0,0,.25,0,0.75,0.,0.)
#' calc_sum_5_day_precip(precip)
calc_sum_5_day_precip <- function(precip) {

  df <- data.frame(lag_0d=precip,
                   lag_1d=dplyr::lag(precip, n=1, default=0.0),
                   lag_2d=dplyr::lag(precip, n=2, default=0.0),
                   lag_3d=dplyr::lag(precip, n=3, default=0.0),
                   lag_4d=dplyr::lag(precip, n=4, default=0.0),
                   lag_5d=dplyr::lag(precip, n=5, default=0.0))

  lagged_precip <- with( df, lag_0d + lag_1d + lag_2d + lag_3d + lag_4d )

  return( lagged_precip )

}
