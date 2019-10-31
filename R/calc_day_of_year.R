#' Calculate day of year for a given date
#'
#' @param date Date for which the day-of-year is desired.
#'
#' @return Day of year, assuming January 1 is day 1.
#' @export
#'
#' @examples
day_of_year <- function(date) {

  year <- lubridate::year(date)
  start_date <- lubridate::make_date(year=year, month=1, day=1)
  return(as.numeric(date - start_date + 1))
}
