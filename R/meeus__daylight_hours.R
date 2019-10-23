#' Calculate number of daylight hours
#'
#' Returns the number of daylight hours for a given latitude and time of year.
#'
#' @param month Month of year (1-12)
#' @param day Day of month (1-31)
#' @param year Year (YYYY)
#' @param latitude Latitude of station, in decimal degrees?
#'
#' @return Number of daylight hours
#' @export
#'
#' @references
#' Meeus, J.H., 1991, Astronomical algorithms: Willmann-Bell, Incorporated, 477 p.
calc_daylight_hours <- function(month, day, year, latitude) {

  # convert from decimal degrees to radians
  latitude_radians <- latitude * pi / 180

  date <- lubridate::make_datetime(year=year, month=month, day=day, hour=12)

  frac_year <- lubridate::decimal_date(date) - trunc(lubridate::decimal_date(date))
  solar_decl <- 0.4093 * sin((2 * pi * lubridate::decimal_date(date)) - 1.405)
  sunset_angle <- acos( - tan(latitude_radians) * tan(solar_decl) )
  daylight_hours <- 24. / pi * sunset_angle

  return(daylight_hours)

}
