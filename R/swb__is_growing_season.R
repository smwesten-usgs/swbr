#' Determine if given date is within growing season
#'
#' @param date Date to classify as "growing" or "non-growing" season.
#' @param growing_season_start Month and day of growing season start (mm/dd).
#' @param growing_season_end Month and day of growing season end (mm/dd).
#'
#' @return Classification of date regarding whether it falls within growing season (TRUE/FALSE).
#' @export
#'
#' @examples
is_growing_season <- function(date, growing_season_start, growing_season_end) {

  start_month <- unlist(stringr::str_split(growing_season_start,pattern="/"))[1]
  start_day <- unlist(stringr::str_split(growing_season_start,pattern="/"))[2]

  end_month <- unlist(stringr::str_split(growing_season_end,pattern="/"))[1]
  end_day <- unlist(stringr::str_split(growing_season_end,pattern="/"))[2]

  year <- lubridate::year(date)

  start_date <- lubridate::make_date(year,start_month,start_day)
  end_date <- ifelse(end_month < start_month,
                     lubridate::make_date(year+1,end_month,end_day),
                     lubridate::make_date(year,end_month,end_day))

  return(ifelse( (date >= start_date) & (date <= end_date),
                 TRUE,
                 FALSE)
  )
}
