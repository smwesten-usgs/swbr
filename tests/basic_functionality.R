library(swbr)
library(magrittr)

data(coshocton_1999)

df <- coshocton_1999

# create new cell object and populate tmin, tmax
swb <- ModelCell$new()
swb$gross_precip <- df$precip
swb$tmax <- df$tmax
swb$tmin <- df$tmin
swb$base_curve_number <- 78

# populate date information
swb$month <- df$month
swb$day <- df$day
swb$year <- df$year
swb$latitude <- 42.
swb$date <- lubridate::make_date(df$year,df$month,df$day)

swb$calc_mean_air_temp()
swb$calc_monthly_mean_air_temp()
swb$calc_monthly_heat_index_i()
swb$calc_monthly_heat_index_exponent()
swb$calc_TM_PET()
swb$calc_TM_adjusted_PET()
swb$calc_sum_5_day_precip()
swb$calc_adjusted_curve_number()
