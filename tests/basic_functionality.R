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
swb$snow_storage_init <- 2.
swb$rooting_depth <- 1.5 # feet
swb$available_water_capacity <- 2.0 # inches/foot
swb$soil_moisture_storage_max <- swb$rooting_depth * swb$available_water_capacity

# populate date information
swb$month <- df$month
swb$day <- df$day
swb$year <- df$year
swb$latitude <- 42.
swb$date <- lubridate::make_date(df$year,df$month,df$day)
swb$calc_is_growing_season()

swb$calc_mean_air_temp()
swb$calc_monthly_mean_air_temp()
swb$calc_monthly_heat_index_i()
swb$calc_monthly_heat_index_exponent()
swb$calc_fraction_as_rain_classic()

swb$gross_rainfall <- swb$gross_precip * swb$fraction_as_rain

# calculate the potential interception, capping the amount so it doesn't exceed the
# actual amount of gross rainfall received on a given day
swb$calc_interception_bucket()
swb$potential_interception <- pmin(swb$potential_interception, swb$gross_rainfall)

swb$calc_TM_PET()
swb$calc_TM_adjusted_PET()

swb$snowfall <- swb$gross_precip * (1.0 - swb$fraction_as_rain)
swb$potential_snowmelt <- swb$calc_potential_snowmelt_classic()

swb$interception_storage <- rep(0, length(swb$potential_interception))
swb$interception <- rep(0, length(swb$potential_interception))
swb$interception_evap <- rep(0, length(swb$potential_interception))
swb$available_reference_et0 <- rep(0, length(swb$potential_interception))

swb$interception[1] <- swb$potential_interception[1]
swb$interception_storage[1] <- swb$interception[1]
for (n in 2:length(swb$potential_interception)) {
    swb$interception[n] <- pmin( swb$potential_interception[n],
                                (swb$interception_storage_max[n] - swb$interception_storage[n-1]))
    swb$interception_storage[n] <- pmin(swb$interception_storage[n-1] + swb$interception[n],
                                        swb$interception_storage_max[n])
    swb$interception_evap[n] <- pmin( swb$reference_et0[n], swb$interception_storage[n])
    swb$interception_storage[n] <- pmax( swb$interception_storage[n] - swb$interception_evap[n], 0.)
    swb$available_reference_et0[n] <- pmax(swb$reference_et0[n] - swb$interception_evap[n], 0.0)
}
swb$net_rainfall <- swb$gross_rainfall - swb$interception

swb$snow_storage[1] <- swb$snow_storage_init + swb$snowfall[1]
swb$snowmelt[1] <- min(c(swb$snow_storage[1], swb$potential_snowmelt[1]))
swb$snow_storage[1] <- swb$snow_storage[1] - swb$snowmelt[1]

for (n in 2:length(swb$potential_snowmelt)) {
    swb$snow_storage[n] <- swb$snow_storage[n-1] + swb$snowfall[n]
    swb$snowmelt[n] <- min(c(swb$snow_storage[n], swb$potential_snowmelt[n]))
    swb$snow_storage[n] <- swb$snow_storage[n] - swb$snowmelt[n]
}

swb$calc_sum_5_day_precip()
swb$calc_adjusted_curve_number()
swb$calc_runoff_cn()

swb$infiltration <- swb$net_rainfall + swb$snowmelt - swb$runoff

swb$p_minus_pe <- swb$infiltration - swb$available_reference_et0

if (swb$p_minus_pe[1] > 0) {
  swb$soil_moisture_storage[1] <- swb$soil_moisture_storage_init
  swb$soil_moisture_storage[1] <- swb$soil_moisture_storage[1] + swb$infiltration
} else {
  swb$accumulated_potential_water_loss[1] <- swb$accumulated_potential_water_loss[1] - swb$p_minus_pe

}  

