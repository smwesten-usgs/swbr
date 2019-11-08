library(swbr)
library(magrittr)

data(coshocton_1999)

df <- coshocton_1999

# create new cell object and populate tmin, tmax
swb <- ModelCell$new()
swb$gross_precip <- df$precip
swb$tmax <- df$tmax
swb$tmin <- df$tmin
swb$base_curve_number <- 77
swb$snow_storage_init <- 2.
swb$rooting_depth <- 2.0 # feet
swb$available_water_capacity <- 3.0 # inches/foot
swb$soil_moisture_storage_max <- swb$rooting_depth * swb$available_water_capacity
swb$soil_moisture_storage_init <- swb$soil_moisture_storage_max

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

numrecs <- length(swb$potential_interception)
swb$interception <- rep(0, numrecs)
swb$net_infiltration <- rep(0, numrecs)
swb$available_reference_et0 <- rep(0, numrecs)
swb$soil_moisture_storage <- rep(0, numrecs)
swb$actual_et <- rep(0, numrecs)
swb$accumulated_potential_water_loss <- rep(0, numrecs)
swb$soil_moisture_storage_max <- rep(swb$soil_moisture_storage_max, numrecs)

swb$interception <- swb$potential_interception
swb$net_rainfall <- swb$gross_rainfall - swb$interception
swb$available_reference_et0 <- swb$reference_et0

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

for (n in 1:numrecs) {
  if (swb$p_minus_pe[n] > 0) {

    swb$actual_et[n] <- swb$available_reference_et0[n]

    if (n==1) {
      swb$soil_moisture_storage[n] <- swb$soil_moisture_storage_init + swb$p_minus_pe[n]
    } else {
      swb$soil_moisture_storage[n] <- swb$soil_moisture_storage[n-1] + swb$p_minus_pe[n]
    }

    if (swb$soil_moisture_storage[n] > swb$soil_moisture_storage_max[n]) {
      swb$net_infiltration[n] <- swb$soil_moisture_storage[n] - swb$soil_moisture_storage_max[n]
      swb$soil_moisture_storage[n] <- swb$soil_moisture_storage_max[n]
      swb$accumulated_potential_water_loss[n] <- 0
    } else {
      swb$calc_TM_update_APWL_orig(n)
      swb$net_infiltration[n] <- 0
    }
  } else {

    if (n==1) {
      swb$accumulated_potential_water_loss[n] <-  - swb$p_minus_pe[n]
      swb$calc_TM_update_soil_moisture_orig(n)
      swb$actual_et[n] <- swb$soil_moisture_storage_init - swb$soil_moisture_storage[n]
    } else {
      swb$accumulated_potential_water_loss[n] <- swb$accumulated_potential_water_loss[n-1] - swb$p_minus_pe[n]
      swb$calc_TM_update_soil_moisture_orig(n)
      swb$actual_et[n] <- swb$soil_moisture_storage[n-1] - swb$soil_moisture_storage[n]
    }

  }  
}

swb$actual_et <- swb$actual_et + swb$interception_evap

