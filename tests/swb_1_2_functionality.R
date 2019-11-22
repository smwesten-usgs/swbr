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
swb$cfgi_init <- 0.
swb$rooting_depth <- 2.0 # feet
swb$available_water_capacity <- 3.0 # inches/foot
swb$soil_moisture_storage_max <- swb$rooting_depth * swb$available_water_capacity
swb$soil_moisture_storage_init <- swb$soil_moisture_storage_max

# initialize water balance variables
numrecs <- length(swb$gross_precip)
swb$interception <- rep(0, numrecs)
swb$cfgi <- rep(0, numrecs)
swb$net_infiltration <- rep(0, numrecs)
swb$available_reference_et0 <- rep(0, numrecs)
swb$soil_moisture_storage <- rep(0, numrecs)
swb$actual_et <- rep(0, numrecs)
swb$accumulated_potential_water_loss <- rep(0, numrecs)
swb$soil_moisture_storage_max <- rep(swb$soil_moisture_storage_max, numrecs)

# populate date information
swb$month <- df$month
swb$day <- df$day
swb$year <- df$year
swb$latitude <- 42.
swb$date <- lubridate::make_date(df$year,df$month,df$day)
swb$calc_is_growing_season()

# munge daily weather data to produce heat index i and exponents needed by Thornthwaite
# PET method
swb$calc_mean_air_temp()
swb$calc_monthly_mean_air_temp()
swb$calc_monthly_heat_index_i()
swb$calc_monthly_heat_index_exponent()

# obtain fraction of precipitation that falls as rain
swb$calc_fraction_as_rain_classic()

# calculate rainfall based on the previously calculated rainfall fraction
swb$gross_rainfall <- swb$gross_precip * swb$fraction_as_rain

# calculate the potential interception, capping the amount so it doesn't exceed the
# actual amount of gross rainfall received on a given day
swb$calc_interception_bucket()

# potantial interception: cannot have interception values > gross precipitation amounts
swb$potential_interception <- pmin(swb$potential_interception, swb$gross_precip)

# calculate Thornthwaite potential evapotranspiration
swb$calc_TM_PET()
swb$calc_TM_adjusted_PET()

swb$interception[1] <- swb$potential_interception[1]
swb$interception_storage[1] <- swb$interception[1]
for (n in 2:length(swb$potential_interception)) {
  swb$interception[n] <- min(c(swb$potential_interception[n],
                               swb$interception_storage_max[n] - swb$interception_storage[n-1]))
  swb$interception_storage[n] <- swb$interception_storage[n-1] + swb$interception[n]
#  rFraction_Wet = ( cel%rInterceptionStorage / ( rMAXIMUM_INTERCEPTION_STORAGE + 1.0e-6) )**0.66666667_c_float
#  rPotential_Evaporated_Interception = min( cel%rReferenceET0, rFraction_Wet * cel%rInterceptionStorage )
#  cel%rInterceptionStorage = max( cel%rInterceptionStorage - rPotential_Evaporated_Interception, 0.0_c_float )
#  cel%rActual_ET_interception = max( rPrevious_Interception_Storage - cel%rInterceptionStorage, 0.0_c_float )
  fraction_wet <- ( swb$interception_storage[n] / swb$interception_storage_max + 1.0e-6)^0.6666666667
  potential_evap_interception <- min(c(swb$reference_et0[n], 
                                       fraction_wet * swb$interception_storage[n]))  
  previous_interception_storage <- swb$interception_storage[n]
  swb$interception_storage[n] <- max(c(swb$interception_storage[n] - potential_evap_interception, 0.))
  swb$interception_evap[n] <- max( c(previous_interception_storage - swb$interception_storage[n], 0))
  swb$available_reference_et0[n] <- swb$reference_et0[n] - swb$interception_evap[n]
}

swb$net_rainfall <- swb$gross_rainfall - swb$interception
swb$snowfall <- swb$net_precip * (1.0 - swb$fraction_as_rain)

swb$potential_snowmelt <- swb$calc_potential_snowmelt_classic()
swb$snow_storage[1] <- swb$snow_storage_init + swb$snowfall[1]
swb$snowmelt[1] <- min(c(swb$snow_storage[1], swb$potential_snowmelt[1]))
swb$snow_storage[1] <- swb$snow_storage[1] - swb$snowmelt[1]

# CFGI is calculated on the basis of the snow_storage of the previous day
swb$calc_cfgi(1, swb$tmean_C[1], swb$snow_storage_init, swb$cfgi_init)

for (n in 2:length(swb$potential_snowmelt)) {
    swb$calc_cfgi(n, swb$tmean_C[n], swb$snow_storage[n-1], swb$cfgi[n-1])
    swb$snow_storage[n] <- swb$snow_storage[n-1] + swb$snowfall[n]
    swb$snowmelt[n] <- min(c(swb$snow_storage[n], swb$potential_snowmelt[n]))
    swb$snow_storage[n] <- swb$snow_storage[n] - swb$snowmelt[n]
}

swb$calc_sum_5_day_precip()              # curve-number 5-day sum of precip + snowmelt
swb$calc_arc_threhold()                  # update antecedant runoff condition (ARC) numbers
#
# ***because of the sequencing of statements in SWB 1.0, the routine that updates the 
# curve number actually sees a lagged version of the 5-day sum of precip***
#
swb$calc_adjusted_curve_number(lag=1)    # update curve number based on ARC
swb$calc_runoff_cn()                     # calculate runoff

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
    
  } else {   # P minus PE < 0; evapotranspiration exceeds precipitation

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

