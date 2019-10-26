library(R6)

ModelCell <- R6Class("ModelCell", 
  public = list(
    day = 1,
    year = 1900,
    month = 1,
    date = lubridate::make_date(year=1900,month=1,day=1),
    available_water_capacity=0.,
    rooting_depth = 0.,
    curve_number = 0,
    latitude = 0,
    soil_moisture_storage = 0,
    interception_storage = 0,
    snow_storage = 0,
    accumulated_potential_water_loss = 0,
    reference_et0 = 0,
    actual_et = 0,
    p_minus_pe = 0,
    gross_precip = 0,
    tmin = 0,
    tmax = 0,
    tmean = 0,
    tmin_C = 0,
    tmax_C = 0,
    tmean_C = 0,
    gross_rainfall = 0,
    net_rainfall = 0,
    snowfall = 0,
    calc_mean_air_temp = function() {
        self$tmean <- ( self$tmin + self$tmax ) / 2
    }
  ),
  private = list(
    thornthwaite_heat_index_i = 0,
    thornthwaite_exponent_a = 0
  )
)
