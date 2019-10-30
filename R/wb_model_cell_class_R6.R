library(R6)
library(magrittr)

#' ModelCell class
#'
#' @export
#' @examples
#' # Integrative examples go here.
ModelCell <- R6Class("ModelCell",
  public = list(
    day = 1,
    year = 1900,
    month = 1,
    date = lubridate::make_date(year=1900,month=1,day=1),
    available_water_capacity=0.,
    rooting_depth = 0.,
    curve_number = 0,
    base_curve_number = 0,
    latitude = 0,
    soil_moisture_storage = 0,
    interception_storage = 0,
    snow_storage = 0,
    accumulated_potential_water_loss = 0,
    unadjusted_pet=0,
    reference_et0 = 0,
    actual_et = 0,
    p_minus_pe = 0,
    gross_precip = 0,
    sum_5_day_precip=0,
    tmin = 0,
    tmax = 0,
    tmean = 0,
    tmin_C = 0,
    tmax_C = 0,
    tmean_C = 0,
    gross_rainfall = 0,
    net_rainfall = 0,
    snowfall = 0,
    thornthwaite_heat_index_i = 0,
    thornthwaite_annual_heat_index_I=0.,
    thornthwaite_exponent_a = 0.,
    monthly_mean_air_temp= 0.,
    calc_mean_air_temp = function() {
        self$tmean <- ( self$tmin + self$tmax ) / 2
        self$tmean_C <- ( self$tmean - 32 ) * 5/9
    },
    calc_monthly_mean_air_temp = function() {
      df <- data.frame(month=self$month, tmean_C=self$tmean_C)
      self$monthly_mean_air_temp <-  df %>% dplyr::group_by(month) %>% dplyr::summarize(tmean_mo_C=mean(tmean_C)) %>% dplyr::pull(tmean_mo_C)
    },
    calc_monthly_heat_index_i = function() {
      self$thornthwaite_heat_index_i <- calc_TM_i(self$monthly_mean_air_temp)
      self$thornthwaite_annual_heat_index_I <- sum(self$thornthwaite_heat_index_i)
    },
    calc_monthly_heat_index_exponent = function() {
      self$thornthwaite_exponent_a <- calc_TM_a(self$thornthwaite_annual_heat_index_I)
    },
    calc_TM_PET = function() {
      self$unadjusted_pet <- calc_TM_PET(self$tmean_C,
                                         self$thornthwaite_annual_heat_index_I, 
                                         self$thornthwaite_exponent_a) / 25.4 /
                                         lubridate::days_in_month(self$date)
    },
    calc_TM_adjusted_PET = function() {
      self$reference_et0 = calc_TM_PET_adj(self$month,
                                           self$day,
                                           self$year,
                                           self$latitude, 
                                           self$unadjusted_pet)
    },
    calc_sum_5_day_precip = function() {
      self$sum_5_day_precip = calc_sum_5_day_precip(self$gross_precip)
    },
    calc_adjusted_curve_number =function() {
      self$curve_number <- adjust_curve_number(swb$base_curve_number,
                                                        swb$sum_5_day_precip,
                                                        1.1,
                                                        2.4)
    } 
  )
)
