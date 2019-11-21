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
    runoff=0.,
    infiltration=0.,
    rooting_depth = 0.,
    available_water_capacity = 0,
    curve_number = 0,
    cfgi = 0,
    cfgi_init = 0,
    base_curve_number = 0,
    latitude = 0,
    vars=data.frame(date=numeric(),
                    soil_moisture_storage = numeric(),
                    interception_storage = numeric(),
                    snow_storage = numeric()),
    # -- state variables --
    soil_moisture_storage = 0,
    soil_moisture_storage_max = 0,
    soil_moisture_storage_init=0,
    interception_storage = 0,
    interception_storage_max = 0,
    snow_storage = 0,
    snow_storage_init = 0.,
    accumulated_potential_water_loss = 0,
    unadjusted_pet=0,
    reference_et0 = 0,
    available_reference_et0=0,
    actual_et = 0,
    p_minus_pe = 0,
    gross_precip = 0,
    net_precip=0,
    sum_5_day_precip=0,
    tmin = 0,
    tmax = 0,
    tmean = 0,
    tmin_C = 0,
    tmax_C = 0,
    tmean_C = 0,
    fraction_as_rain = 0.,
    gross_rainfall = 0,
    potential_interception = 0,
    interception_evap=0,
    growing_season_interception_value = 0.05,
    nongrowing_season_interception_value = 0.0,
    interception = 0,
    net_rainfall = 0,
    net_infiltration=0,
    snowfall = 0,
    potential_snowmelt = 0,
    snowmelt = 0,
    arc_i__dormant = 0.5,
    arc_iii__dormant = 1.1,
    arc_i__growing = 1.4,
    arc_iii__growing = 2.1,
    arc_i_threshold = 0,
    arc_iii_threshold = 0,
    # -- Thornthwaite-Mather related parameter values --
    thornthwaite_heat_index_i = 0,
    thornthwaite_annual_heat_index_I=0.,
    thornthwaite_exponent_a = 0.,
    monthly_mean_air_temp= 0.,
    growing_season_start = "05/14",
    growing_season_end = "09/24",
    is_growing_season=FALSE,
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
      self$sum_5_day_precip = calc_sum_5_day_precip(self$net_rainfall+self$snowmelt)
    },
    calc_arc_threhold = function() {
      self$arc_i_threshold <- ifelse(self$is_growing_season,
                                     self$arc_i__growing,
                                     self$arc_i__dormant)
      self$arc_iii_threshold <- ifelse(self$is_growing_season,
                                       self$arc_iii__growing,
                                       self$arc_iii__dormant)
    },
    calc_cfgi = function(n, tmean_c, snow_storage, cfgi) {
      self$cfgi[n] <- swb__update_cfgi(tmean_c,
                                       snow_storage,
                                       cfgi)
    },
    calc_adjusted_curve_number =function() {
      self$curve_number <- adjust_curve_number(self$base_curve_number,
                                               self$sum_5_day_precip,
                                               self$arc_i_threshold,
                                               self$arc_iii_threshold,
                                               self$cfgi)
    },
    calc_runoff_cn = function() {
      self$runoff <- calc_runoff_cn(self$curve_number,
                                    self$net_rainfall + self$snowmelt,
                                    hawkins=TRUE)
    },
    calc_fraction_as_rain_classic = function() {
      self$fraction_as_rain <- calc_fraction_rain_classic(self$tmin,
                                                          self$tmax,
                                                          self$tmean)
    },
    calc_potential_snowmelt_classic = function() {
      self$potential_snowmelt <- calc_potential_snowmelt_classic(self$tmax,
                                                                 self$tmean)
    },
    calc_is_growing_season = function() {
      self$is_growing_season <- is_growing_season(self$date, 
                                                  self$growing_season_start,
                                                  self$growing_season_end)
    },
    calc_interception_bucket = function() {
      self$interception_storage_max <- calc_potential_interception_bucket(self$is_growing_season,
                                                                          self$growing_season_interception_value,
                                                                          self$nongrowing_season_interception_value)
      self$potential_interception <- pmin(self$interception_storage_max, self$gross_precip)
    },
    calc_TM_update_soil_moisture = function(n) {
      self$soil_moisture_storage[n] <- calc_TM_soil_moisture_exp( self$soil_moisture_storage_max[n],
                                                                  self$accumulated_potential_water_loss[n])
    },
    calc_TM_update_APWL = function(n) {
      self$accumulated_potential_water_loss[n] <- calc_TM_APWL_log(self$soil_moisture_storage_max[n],
                                                                   self$soil_moisture_storage[n])
    },
    calc_TM_update_soil_moisture_orig = function(n) {
      self$soil_moisture_storage[n] <- calc_TM_soil_moisture_fitted( self$soil_moisture_storage_max[n],
                                                                  self$accumulated_potential_water_loss[n])
    },
    calc_TM_update_APWL_orig = function(n) {
      self$accumulated_potential_water_loss[n] <- calc_TM_APWL_fitted(self$soil_moisture_storage_max[n],
                                                                   self$soil_moisture_storage[n])
    }
  )
)
