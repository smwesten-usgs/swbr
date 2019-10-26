#' Title
#'
#' @slot month numeric.
#' @slot day numeric.
#' @slot year numeric.
#' @slot date Date.
#' @slot available_water_capacity numeric.
#' @slot rooting_depth numeric.
#' @slot curve_number numeric.
#' @slot latitude numeric.
#' @slot soil_moisture_storage numeric.
#' @slot interception_storage numeric.
#' @slot snow_storage numeric.
#' @slot accumulated_potential_water_loss numeric.
#' @slot reference_ET0 numeric.
#' @slot actual_ET numeric.
#' @slot p_minus_pe numeric.
#' @slot gross_precip numeric.
#' @slot tmin numeric.
#' @slot tmax numeric.
#' @slot tmean numeric.
#' @slot tmin_C numeric.
#' @slot tmax_C numeric.
#' @slot tmean_C numeric.
#' @slot gross_rainfall numeric.
#' @slot net_rainfall numeric.
#' @slot snowfall numeric.
#' @slot thornthwaite_heat_index_i numeric.
#'
#' @return
#' @export
#'
#' @examples
setClass("ModelCell",
  slots= c(
    month = "numeric",
    day = "numeric",
    year = "numeric",
    date = "Date",
    available_water_capacity= "numeric",
    rooting_depth = "numeric",
    curve_number = "numeric",
    latitude = "numeric",
    soil_moisture_storage = "numeric",
    interception_storage = "numeric",
    snow_storage = "numeric",
    accumulated_potential_water_loss = "numeric",
    reference_ET0 = "numeric",
    actual_ET = "numeric",
    p_minus_pe = "numeric",
    gross_precip = "numeric",
    tmin = "numeric",
    tmax = "numeric",
    tmean = "numeric",
    tmin_C = "numeric",
    tmax_C = "numeric",
    tmean_C = "numeric",
    gross_rainfall = "numeric",
    net_rainfall = "numeric",
    snowfall = "numeric",
    thornthwaite_heat_index_i = "numeric"
  ),
  prototype = list(
    date = lubridate::make_date(1900, 01, 01)
  )
)

setGeneric("month", function(this) standardGeneric("month") )
setGeneric("month<-", function(this,value) standardGeneric("month<-") )

setMethod("month", "ModelCell", function(this) this@month )
setMethod("month<-", "ModelCell", function(this,value) {
  this@month <- value
  validObject(this)
  this
})

setGeneric("day", function(this) standardGeneric("day") )
setGeneric("day<-", function(this,value) standardGeneric("day<-") )

setMethod("day", "ModelCell", function(this) this@day )
setMethod("day<-", "ModelCell", function(this,value) {
  this@day <- value
  validObject(this)
  this
})

setGeneric("year", function(this) standardGeneric("year") )
setGeneric("year<-", function(this,value) standardGeneric("year<-") )

setMethod("year", "ModelCell", function(this) this@year )
setMethod("year<-", "ModelCell", function(this,value) {
  this@year <- value
  validObject(this)
  this
})

#setGeneric("date", function(this) standardGeneric("date") )
setGeneric("date<-", function(this,value) standardGeneric("date<-") )

#setMethod("date", "ModelCell", function(this) this@date )
setMethod("date<-", "ModelCell", function(this,value) {
  this@date <- value
  validObject(this)
  this
})

setGeneric("rooting_depth", function(this) standardGeneric("rooting_depth") )
setGeneric("rooting_depth<-", function(this,value) standardGeneric("rooting_depth<-") )

setMethod("rooting_depth", "ModelCell", function(this) this@rooting_depth )
setMethod("rooting_depth<-", "ModelCell", function(this,value) {
  this@rooting_depth <- value
  validObject(this)
  this
})

setGeneric("gross_rainfall", function(this) standardGeneric("gross_rainfall") )
setGeneric("gross_rainfall<-", function(this,value) standardGeneric("gross_rainfall<-") )

setMethod("gross_rainfall", "ModelCell", function(this) this@gross_rainfall )
setMethod("gross_rainfall<-", "ModelCell", function(this,value) {
  this@gross_rainfall <- value
  validObject(this)
  this
})

setGeneric("net_rainfall", function(this) standardGeneric("net_rainfall") )
setGeneric("net_rainfall<-", function(this,value) standardGeneric("net_rainfall<-") )

setMethod("net_rainfall", "ModelCell", function(this) this@net_rainfall )
setMethod("net_rainfall<-", "ModelCell", function(this,value) {
  this@net_rainfall <- value
  validObject(this)
  this
})

setGeneric("snowfall", function(this) standardGeneric("snowfall") )
setGeneric("snowfall<-", function(this,value) standardGeneric("snowfall<-") )

setMethod("snowfall", "ModelCell", function(this) this@snowfall )
setMethod("snowfall<-", "ModelCell", function(this,value) {
  this@snowfall <- value
  validObject(this)
  this
})


setGeneric("tmin", function(this) standardGeneric("tmin") )
setGeneric("tmin<-", function(this,value) standardGeneric("tmin<-") )

setMethod("tmin", "ModelCell", function(this) this@tmin )
setMethod("tmin<-", "ModelCell", function(this,value) {
  this@tmin <- value
  validObject(this)
  this
})

setGeneric("tmax", function(this) standardGeneric("tmax") )
setGeneric("tmax<-", function(this,value) standardGeneric("tmax<-") )

setMethod("tmax", "ModelCell", function(this) this@tmax )
setMethod("tmax<-", "ModelCell", function(this,value) {
  this@tmax <- value
  validObject(this)
  this
})

setGeneric("tmin_C", function(this) standardGeneric("tmin_C") )
setGeneric("tmin_C<-", function(this,value) standardGeneric("tmin_C<-") )

setMethod("tmin_C", "ModelCell", function(this) this@tmin_C )
setMethod("tmin_C<-", "ModelCell", function(this,value) {
  this@tmin_C <- value
  validObject(this)
  this
})

setGeneric("tmax_C", function(this) standardGeneric("tmax_C") )
setGeneric("tmax_C<-", function(this,value) standardGeneric("tmax_C<-") )

setMethod("tmax_C", "ModelCell", function(this) this@tmax_C )
setMethod("tmax_C<-", "ModelCell", function(this,value) {
  this@tmax_C <- value
  validObject(this)
  this
})

setGeneric("tmean", function(this) standardGeneric("tmean") )
setGeneric("tmean<-", function(this,value) standardGeneric("tmean<-") )

setMethod("tmean", "ModelCell", function(this) this@tmean )
setMethod("tmean<-", "ModelCell", function(this,value) {
  this@tmean <- value
  validObject(this)
  this
})

setGeneric("tmean_C", function(this) standardGeneric("tmean_C") )
setGeneric("tmean_C<-", function(this,value) standardGeneric("tmean_C<-") )

setMethod("tmean_C", "ModelCell", function(this) this@tmean_C )
setMethod("tmean_C<-", "ModelCell", function(this,value) {
  this@tmean_C <- value
  validObject(this)
  this
})


setGeneric("gross_precip", function(this) standardGeneric("gross_precip") )
setGeneric("gross_precip<-", function(this,value) standardGeneric("gross_precip<-") )

setMethod("gross_precip", "ModelCell", function(this) this@gross_precip )
setMethod("gross_precip<-", "ModelCell", function(this,value) {
  this@gross_precip <- value
  validObject(this)
  this
})


setGeneric("calc_TM_heat_index", function(this) standardGeneric("calc_TM_heat_index"))


#' Calculate the Thornthwaite daily heat index value
#'
#' This method makes use of the slot "tmean_C"; it calculates the heat index and
#' populates the slot 'thornthwaite_heat_index_i'.
#'
#' @param ModelCell
#'
#' @return
#' @export
#'
#' @examples
#' mycell <- new("ModelCell)
#' tmean_C(mycell) <- c(12,13,12.5,14,11)
#' calc_TM_heat_index(mycell)
setMethod("calc_TM_heat_index", "ModelCell", function(this) {

  this@tmean <- ( this@tmax + this@tmin ) / 2
  this@tmean_C <- ( this@tmean - 32 ) * 5 / 9
  df <- data.frame(month=month(swb),tmean_C=tmean_C(swb))
  mean_monthly_air_temp <- df %>% dplyr::group_by(month) %>% mean(tmean_C, rm.na=TRUE)
  this@thornthwaite_heat_index_i <- swbr::calc_TM_i(mean_monthly_air_temp)
  validObject(this)
  this
})


setGeneric("calc_mean_air_temp", function(this) standardGeneric("calc_mean_air_temp"))
setMethod("calc_mean_air_temp", "ModelCell", function(this) {

  this@tmean <- ( this@tmax + this@tmin ) / 2
  this@tmean_C <- ( this@tmean - 32 ) * 5 / 9
  validObject(this)
  this
})

setGeneric("calc_TM_a_exponent", function(this) standardGeneric("calc_TM_a_exponent"))

#' Calculate the Thornthwaite 'a' exponent
#'
#' Calculate the exponent used in the Thornthwaite potential evapotranspiration calculation.
#'
#' @param ModelCell
#'
#' @return
#' @export
#'
#' @examples
#' mycell <- new("ModelCell)
setMethod("calc_TM_a_exponent", "ModelCell", function(this) {

  this@thornthwaite_heat_index_i <- swbr::calc_TM_i(this@tmean_C)
  validObject(this)
  this
})


setGeneric("make_date", function(this) standardGeneric("make_date"))
setMethod("make_date", "ModelCell", function(this) {
  this@date <- lubridate::make_date(this@year, this@month, this@day)
  validObject(this)
  this
})


setGeneric("init_TM_ET", function(object) standardGeneric("init_TM_ET"))
setMethod("init_TM_ET", "ModelCell", function(object) {




})
