new_model_cell <- function(rooting_depth, AWC, latitude, grow_start, grow_stop) {

  cel <- list(
    rooting_depth=rooting_depth,
    awc=AWC,
    smc=rooting_depth * AWC,
    latitude=latitude,
    grow_start=grow_start,
    grow_stop=grow_stop,
    sm=0.,
    grossprcp=0.,
    prcp=0.,
    potet=0.,
    actet=0.,
    tmax=0.,
    tmin=0.,
    tmean=0.,
    snowfall=0.,
    snow_storage=0.,
    interception=0.,
    interception_storage=0.,
    interception_storage_max=0.,
    apwl=0.,
    recharge=0.

  )

  cel$initialize <- function(sm_init, snowcover_init, max_int_nongrowing, max_int_growing) {
    assign('sm',sm_init, envir=cel)
    assign('snowcover',snowcover_init, envir=cel)
    assign('potentialinterception_nongrow', max_int_nongrowing, envir=cel)
    assign('potentialinterception_grow', max_int_growing, envir=cel)
  }

  cel$init_tm <- function(climate_df) {

    cel$mean_monthly_temps <- monthly_temps(climate_df)

    cel$mean_monthly_temps_C <- FtoC(cel$mean_monthly_temps)

    cel$i <- TM_i(cel$mean_monthly_temps_C)
    cel$I <- TM_I(cel$i)
    cel$a <- TM_a(cel$I)
    cel$daysinmonth <- getdaysinmonth(climate_df$Year[1])

  }

  cel$updateprecip <- function(grossprecip, tmax, tmin) {
    cel$grossprcp <- grossprecip
    #assign('grossprcp', grossprecip, envir=cel)
    assign('tmax', tmax, envir=cel)
    assign('tmin', tmin, envir=cel)
    assign('tmean', mean(c(tmax,tmin)), envir=cel)
  }

  cel$processprecip <- function(doy) {

    rFREEZING <- 32.
    rMELT_INDEX <- 1.5 * 9/5 * 25.4    # 1.5 mm/degC * 5

    lFREEZING <- ifelse (cel$tmean - (cel$tmax-cel$tmin) / 3.0 <= rFREEZING, TRUE, FALSE)

    if(doy < cel$grow_start || doy >= cel$grow_stop ) {
      #netrainfall <- cel$grossprcp - cel$potentialinterception_nongrow
      cel$interception_storage <- max(c(cel$interception_storage + cel$potentialinterception_nongrow,
                                      cel$interception_storage_max))
    } else {
      #netrainfall <- cel$grossprcp - cel$potentialinterception_grow
      cel$interception_storage <- max(c(cel$interception_storage + cel$potentialinterception_grow,
                                        cel$interception_storage_max))
    }

    if ( netrainfall < 0. ) netrainfall <- 0.

    assign('interception', cel$grossprcp - netrainfall, envir=cel)
    assign('netrainfall', netrainfall, envir=cel)

    if (lFREEZING ) {

      cel$snowcover <- cel$snowcover + netrainfall
      cel$netrainfall <- 0.
    } else {
      cel$netrainfall <- netrainfall
    }


    potentialmelt <- ifelse ( cel$tmean > rFREEZING, rMELT_INDEX * ( cel$tmax - rFREEZING ), 0.)

    if ( cel$snowcover > potentialmelt ) {
      cel$snowmelt <- potentialmelt
      cel$snowcover <- cel$snowcover - cel$snowmelt
    } else {   # not enough snowcover to satisfy the amount that *could* melt
      cel$snowmelt <- cel$snowcover
      cel$snowcover <- 0.
    }

  }

  cel$massbalance <- function() {

    APWL_CAP <- -40.69

    cel$netinfil <- cel$netrainfall + cel$snowmelt
    cel$pminuspe <- cel$netinfil - cel$potet

    cel$old_sm <- cel$sm

    if (cel$pminuspe < 0.) {

      cel$apwl <- max(APWL_CAP,(cel$apwl + cel$pminuspe))
      cel$old_sm <- cel$sm
      cel$sm <- calc_SoilMoisture(cel$smc, cel$apwl)
      cel$deltasm <- cel$sm - cel$old_sm

      if (cel$deltasm > abs(cel$pminuspe)) {
        cel$sm <- cel$old_sm + cel$pminuspe
        cel$deltasm <- cel$sm - cel$old_sm
      }

      cel$actet <- min(cel$netinfil + abs(cel$deltasm), cel$potet)
      cel$deficit <- cel$potet - cel$actet
      cel$surplus <- 0.

    } else {   # precip *EXCEEDS* Potential ET

      cel$deficit <- 0.
      cel$surplus <- max(0., cel$sm + cel$pminuspe - cel$smc)

      cel$sm <- min(cel$smc, (cel$sm + cel$pminuspe) )
      cel$deltasm <- cel$sm - cel$old_sm
      cel$actet <- cel$potet
      cel$apwl <- calc_APWL(cel$smc, cel$sm)

    }

    cel$recharge <- cel$surplus

  }

  cel$processet <- function(month, day, year, latitude) {

    lFREEZING = 32.

    TM_e <- TM_e(AirTemp=FtoC(cel$tmean), HeatIndex_I=cel$I, TM_a=cel$a)
    cel$potet <- TM_ea(month, day, year, cel$latitude,TM_e) / 25.4

    if(cel$tmean < lFREEZING) cel$potet <- 0.

  }

  cel$opendaily <- function(filename) {

    header <- paste("Date","MIN_TEMP","MAX_TEMP","AVG_TEMP","GROSS_PRECIP","INTERCEPTION","SNOWCOVER","SNOWMELT","NET_RAINFALL",
                    "NET_INFIL","P_MINUS_PE","POT_ET","ACT_ET","CHG_IN_SOIL_MOIS","SOIL_MOISTURE", "SM_SURPLUS","SM_DEFICIT",
                    "SM_APWL","RECHARGE", sep=",")

    cel$filename <- filename
    write(x=header, file=filename)

  }

  cel$dumpdaily <- function(currentdate) {

    SIGDIGITS <- 5

    outputtext <- paste(format(currentdate, "%m/%d/%Y"),
                        signif(cel$tmin, digits=SIGDIGITS),
                        signif(cel$tmax, digits=SIGDIGITS),
                        signif(cel$tmean, digits=SIGDIGITS),
                        signif(cel$grossprcp, digits=SIGDIGITS),
                        signif(cel$interception, digits=SIGDIGITS),
                        signif(cel$snowcover, digits=SIGDIGITS),
                        signif(cel$snowmelt, digits=SIGDIGITS),
                        signif(cel$netrainfall, digits=SIGDIGITS),
                        signif(cel$netinfil, digits=SIGDIGITS),
                        signif(cel$pminuspe, digits=SIGDIGITS),
                        signif(cel$potet, digits=SIGDIGITS),
                        signif(cel$actet, digits=SIGDIGITS),
                        signif(cel$deltasm, digits=SIGDIGITS),
                        signif(cel$sm, digits=SIGDIGITS),
                        signif(cel$surplus, digits=SIGDIGITS),
                        signif(cel$deficit, digits=SIGDIGITS),
                        signif(cel$apwl, digits=SIGDIGITS),
                        signif(cel$recharge, digits=SIGDIGITS), sep=",")

    write(x=outputtext, file=cel$filename, append=TRUE)

  }

  cel$get <- function(itemname) {

    retval <- get(itemname, envir=cel)
    return(retval)
  }

  cel <- list2env(cel)
  class(cel) <- "model_cell"

  return(cel)

}
