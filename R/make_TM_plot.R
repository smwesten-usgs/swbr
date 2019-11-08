make_TM_plot <- function(swb_obj) {
  
  df <- data.frame(date=swb_obj$date,
                   month=lubridate::month(swb_obj$date),
                   rainfall=swb_obj$net_rainfall,
                   snowmelt=swb_obj$snowmelt,
                   reference_et0=swb_obj$reference_et0,
                   actual_et=swb_obj$actual_et)

#  df_monthly <- df %>%
#                  dplyr::group_by(month) %>%
#                  dplyr::mutate_at(c("rainfall",
#                                     "snowmelt",
#                                     "reference_et0",
#                                     "actual_et"), sum)


  df_monthly <- df %>%
                dplyr::group_by(month) %>%
                dplyr::select(month,
                              rainfall,
                              snowmelt,
                              reference_et0,
                              actual_et) %>%
                dplyr::summarize(rainfall=sum(rainfall),
                                 snowmelt=sum(snowmelt),
                                 reference_et0=sum(reference_et0),
                                 actual_et=sum(actual_et))

  #plot(x=df_monthly$month, y=df_monthly$rainfall+df_monthly$snowmelt)
  #b <- bezierCurveFit(df_monthly$rainfall+df_monthly$snowmelt)
  #plot(b,type="l")

  return(list(df=df, df_monthly=df_monthly))

}
