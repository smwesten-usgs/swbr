make_TM_plot <- function(date,
                         rainfall,
                         snowmelt,
                         reference_et0,
                         actual_et) {

  df <- data.frame(date=date,
                   month=lubridate::month(date),
                   rainfall=rainfall,
                   snowmelt=snowmelt,
                   reference_et0=reference_et0,
                   actual_et=actual_et)

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
