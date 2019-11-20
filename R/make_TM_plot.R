make_monthly_df <- function(swb_obj) {
  
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

  return(list(df=df, df_monthly=df_monthly))

}

create_smooth_line <- function(x,y) {
  
  min_x <- min(x)
  max_x <- max(x)
  
  first_y <- y[1]
  
  # generate loess curve object
  lo <- loess(y ~ x, span=0.35)
  
  predicted_y <- predict(lo,xl)
  max_y <- max(predicted_y)
  #min_y <- min(predicted_y)
  min_y <- 0
  
  # create sequence of dummy 'filler' x values
  xl <- seq(min_x,max_x, (max_x - min_x)/1000)
  
  line <- data.frame(x=xl,y=predict(lo,xl))
  
  return(line)  
  
}

create_poly <- function(x, y) {
  
  min_x <- min(x)
  max_x <- max(x)
  
  first_y <- y[1]
  
  # generate loess curve object
  lo <- loess(y ~ x, span=0.35)

  predicted_y <- predict(lo,xl)
  max_y <- max(predicted_y)
  #min_y <- min(predicted_y)
  min_y <- 0
  
  # create sequence of dummy 'filler' x values
  xl <- seq(min_x,max_x, (max_x - min_x)/1000)
  
  poly <- data.frame(x=c(xl,max_x,min_x,min_x), 
             y=c(predict(lo,xl),min_y,min_y,first_y))
             
  return(poly)  
  
}


make_TM_plot <- function(swb_obj) {
  
  df <- make_monthly_df(swb_obj)$df
  df_monthly <- make_monthly_df(swb_obj)$df_monthly
  
  df_monthly$rain_plus_snowmelt <- df_monthly$rainfall + df_monthly$snowmelt
  df_monthly$maxy <- pmax(df_monthly$rain_plus_snowmelt, df_monthly$reference_et0)
  
  with(df_monthly, plot(month, rain_plus_snowmelt, type="n", ylim=c(0,max(df_monthly$maxy))))
  
  precip_line <- create_smooth_line(df_monthly$month, df_monthly$rain_plus_snowmelt)
  precip_poly <- create_poly(df_monthly$month, df_monthly$rain_plus_snowmelt)
  
  ref_et_line <- create_smooth_line(df_monthly$month, df_monthly$reference_et0)
  ref_et_poly <- create_poly(df_monthly$month, df_monthly$reference_et0)
  
  act_et_line <- create_smooth_line(df_monthly$month, df_monthly$actual_et)
  act_et_poly <- create_poly(df_monthly$month, df_monthly$actual_et)
  
  #with(ref_et_poly, polygon(x, y, density=14, angle=30, border=NA))
  
  #with(precip_poly, polygon(x, y, col="white", border=NA))
  with(precip_poly, polygon(x, y, col=rgb(0,0,1,1), border=NA))
  with(ref_et_poly, polygon(x, y, col=rgb(0,1,0,1), border=NA))
  
  with(act_et_poly, polygon(x, y, col=rgb(0,1,1,1), border=NA))
  
  with(precip_line, lines(x, y, lty="dashed"))
  with(ref_et_line, lines(x, y, lty="solid"))
  with(act_et_line, lines(x, y, lty="twodash"))
  
  with(df_monthly, points(month, rain_plus_snowmelt, pch=20))
  with(df_monthly, points(month, reference_et0, pch=1, cex=0.7))
  with(df_monthly, points(month, actual_et, pch=4, cex=0.7))
  
  
    
}