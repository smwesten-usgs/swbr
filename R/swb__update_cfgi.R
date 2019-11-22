swb__update_cfgi <- function(tmean_c, snow_storage, cfgi) {
  
  A <- 0.97
  swe_to_cm <- 10 * 2.54  #snow water-equivalent to centimeters conversion

  snow_depth_cm <- snow_storage * swe_to_cm
  
  cfgi <- ifelse( tmean_c > 0,
                  max(A*cfgi - tmean_c * exp(-0.4 * 0.5 * snow_depth_cm), 0),
                  max(A*cfgi - tmean_c * exp(-0.4 * 0.08 * snow_depth_cm), 0)
          )
  
  return(cfgi)
}
