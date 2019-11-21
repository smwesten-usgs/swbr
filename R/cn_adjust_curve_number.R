#' Adjust curve number
#'
#' @param base_curve_number Base curve number assigned to a given land use.
#' @param five_day_precip Running sum of previous five days of precipitation.
#' @param dry_threshold Threshold that defines 'dry' conditions.
#' @param wet_threshold Threshold that defines 'wet' conditions.
#' @param cfgi Continuous frozen ground index; used to increase curve number under freezing conditions.
#'
#' @return Curve number updated for current soil moisture conditions.
#' @export
#'
#' @examples
#' # update curve number for 'dry, growing season' conditions
#' cn <- adjust_curve_number(base_curve_number=82, five_day_precip=1.3,
#'                           dry_threshold=1.4, wet_threshold=2.1, cfgi=76)
adjust_curve_number <- function(base_curve_number, five_day_precip,
                                dry_threshold, wet_threshold, cfgi) {

  ll_cfgi <- 50
  
  ul_cfgi <- 80
    
  prob_runoff_enhancement <- function(cfgi, ll_cfgi, ul_cfgi) {
    
    pRE <- (cfgi - ll_cfgi) / (ul_cfgi - ll_cfgi)

    pRE <- ifelse( pRE < 0, 
                   0,
                   pRE )
    
    pRE <- ifelse( pRE > 1,
                   1,
                   pRE)
    
    return(pRE)
  }
    
  pRE <- prob_runoff_enhancement(cfgi, ll_cfgi, ul_cfgi)
  
# use probability of runoff enhancement to calculate a weighted
# average of curve number under Type II vs Type III antecedent
# runoff conditions

  curve_number <- ifelse(pRE > 0.001,
                         base_curve_number * (1 - pRE) +  
                           base_curve_number / (0.427 + 0.00573 * base_curve_number) * pRE,
                         ifelse ( five_day_precip < dry_threshold,
                                  base_curve_number / ( 2.281 - 0.01281 * base_curve_number ),
                                  ifelse((five_day_precip >= dry_threshold) & (five_day_precip < wet_threshold),
                                          base_curve_number,
                                          base_curve_number / (0.427 + 0.00573 * base_curve_number)
                                        )
                                )
                        )

  return(curve_number)

}
