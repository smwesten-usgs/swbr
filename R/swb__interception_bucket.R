#' Calculate potential bucket interception value.
#'
#' @param is_growing_season Logical indicating growing season status (TRUE/FALSE).
#' @param growing_season_intcp Maximum growing season interception value (inches).
#' @param non_growing_season_intcp Maximum non-growing season interception value (inches).
#' @param gross_rainfall Amount of daily precipitation that falls as rainfall (inches).
#'
#' @return Maximum interception value for given growing season status.
#' @export
#'
#' @examples
calc_potential_interception_bucket <- function(is_growing_season,
                                               growing_season_intcp,
                                               non_growing_season_intcp) {

    potential_interception <- ifelse(is_growing_season,
                                   growing_season_intcp,
                                   non_growing_season_intcp)

  return( potential_interception )
}
