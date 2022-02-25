#'
#'
#'
#' @title Probability function based on closest distances
#' @description A certain probability function for thinning the points (either base or end points). The functions is
#' given by probability = exp (- ( w* min_distance)^2)
#'
#' @param{point_pattern} Two-dimensional point pattern for the end points(object of class "ppp").
#' @param{point_pattern_bp} Two-dimensional point pattern for the base points(object of class "ppp").
#' @param{weight_bp} scale parameter if type = "BP". Those parameters need to be tuned.
#' @param{weight_ep} scale parameter if type = "EP". Those parameters need to be tuned.
#' @param{minimim} The percentile of the distance to consider. For example, minimum=0 corresponds to the min distance, minimum =0.1 is the 10% percentile and so on.
#' Default value is minimum = 0 which returns the closest distance
#' @param{type} This argument define which pattern should be thinned
#' \itemize{
#' \item "BP" : Base points
#' \item "EP" : End points
#' }
#' @param{use_square} A parameter that defines whether the probability function will be gaussian of exponential.
#'  Default = TRUE which gives gaussian like probability
#' @return A vector of probabilities
#' @export
#'
#' @examples
#' PROBABILITY_THINNING_(realep[[1]],realbp[[1]],weight_bp = 0.05)


PROBABILITY_THINNING_ <- function(point_pattern,
                                  point_pattern_bp,
                                  weight_bp = 0.013,
                                  weight_ep = 0.045,
                                  minimum = 0,
                                  type ="BP",
                                  use_square = TRUE){
  if(use_square==TRUE){
  if(type=="BP"){
    min_distance = minimum_distance(point_pattern_bp,minimum)
        probability = exp( -  weight_bp^2 * min_distance^2)
  }
  if(type=="EP"){
    min_distance = minimum_distance(point_pattern,minimum)
    probability = exp( -  weight_ep^2 * min_distance^2)
  }}
  else{
    if(type=="BP"){
      min_distance = minimum_distance(point_pattern_bp,minimum)
      probability = exp( -  weight_bp * min_distance)
    }
    if(type=="EP"){
      min_distance = minimum_distance(point_pattern,minimum)
      probability = exp( -  weight_ep * min_distance)
    }
  }
  probability
}

