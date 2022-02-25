#'
#'
#'
#' @title Probability function based on closest distances and cluster sizes
#' @description A certain probability function for thinning the end points
#'  given by probability = exp (- ( w* min_distance)^2) if size !=1 oterwise probability=1
#' , where size is the cluster size.
#' @param{point_pattern} Two-dimensional point pattern for the end points(object of class "ppp").
#' @param{weight_ep} the scale parameter w of the probability function.
#' @return A vector of probabilities
#' @export
#'
#' @examples
#' probability.retain_points(X_thin_end[[63]] )


probability.retain_points <- function(point_pattern,
                                      weight_ep = 0.045
                                      ){
  min_distance = minimum_distance(point_pattern)
  sizes = add_cluster_size_marks(point_pattern)$marks$size
  probability = c()
  count=1
  for ( i in sizes){
    if(i ==1){
      probability = c( probability , 1 )
    }
    else{
      probability = c(probability,
        exp( - weight_ep^2*min_distance[count]^2) )
    }
    count = count+1
  }
      probability
}



