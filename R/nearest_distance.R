#'
#'
#'
#' @title  Closest distance between the points in a point pattern
#' @description For each point in the point pattern it computes the distances to the other points and returns the "minimum" percentile of those distances. If
#' minimum = 0 the minimum distance is returned. This function is mainly used to create distance based marks and thin the patterns according to the values of those marks.
#' @param{point_pattern}  Two-dimensional point pattern (object of class "ppp").
#' @param{minimum} The percentile of the distance to consider. For example, minimum=0 corresponds to the min distance, minimum =0.1 is the 10% percentile and so on.
#' Default value is minimum = 0 which returns the closest distance
#' @return A vector with the minimum distances
#'@export
#'
#' @examples
#' minimum_distance(rpoispp(10))


minimum_distance  <- function(point_pattern,minimum=0){

  D = pairdist(point_pattern)
  n = npoints(point_pattern)
  if(minimum==0){
    smaller_dist <- unlist(lapply(seq_along(1:n),
                                  function(x) sort(D[x,], decreasing = FALSE)[2]))
  }
  if(minimum!=0){
    smaller_dist <- unlist(lapply(seq_along(1:n),
                                  function(x) sort(D[x,], decreasing = FALSE)[ceil(n*minimum)]))
  }

  smaller_dist
}


