#'
#'
#'
#' @title Apply the hardcore radius to a pattern
#' @description This function remove all points in a point pattern that do not respect the hardcore i.e.
#' If two points are closer than distance h apart one of them is removed.
#' @param{point_pattern}  Two-dimensional point pattern (object of class "ppp").
#' @param{h} The hardcore parameter h > 0.
#' @return A two-dimensional points pattern (object of class "ppp") with minimum distance between the points larger than the hardcore parameter h.
#' @export
#'
#' @examples
#' remove.points_hardcore(rpoispp(10),0.2)

remove.points_hardcore <- function(point_pattern, h){

  distances = minimum_distance(point_pattern )

  while( min(distances) < h){
    problematic = which(distances < h)
    point_pattern = point_pattern[-problematic[1]]
    distances = minimum_distance(point_pattern)
  }

  point_pattern
}

