#'
#'
#'
#' @title Estimates the hardcore from a point pattern
#' @description Estimates the hardcore parameter h from a point pattern. The estimate is given by h = (N-1) / N * d_min
#' with N being the number of points in the pattern.
#' @param{point_pattern}  Two-dimensional point pattern (object of class "ppp").
#' @return The estimated hardcore parameter h > 0.
#' @export
#'
#' @examples
#' get_hardcore(rpoisppp(10))

get_hardcore <- function(point_pattern){
  N = npoints(point_pattern)
  res = (N-1)/N*min(minimum_distance(point_pattern ))
  res
}

