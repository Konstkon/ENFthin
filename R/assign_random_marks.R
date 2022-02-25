#'
#'
#'
#' @title Assigns random marks to a pattern
#' @description This function assigns random marks to a pattern. Mainly used in the Ind_thin() function to randomly thin a pattern to a fixed number of points
#' @param{point_pattern}  Two-dimensional point pattern (object of class "ppp").
#' @return Two-dimensional marked point pattern (object of class "ppp"). The marks "point_id" will be added to the pattern.
#' @export
#'
#' @examples
#' assign_random_marks(realep[[1]])



assign_random_marks  <- function(point_pattern){

  N = npoints(point_pattern)
  m = 1:N
  point_pattern$marks$point_id = sample.int(N)
  point_pattern
}

