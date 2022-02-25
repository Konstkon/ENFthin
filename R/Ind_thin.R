#'
#'
#'
#' @title Independent random thinning of a point pattern to a fixed number of points
#' @description Removes points from a pattern until the pattern consists of "num" points
#' @param{point_pattern}  Two-dimensional marked point pattern (object of class "ppp").
#' Should contain a mark "point_id". The assign_random_marks() function can be used to create such a mark
#' for a general ppp object.
#' @param{num} The desired number of points in the pattern
#' @return Two-dimensional thinned point pattern (object of class "ppp").
#' @export
#'
#' @examples
#' ind_thin(realep[[1]],10)
#

ind_thin <- function(point_pattern, num){
  pattern_new = assign_id_to_pattern(point_pattern)
  pattern_new = pattern_new [ pattern_new$marks$point_id <=num]
  pattern_new = setmarks(pattern_new,
                         data.frame(Tree = pattern_new$marks$Tree,
                                    type = pattern_new$marks$type
                         ))
  pattern_new
}

