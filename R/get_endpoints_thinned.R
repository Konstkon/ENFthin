#'
#'
#'
#' @title Updates the end point patterns.
#' @description After thinning of the base point patterns this function thins the correspondin end point patterns.
#' This connection is done using the "Tree" mark in the two patterns.
#' @param{point_pattern}  Two-dimensional marked point pattern for the end point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern for the base point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @return The corresponding end point patterns after thinning the base points
#' @export
#'
#' @examples
#' get.basepoints_thinned(realep[[1]], X_thin_base[[1]])



get.endpoints_thinned <- function(point_pattern, point_pattern_bp){
  #GET TREE IDS OF THE BASEPOINTS
  marks_thinned_ppp_bp = unique(point_pattern_bp$marks$Tree)

  #UPDATE CORRESPONDING ENDPOINTS patterns

  point_pattern = point_pattern[ which(is.na(match( point_pattern$marks$Tree,marks_thinned_ppp_bp))==FALSE)]
  point_pattern
}

