#'
#'
#'
#' @title Updates the base point patterns.
#' @description After thinning of the end point patterns this function thins the correspondin base point patterns.
#' This connection is done using the "Tree" mark in the two patterns.
#' @param{point_pattern}  Two-dimensional marked point pattern for the end point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern for the base point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @return The corresponding base point patterns after thinning the end points
#' @export
#'
#' @examples
#' get.basepoints_thinned(X_thin_end[[1]], realbp[[1]])



get.basepoints_thinned<- function(point_pattern, point_pattern_bp ){
  #GET TREE IDS OF THE REMAINING ENDPOINTS
  marks_ep =  unique(point_pattern$marks$Tree)

  #GET THE CORRESPONDING BASEPOINTS
  depe_bp = point_pattern_bp[match(marks_ep, point_pattern_bp$marks$Tree)]

  depe_bp
}



