#'
#'
#'
#' @title Overlap of reactive territories
#' @description Calculates the percentage of overlap of the reactive territories. The Overlap is defined as 1 - ratio, where ratio
#' is the ratio between the area of the union of the reactive territories divided by the
#' total area covered by the reactive territories.
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#'
#' @return The overlap of reactive territories. A value between 0 and 1.
#' @export
#'
#' @examples
#' GET_OVERLAP(realep[[1]],realbp[[1]])

GET_OVERLAP <- function(point_pattern, point_pattern_bp){
  UNION_AREAS = GET_UNION_AREA(point_pattern ,point_pattern_bp)
  TOTAL_AREAS = total_area(point_pattern ,point_pattern_bp)$Total_area
  OVERLAP = 1- UNION_AREAS/TOTAL_AREAS
  OVERLAP
}

