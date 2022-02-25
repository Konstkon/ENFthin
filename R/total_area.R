#'
#'
#'
#' @title Computes the total area of the skin covered by the ENFs
#' @description Computes the total and average area of reactive territories given two point patterns( base and end point patterns)
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#'
#' @return A data frame with columns Total_area (total area of reactive territories in the pattern),
#' Trees( Number of nerve trees in the patterns) and area_per_tree( The average reactive territory area per tree)
#' @export
#'
#' @examples
#' total_area(realep[[1]],realbp[[1]])


#function returning a dataframe with column names
# - the total area of the reactive territories,
# - number of nerve tree and
# - area per tree

total_area <- function(point_pattern, point_pattern_bp){
  area_per_trees = area_convex_hull(point_pattern, point_pattern_bp )
  areas = sum(area_per_trees[is.na(area_per_trees)==FALSE])
  Trees = unique(point_pattern$marks$Tree)
  df = data.frame(Total_area = areas, Trees = length(Trees), area_per_tree = areas/length(Trees))
  df
}

