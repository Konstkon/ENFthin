#'
#'
#'
#' @title Cluster centres
#' @description Calculates the cluster centre of each cluster
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @return A two dimensional point pattern
#' @export
#'
#' @examples
#' get_cluster_centres(realep[[1]])

get_cluster_centres <- function(point_pattern
                                ){
  Trees = unique(point_pattern$marks$Tree)
  mid_point_coords_x  = c()
  mid_point_coords_y = c()
  for( t in Trees){
    isin.Tree =  point_pattern$marks$Tree == t
    point_pattern_tree = point_pattern[isin.Tree]
   mid_point_coords_x = c( mid_point_coords_x, mean(point_pattern_tree$x ))
   mid_point_coords_y = c( mid_point_coords_y, mean(point_pattern_tree$y))
  }                      
  
result = ppp(mid_point_coords_x ,
             mid_point_coords_y, window   = point_pattern$window) 
result = setmarks(result,
                  data.frame(Tree=Trees,
                             type = 4*rep(1,length(Trees))))
result
}

