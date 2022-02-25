#'
#'
#'
#' @title Adds a cluster size mark to a point pattern.
#' @description Adds a size (cluster size) mark to each point in the pattern. This function uses the get_cluster_size() function to determine the cluster sizes.
#' @param{point_pattern}  Two-dimensional marked point pattern (object of class "ppp").
#' Should contain a mark "Tree".
#' @return  A Two-dimensional marked point pattern(object of class "ppp") with mark "size" (the number of points in each cluster)
#' @export
#'
#' @examples
#' add_cluster_size_marks(realep[[1]])


add_cluster_size_marks <- function(point_pattern){
  sizes = get_cluster_size(point_pattern)
  tree_ids = unique(point_pattern$marks$Tree)
  new_marks = c()

  for( j in 1:length(tree_ids)){
    m = sizes[j]*rep(1,sizes[j])
    new_marks = c(new_marks,  m)
  }

  point_pattern$marks$size = new_marks
  point_pattern

}


