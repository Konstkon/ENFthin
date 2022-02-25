#'
#'
#'
#'
#' @title Counts the number clusters with one end points
#' @description Counts the number of "orphan" end points
#' @param{point_pattern}  Two-dimensional marked point pattern (object of class "ppp").
#'  The point pattern should have a "Tree" mark denoting the ID of the nerve tree.
#'
#' @return An integer representing the number of one end point trees
#' @export
#'
#' @examples
#' count_orphan_trees(realep[[1]])

count_orphan_trees <- function(point_pattern){
  Trees = point_pattern$marks$Tree
  N = npoints(point_pattern)
  count= 0
  for (i in unique(Trees)){
    tree_size = length(Trees[Trees==i])
    if(tree_size==1){count=count+1}

  }
  count
}

