#'
#'
#'
#' @title Returns a ppp object containing the base point and end points of a specific nerve tree.
#' @description Creates a list with $x~coordinates $bbox~bounding box to be used in the Kest_directional function
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{treeid}  The id of the tree
#' @return A Multi-type point pattern with points in the "treeid" Tree.
#' @export
#'
#' @examples
#' get.points_in_tree ( realep[[1]], realbp[[1]], 1)

get.points_in_tree<- function(point_pattern,point_pattern_bp, treeid ){
  N = npoints(point_pattern)
  index = c()
  Tree= point_pattern$marks$Tree
  #GET POINT INDICES WITHIN A NERVE TREE
  for( n in 1:N){
    if(Tree[n]== treeid)
      index=c(index,n)
  }
  #FIND THE CORRECT POINTS AND CREATE A MULTITYPE POINT PATTERNS
  bp_ppp = point_pattern_bp[point_pattern_bp$marks$Tree== treeid]
  ep_ppp = point_pattern[index]
  point_final = superimpose(bp_ppp, ep_ppp)
  point_final
}


