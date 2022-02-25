#'
#'
#'
#' @title Function for random tree-wise thinning
#' @description This function will thin trees(all points in the tree are removed) according to a fixed probability prob.
#' @param{point_pattern} A two-dimensional point pattern(object of class "ppp" in spatstat).
#' Should have a mark "Tree".
#' @param{prob} A real number giving the fixed probability
#' @return A thinned point pattern
#' @export
#'
#' @examples
#' random_tree_thinning( marked_point_pattern , prob)
random_tree_thinning <- function(ppp,
                                 prob){

  Tree = ppp$marks$Tree
  tree_set = unique(Tree)

  accepted =c()
  for (num in tree_set){
    rand = runif(1,0,1)
    if(rand < prob){
      accepted=c(accepted,num)
    }
  }
  index = c()
  for( n in 1:npoints(ppp)){
    for( i in accepted){
      if(Tree[n]==i)
        index=c(index,n)
    }
  }
  ppp[index]

}
