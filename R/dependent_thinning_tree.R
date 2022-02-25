#'
#'
#'
#' @title Function for tree-wise thinning
#' @description This function will thin trees(all points in the tree are removed) according to the vector of probabilities
#' . The main difficulty is to find a model for the probabilities,
#' i.e what nerve trees should be more probable to be removed. For instance, we can use PROBABILITY_THINNING_()
#' to create such a vector.
#' @param{point_pattern} A two-dimensional point pattern(object of class "ppp" in spatstat).
#' Should have a mark "Tree".
#' @param{P} A vector of probabilities to retain each tree. We can get such a vector with the THINING_PROPABILITY_() function.
#' @return A thinned point pattern
#' @export
#'
#' @examples
#' dependent_thinning( marked_point_pattern , probabilities)



dependent_thinning <- function(point_pattern, P ){

  Tree = point_pattern$marks$Tree
  tree_set = unique(Tree)
  accepted =c()
  for (num in 1:length(tree_set)){
    rand = runif(1,0,1)
    if(is.na(P[num])==FALSE){
      if(rand < P[num]){
        accepted=c( accepted , tree_set[num] )
      }}
  }
  index = c()
  for( n in 1:npoints(point_pattern)){
    for( i in accepted){
      if(Tree[n]==i)
        index=c(index,n)
    }
  }
  point_pattern[index]
}



