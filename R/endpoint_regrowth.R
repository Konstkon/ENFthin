#'
#'
#'
#' @title Regrowth model
#' @description Add or remove end point to match a target number of points
#'
#' @param{point_pattern}  A two-dimensional point pattern (object of class "ppp").
#' Should have a mark "Tree.
#' @param{num_ep} Target number of end points
#' @param{hc} Hardcore parameter
#' @param{R} Radius parameter in the regrowth model when adding points
#' @param{weight_ep} Scaling parameter in the probabiity function.
#' @return A two-dimensional point pattern
#' @export
#'
#' @examples
#' X = endpoint_regrowth(X_thin_end[[90]], num_ep = 40)
#' X_regrowth = lapply (X_thin_end, function(x) endpoint_regrowth(x,num_ep = 40))

endpoint_regrowth <- function( point_pattern,
                               num_ep,
                               hc = 1,
                               R=20,
                               weight_ep=0.045 ){

  N = npoints(point_pattern)# number of points in point pattern

  N_add = num_ep - N  #difference to optimal

  Trees = unique(point_pattern$marks$Tree) #Available trees
  num_trees = length(Trees) #number of trees
  Window_point_pattern = window(point_pattern) #Window
  while ( N_add!= 0){
    get_cluster_size(point_pattern)

    if(N_add > 0 ){  #We add points
      #choose  tree (at random)

      Add_to_tree = Trees [sample.int( num_trees , 1)]
      #point_pattern[ sample.int( N , 1)]$marks$Tree

      # Create a new ppp with one point
      #TRUE FOR POINTS THAT HAVE TREE MARK Add_to_tree
      isin.Add_to_tree = point_pattern$marks$Tree == Add_to_tree
      #sugget new point
      NP = suggest_new_point(point_pattern, isin.Add_to_tree,hc = hc,R=R)
      if(length(NP)>0){
        #Add point to the pattern
        one_point_ppp = ppp ( NP$X, NP$Y, window = point_pattern$window)
        #Add relevant marks
        one_point_ppp = setmarks(one_point_ppp, data.frame(Tree= Add_to_tree, type =0))
        #Superimpose the ppp objects
        point_pattern = superimpose(point_pattern, one_point_ppp)



        #Update cluster size marks
        #point_pattern = add_cluster_size_marks(point_pattern)

        N = npoints(point_pattern)
        N_add = num_ep - N} #Update N_Add
    }

    if(N_add < 0 ){#We remove points
      #Update cluster size marks
      point_pattern = add_cluster_size_marks(point_pattern)

      #Probability function
      #depends on endpoints minimum distance but
      #we dont remove trees i.e if size is 1 P=1
      point_pattern$marks$PROBS = probability.retain_points(point_pattern,weight_ep = weight_ep)

      #remove one point according to mark PROB
      point_pattern = thin_with_probability(point_pattern)

      N_add = N_add + 1 #Updata N_add
    }


  }
  point_pattern

}
