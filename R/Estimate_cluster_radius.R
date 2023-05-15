#'
#'
#'
#' @title Estimates the cluster radius
#' @description Estimates the cluster radius as the maximum distance from an endpoint to the centre of the cluster
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @return A two dimensional point pattern
#' @export
#'
#' @examples
#' Estimate_cluster_radius(realep[[1]])
Estimate_cluster_radius <- function(point_pattern){
  
  centres_pattern = get_cluster_centres(point_pattern)
  Trees = unique(point_pattern$marks$Tree)
  distances = c()
  for (t in Trees){
    isin.Tree =  point_pattern$marks$Tree == t
    point_pattern_tree = point_pattern[isin.Tree]
    centre_pattern_tree = centres_pattern[which(centres_pattern$marks$Tree==t)]
    distances = c(distances,
                  max(pairdist(superimpose(centre_pattern_tree,point_pattern_tree))[,1]))
    
  }
max_dist = max(distances)
max_dist
#distances
    }
