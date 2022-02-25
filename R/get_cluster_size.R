library(ggplot2)
library(dplyr)
library(hrbrthemes)
#'
#'
#'
#' @title Computes the size of each cluster.
#' @description Computes a vector of cluster sizes (number of points per tree) for each nerve tree from a two-dimensional marked point
#' pattern that should have a mark "Tree"
#' @param{point_pattern}  Two-dimensional marked point pattern (object of class "ppp").
#' Should contain a mark "Tree".
#' @return A vector of the cluster sizes
#' @export
#'
#' @examples
#' get_cluster_size(realep[[1]])


get_cluster_size <- function(point_pattern){
  tree_ids = unique(point_pattern$marks$Tree)
  sizes = c()
  for (i in tree_ids){
    sizes = c(sizes , length(point_pattern$x [point_pattern$marks$Tree==i]  ) )
  }
  sizes
}
