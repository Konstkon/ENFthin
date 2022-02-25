#'
#'
#'
#' @title Calculates non-spatial statistics
#' @description Calculates non-spatial statistics from the patterns.
#' @param{point_pattern} A list of two-dimensional point patterns for the end points(object of class "ppp").
#' Should have a mark "Tree".
#' @param{point_pattern_bp} A list of two-dimensional point patterns for the base points(object of class "ppp").
#' Should have a mark "Tree".
#' @return A dataframe with columns
#' \itemize{
#' \item $num_ep : Number of end points
#' \item $area_per_tree : Average area of reactive territory
#' \item $trees_per_sample :  Number of nerve trees
#' \item $points_per_cluster : Number of end points per nerve tree
#' \item $orphan_trees : Number of clusters with one end point
#' }
#' @export
#'
#' @examples
#' overall_statistics(point_pattern = realep,
#'                    point_pattern_bp = realbp)




overall_statistics <- function(point_pattern,
                               point_pattern_bp){

  if(class(point_pattern)=="ppp"){
    point_pattern = list(point_pattern)
    point_pattern_bp = list(point_pattern_bp)
  }
  area_per_trees = unlist(lapply(seq_along(1:length(point_pattern)),
                                 function(x) total_area(point_pattern[[x]],
                                                        point_pattern_bp [[x]])$area_per_tree))

  mean_area_per_tree <- area_per_trees[is.na(area_per_trees)==FALSE]


  num_ep <-unlist(lapply(point_pattern,function(x) npoints(x)))
  trees_per_sample <- unlist(lapply(point_pattern , function(x) length(unique(x$marks$Tree))))

  num_orhan_trees <-unlist(lapply(point_pattern, function(x)count_orphan_trees(x)))


  df= data.frame(num_ep = num_ep ,
                 area_per_tree = mean_area_per_tree,
                 trees_per_sample = trees_per_sample,
                 points_per_cluster= num_ep/trees_per_sample,
                 orphan_trees = num_orhan_trees)

}


