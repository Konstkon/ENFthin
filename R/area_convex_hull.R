#load sp library
library(sp)

#'
#'
#'
#' @title Areas of reactive territories
#' @description Computes the areas of reactive territories given two point patterns( base and end point patterns)
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#'
#' @return The areas of reactive territories (the convex hull of the nerve trees including both end and base points)
#' @export
#'
#' @examples
#' area_convex_hull(realep[[1]], realbp[[1]])



area_convex_hull  <- function(point_pattern, point_pattern_bp){
  Trees = unique(point_pattern$marks$Tree)
  area = c()
  CHULL=list()
  count=1
  for (l in Trees){
    CHULL[[count]] = get.points_in_tree(point_pattern ,point_pattern_bp , l)
    count=count+1
  }
  np = unlist(lapply(CHULL, function(x)npoints(x)))
  POSITIVE_AREA_ID = which(np>2)
  REACTIVES_POSITIVE_AREA = CHULL[POSITIVE_AREA_ID]
  #GET AREA OF REACTIVE TERRITORIES
  xy_coords <- lapply(REACTIVES_POSITIVE_AREA , function(x)cbind(x$x, x$y))
  ch = lapply(xy_coords, function(x)chull(x))
  #ORDER THE COORDINATES
  xy_coords_new <- lapply(seq_along(xy_coords),
                          function(x) xy_coords[[x]][c(ch[[x]], ch[[x]][1]), ])
  p = lapply(xy_coords_new, function(x)Polygon(x))
  ps = lapply(p, function(x) Polygons(list(x),1))
  sps = lapply(ps, function(x) SpatialPolygons(list(x)))
  for (i in 1:length(sps)){
    area = c(area,sps[[i]]@polygons[[1]]@area)
  }
  area

}





