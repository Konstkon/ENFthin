#loads raster library
library(raster)

#'
#'
#'
#' @title Area of the union of reactive territories
#' @description Calculates the area of the union of reactive territories
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#'
#' @return The areas of the union of the reactive territories (the convex hull of the nerve trees including both end and base points)
#' @export
#'
#' @examples
#' GET_UNION_AREA(realep[[1]],realbp[[1]])


GET_UNION_AREA <- function(point_pattern,point_pattern_bp){

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
  xy_coords <- lapply(REACTIVES_POSITIVE_AREA , function(x)cbind(x$x, x$y))
  ch = lapply(xy_coords, function(x)chull(x))
  xy_coords_new <- lapply(seq_along(xy_coords), function(x) xy_coords[[x]][c(ch[[x]], ch[[x]][1]), ])
  p = lapply(xy_coords_new, function(x)Polygon(x))
  ps = lapply(p, function(x) Polygons(list(x),1))
  sps = lapply(ps, function(x) SpatialPolygons(list(x)))

  if(length(sps)>=2){
    UNION_SHAPE = union(sps[[1]],sps[[2]])
    if(length(sps)>2){
      for (i in 3:length(sps)){
        UNION_SHAPE = union(UNION_SHAPE,sps[[i]])
      }}}
  UNION_AREA = sum(unlist(lapply(seq_along(UNION_SHAPE), function(x) UNION_SHAPE@polygons[[x]]@area)))
  UNION_AREA
}
