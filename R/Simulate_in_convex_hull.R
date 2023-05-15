#simulate in convex hull# Needed packages
library(spatstat)
#> Loading required package: spatstat.data
#> Loading required package: nlme
#> Loading required package: rpart
#>
#> spatstat 1.62-2       (nickname: 'Shape-shifting lizard')
#> For an introduction to spatstat, type 'beginner'
#library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.2, PROJ 6.2.1
library(maptools)
#> Loading required package: sp
#> Checking rgeos availability: TRUE
#library(tigris)
#load sp library
library(sp)

#'
#'
#'
#' @title Suggest point in reactive territory
#' @description Simulate a uniformly random point in reactive territory
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{Treeid} The Id of the tree
#' @param{hc} The hardcore parameter
#' @return A two dimensional point pattern with one point
#' @export
#'
#' @examples
#' Simulate_in_convex_hull(realep[[1]],realbp[[1]],1)

Simulate_in_convex_hull  <- function(point_pattern,
                                     point_pattern_bp,
                                     Treeid,
                                     hc= 0.62){
  new_point =NULL
  CHULL =get.points_in_tree(point_pattern, point_pattern_bp ,Treeid)#get the points in a specific tree
  np = npoints(CHULL) # number of points in the reactive territory
  area_chull = spatstat::area(convexhull(CHULL))
  if(np>2){
    if(area_chull>30){
    cond= TRUE
    count=1
    while(cond==TRUE){
     xy_coords =  cbind(CHULL$x, CHULL$y)
    ch  = chull(xy_coords)
    xy_coords_new <- xy_coords[c(ch, ch[1]), ]
    p = Polygon(xy_coords_new)
    ps = Polygons(list(p),1)
    sps =  SpatialPolygons(list(ps))
    new_point = runifpoint(1, win = sps)
    #Create a ppp object
    new_pattern = ppp ( new_point$x, new_point$y, window =point_pattern$window)
    #Add relevant marks
    new_pattern = setmarks(new_pattern, data.frame(Tree= Treeid,type =0))
    mixed_pattern = superimpose(point_pattern, new_pattern)
    D = min (minimum_distance(mixed_pattern))
    count=count+1
    if(count >20){cond =FALSE
        }
    if(is.na(D)== FALSE){
      if( D > hc){
        cond = FALSE
      }
    }
    }
    }
    }
  new_point

}





