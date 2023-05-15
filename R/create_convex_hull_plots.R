
#'
#'
#'
#' @title Plots reactive territories
#' @description PLOTS THE CONVEX HULL OF THE BASE AND END POINT PATTERNS
#' @param{point_pattern}  Two-dimensional marked point pattern of the end points (object of class "ppp").
#' Should contain a mark "Tree".
#' @param{point_pattern_bp}  Two-dimensional marked point pattern of the base points (object of class "ppp").
#' Should contain a mark "Tree".
#'
#' @return Plots the reactive territories (the convex hull of the nerve trees including both end and base points)
#' @export
#'
#' @examples
#' create_convex_hull_plot(realep[[1]],realbp[[1]])

create_convex_hull_plot <- function(point_pattern, point_pattern_bp){
  plot(point_pattern$x,
       point_pattern$y,
       main ="",
       cex=1,
       pch = 19,
       col="blue",
       xlab="",
       ylab="",
       ylim=c(-330,0),
       xlim=c(0,430),
       axes=FALSE,
       frame=TRUE,
       cex.axis=1)
  axis(1,c(0,100,200,300,400),tck=-0.05,cex.axis=1,padj = 1)
  axis(2,c(-300,-200,-100,0),tck=-0.05,,cex.axis=1,padj = -1)
  plot(unmark(point_pattern_bp),pch=19,cex=1,
       col="red",add=TRUE)

  #plot(unmark(point_pattern),main ="")
  #plot(unmark(point_pattern_bp),chars=2,add=TRUE)
  Trees = unique(point_pattern$marks$Tree)
  for (l in Trees){

    CHULL = get.points_in_tree(point_pattern ,point_pattern_bp , l)
    np = npoints(CHULL)
    if(np >2){
      plot(convexhull(CHULL),col="lightblue", add=TRUE)
    }
  }
  legend('topleft',c('Basepoints','Endpoints'),
         col=c("red","blue"),pch=c(19,19),cex = 1)

}

