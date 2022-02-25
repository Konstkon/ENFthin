#'Simulates uniform points in the disc with centre and Radius r
#'
#'
#' @title Suggests a new point in a disc
#' @description Suggest a new point in the disc with radius R and centre estimated as the mean of the end points
#' in a specific tree. The point is accepted if it respects the hardcore parameter hc, i.e. if it is at least hc apart from the other points.
#' @param{point_pattern} Two-dimensional point pattern for the end points(object of class "ppp").
#' @param{isin.Tree} A boolean vector. If true it means that the point is in a specific tree.
#' @param{hc} the hardcore parameter hc > 0. The point are accepted only if they respect the hardcore
#' @param{r} the radius of the disc
#' @return A two-dimensional point pattern with the suggested point
#' @export
#'
#' @examples
#' suggest_new_point(point_pattern, isin.Add_to_tree,hc = hc,R=R)
#'

#Function that suggest a new_point
suggest_new_point<- function(point_pattern,
                             isin.Tree,
                             R = 20,
                             hc= 0.62){
  #get_mid_point and tree
  point_pattern_tree = point_pattern[isin.Tree]
  Tree = unique(point_pattern_tree$marks$Tree)
  mid_point_coords = c( mean(point_pattern_tree$x ),
                        mean(point_pattern_tree$y ))

  cond= TRUE
  count=1
  while(cond==TRUE){
    #Simulate random points
    new_point = simulate_uniformly_in_disc( 1 , mid_point_coords , R)
    #Create a ppp object
    new_pattern = ppp ( new_point$X, new_point$Y, window =point_pattern$window)
    #Add relevant marks
    new_pattern = setmarks(new_pattern, data.frame(Tree= Tree,type =0))
    #Superimpose the ppp objects
    mixed_pattern = superimpose(point_pattern, new_pattern)
    #Get min distance
    D = min (minimum_distance(mixed_pattern))
    count=count+1
    if(count >20){return()}
    if(is.na(D)== FALSE){
      if( D > hc){
        cond = FALSE
      }
    }
  }#end while
  new_point
}

