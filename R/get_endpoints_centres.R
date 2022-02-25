#'
#'
#'
#' @title Create the point pattern of the end point cluster centres
#' @description Computes the coordinates of the end points cluster centres and then creates a ppp object where each point represent an end point cluster centre.
#' @param{point_pattern}  Two-dimensional marked point pattern for the end point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @return A two-dimensional point pattern for the centre of the end point clusters.
#' @export
#'
#' @examples
#' Get_endpoint.centres(realep[[1]])

Get_endpoint.centres<- function(point_pattern){
  Endpoints = point_pattern
  Trees = unique(Endpoints$marks$Tree)
  xcoord = c( )
  ycoord = c( )
  for (t in Trees){
    Trees_in_tree = Endpoints[Endpoints$marks$Tree == t]
    xcoord = c(xcoord ,
              mean ( Trees_in_tree$x ))
    ycoord = c(ycoord ,
              mean ( Trees_in_tree$y ))
  }
  point_pattern = ppp(xcoord,
                      ycoord,
                      window = Endpoints$window )
  setmarks(point_pattern,
           data.frame(Tree=Trees,
                      type = 3*ones(length(Trees),1))
           )

 point_pattern
}

