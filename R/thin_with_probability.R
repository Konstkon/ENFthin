#'
#'
#'
#' @title Mark based thinning
#' @description Thins a point pattern according to a "PROBS" marks.
#' @param{point_pattern} Two-dimensional point pattern (object of class "ppp"). Should have a mark "PROBS"
#' @return The thinned point pattern (object of class "ppp").
#' @export
#'
#' @examples
#' thin_with_probability(marked_point_pattern)

thin_with_probability <- function(point_pattern){
  N = npoints(point_pattern)
  cond = TRUE
  while(cond==TRUE){

  index_random = sample.int(N,1)
  U =runif(1,0,1)

  if(point_pattern[index_random]$marks$PROBS < U){
    point_pattern = point_pattern[-index_random]
    cond=FALSE
  }#end if

  }#end while
  point_pattern
}


