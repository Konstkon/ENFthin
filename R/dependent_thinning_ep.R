#'
#'
#'
#' @title Function for point-wise thinning
#' @description This function will thin every point according to the vector of probabilities
#' . The main difficulty is to find a model for the probabilities,
#' i.e what points should be more probable to be removed. For instance, we can use PROBABILITY_THINNING_()
#' to create such a vector.
#' @param{point_pattern} A two-dimensional point pattern(object of class "ppp" in spatstat)
#' @param{P} A vector of probabilities. We can get such a vector with the THINING_PROPABILITY_() function.
#' @return A thinned point pattern
#' @export
#'
#' @examples
#' dependent_thinning_ep( point_pattern , probabilities)



dependent_thinning_ep <- function(point_pattern, P ){

  N = npoints(point_pattern)
  accepted =c()
  for (num in 1:N){
    rand = runif(1,0,1)
    if(rand < P[num]){
      accepted=c( accepted , num )
    }
  }
  point_pattern[accepted]
}

