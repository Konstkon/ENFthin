#'
#'
#'
#' @title Overall two-dimensional L(r) - r function
#' @description Computes the overall  L(r) - r function from a list of ppp objects.
#' The Ripley's K(r) function for each sample is weighted using square number points weights.
#' @param{list_of_patterns} A list containing two-dimensional point patterns (objects of class "ppp").
#' @return A dataframe with columns
#' \itemize{
#' \item $r:  radiuses where the summary function is evaluated and
#' \item $L:  a vector containing the values of the summary statistc for each r}
#' @export
#'
#' @examples
#' Overall_centred_Lest(realep[1:4])

Overall_centred_Lest <- function(list_of_patterns,RMAX=30){
  #Number of samples
  number_of_samples =  length(list_of_patterns)
  #samplewise K functions
  K = lapply(list_of_patterns, function(x) Kest(x,rmax=RMAX,nrval=300))

  #Number of points per sample
  N = unlist(lapply( list_of_patterns, function(x) npoints(x)))
  #Total number of points in subject
  Total_N_squared = sum(N^2)
  #compute the weights
  weights = N^2/Total_N_squared
  # Pooled K
  Pooled_K = 0
  for (num in 1:number_of_samples){
    Pooled_K = Pooled_K + weights[[num]]*K[[num]]$trans
  }
  Pooled_L = sqrt(Pooled_K/pi) - K[[1]]$r
  Pooled = data.frame ( r = K[[1]]$r, L =Pooled_L)
  Pooled
}
