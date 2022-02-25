#'
#'
#'
#' @title Overall three-dimensional L(r) - r function
#' @description Computes the overall  L(r) - r function from a list of pp3 objects.
#' The Ripley's K(r) function for each sample is weighted using square number points weights.
#' @param{list_of_patterns} A list containing three-dimensional point patterns (objects of class "pp3").
#' @param{RMAX} Maximum value for the radiuses. The function will be evaluated  in (0, RMAX ) at 100 different radius values.
#' @return A dataframe with columns
#' \itemize{
#' \item $r:  radiuses where the summary function is evaluated and
#' \item $L:  a vector containing the values of the summary statistc for each r}
#' @export
#'
#' @examples
#' Overall_centred_L3est(realep3D)

Overall_centred_L3est<- function(list_of_patterns,RMAX=30){
  #Number of samples
  number_of_samples =  length(list_of_patterns)
  #samplewise K functions
  K = lapply(list_of_patterns, function(x) K3est(x,rmax = RMAX,nrval = 300))

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
  Pooled_L = (Pooled_K/(4/3*pi))^(0.333)-K[[1]]$r
  Pooled = data.frame ( r = K[[1]]$r, L =Pooled_L)
  Pooled
}

