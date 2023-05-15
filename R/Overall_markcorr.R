#'
#'
#'
#' @title Overall mark correlation function
#' @description Computes the overall mark correlation function from a list of ppp objects.
#' The mark correlation function for each sample is weighted using square number points weights.
#' @param{list_of_patterns} A list containing ENFtrees or ENFdata objects.
#' @return A dataframe with columns
#' \itemize{
#' \item $r:  radiuses where the summary function is evaluated and
#' \item $M:  a vector containing the values of the summary statistc for each r}
#' @export
#'
#' @examples
#' Overall_markcorr(realbpmild_new)

Overall_markcorr <- function(list_of_patterns,
                             RMAX=55){
  #Number of samples
  number_of_samples =  length(list_of_patterns)
  #samplewise K functions
  K = lapply(list_of_patterns, function(x) 
    markcorr(x,r=seq(0,RMAX,len=200))$Area)

  #Number of points per sample
  N = unlist(lapply( list_of_patterns, function(x)npoints(x)))
  #Total number of points in subject
  Total_N_squared = sum(N^2)
  #compute the weights
  weights = N^2/Total_N_squared
  # Pooled K
  Pooled_K = 0
  for (num in 1:number_of_samples){
    Pooled_K = Pooled_K + weights[[num]]*K[[num]]$trans
  }
  #Pooled_L = sqrt(Pooled_K/pi) - K[[1]]$r
  Pooled = data.frame ( r = K[[1]]$r, M =Pooled_K)
  Pooled
}
