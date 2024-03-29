#'
#'
#'
#' @title Overall centre markcorrelation curves from patterns in a list
#' @description Provided a list of size [N] [M], where N=number of samples and M=number of simulations
#' , at each iteration a random simulated pattern from each sample is selected and the overall centred L
#' function is estimated. This procedure is repeated nsim times which produces nsim curves.  Square number
#' weights are used to weights the sample wise summary functions.
#' @param{point_pattern_list} A list of size [N] [M] containing two dimensional point patterns ("ppp" objects).
#' @param{point_pattern_list_bp} A list of size [N] [M] containing two dimensional point patterns ("ppp" objects).
#' @param{nsim}  Number of simulations. Default is nsim=2500.
#' @param{RMAX} Maximum value for the radius parameter
#' @return A matrix where each column is a pooled estimate of markedcorr function
#' from patterns that are obtained at random from the list.
#' @export
#'
#' @examples
#' get.simulated_statistics_markcorr(simulations)
#'

get.simulated_statistics_mark <- function( point_pattern_list,
                                           nsim=2500,
                                           RMAX=200){
   #loop through nsim
  for( i in 1:nsim){
    count=1
    

    #Calculate curves for each simulation
    while(count>0){
      INDEX = Random.samples(point_pattern_list,1)
      simulated_samples = Create.list.samples(point_pattern_list, INDEX[1,])
    simulated = Overall_markcorr(simulated_samples,RMAX=RMAX)
    count = sum(is.na(simulated$M))
    count
    }
    if(i==1){
      simulated_statistics = simulated$M
    }else{
      
      simulated_statistics = cbind(simulated_statistics, simulated$M)
    }
  }
  simulated_statistics
  
}

