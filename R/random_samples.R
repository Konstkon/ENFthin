#'
#'
#'
#' @title Chooses samples at random
#' @description From a list of size [N][M] create a matrix of size (N) x (nsim) containing random indices between 1 and M.
#' @export.
#' @param{list_of_ENFtrees}  list of size [N][M]
#' @param{nsim}  Number of indices to simulate. Default value is nsim = 2500.
#' @return A (N) x (nsim) matrix containing random indices between 1 and M.
#' @export
#'
#' @examples
#' Random.samples(simulations)



Random.samples <- function(list_of_ENFtrees, nsim = 2500){
  #Number of samples
  number_of_samples = length( list_of_ENFtrees )

  #Number of patterns per sample
  number_of_simulations = unlist( lapply( seq_along(1:number_of_samples),
                                          function(x) length( list_of_ENFtrees[[x]] ) ) )

  #Create an index matrix and return it
  for ( i in 1: number_of_samples){
    if(i==1){
      Index.matrix = sample.int(number_of_simulations[[1]], nsim , replace=TRUE)
    }
    else{
      Index.matrix = cbind( Index.matrix ,sample.int(number_of_simulations[[i]], nsim , replace=TRUE) )
    }

  }
  Index.matrix
}


