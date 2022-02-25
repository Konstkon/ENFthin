#'
#'
#'
#' @title List to matrix
#' @description Transforms a list of size [N] with m elements to a matrix of the same size.
#' @param{list_} A list of size [N] with each column consisting of m elements
#'  @return A matrix
#' @export
#'
#' @examples
#' list_of_numbers = list(runif(100,0,1),rnorm(100,0,1))
#' from_list_to_matrix(list_of_numbers)

from_list_to_matrix<- function(list_){
  N = length(list_)
  m = length(list_[[1]])
  res = matrix(nrow=m,ncol=N)
  for (i in 1:N){

      res[,i] = list_[[i]]

  }
  res
}

