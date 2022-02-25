#'
#'
#'
#' @title Creates a list of samples
#' @description Creates a list of samples according to a vector of indeces and a list of size [number of samples][number of simulations] of ppp objects
#' @param{list_of_samples}  a list of list of two-dimensional point patterns(objects of class "ppp")
#' @param{Index_row}  a vector of indices
#' @return A list of two-dimensional point patterns
#' @export
#'
#' @examples
#' Create.list.samples(simulations, Random.samples(simulations))


Create.list.samples <- function( list_of_samples, Index_row){

  #Number of samples
  number_of_samples = length( list_of_samples )

  list.samples = list()
  #Create a list of relevant samples

  for (num in 1:number_of_samples){
    #
    index = Index_row[num]
    list.samples[[num]] = list_of_samples[[num]] [[ index]]
  }
  list.samples
}
