#'
#'
#'
#'@title Simulates summary function from a point pattern realisation
#'@description Computes a summary function for the ABC algorithm. It returns the radius
#' where pcf of the basepoint patterns is less than 2. If the radius is less than 5 it returns 0
#'@param{x} A list containing an ENFtrees object
#'@return The radius where pcf <2 for the first time
#'@export
#'@examples
#'Summary = pcfsummary(Thinned_basepoints = simulate_thinned_basepointpatterns(N_samples = 1,simulate_healthy = TRUE,weight_bp = s_b))



pcfsummary<- function(x,value =1.2, r_min = 5){
  pcf_base = pcf(x[[1]]@Basepoints)

  res = pcf_base$r[which(pcf_base$trans<value)][1]
  if(is.na(res)) return(0)
  if(res < r_min) return(0) else return(res)
  }
