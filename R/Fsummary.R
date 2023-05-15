#'
#'
#'
#'@title Simulates summary function from a point pattern realisation
#'@description Computes a summary function for the ABC algorithm. It returns the radius
#' where F of the basepoint patterns is more than value.
#'@param{x} A list containing an ENFtrees object
#'@return The radius where F >value for the first time
#'@export
#'@examples
#'Summary =Fsummary(Thinned_basepoints = simulate_thinned_basepointpatterns(N_samples = 1,simulate_healthy = TRUE,weight_bp = s_b))



Fsummary<- function(x,value =0.3){
  r = seq(0,120,len=sample(300:1000,1))
  F_base = Fest(x[[1]]@Basepoints,r=r)

  res = F_base$r[which(F_base$cs>value)[1]]
  if(is.na(res)) return(0) else return(res)
}
