#'
#'
#'
#' @title Simulates from matern cluster point process
#' @description Simulates the daughter and parent process from a matern cluster point process
#' @param{kappa_par} Intensity of the Poisson process of cluster centres.
#' @param{scale_par} Radius parameter of the clusters.
#' @param{mu_par} Mean number of points per cluster (a single positive number)
#' @param{window} Window in which to simulate the pattern. An object of class "owin" or something acceptable to as.owin.
#' @return A marked ppp object. This pattern contains two marks "Tree" and "type".
#' @export
#'
#' @examples
#'
#' parameters_fit =matclust.estK(realep[[66]],
#'                               startpar=c(kappa=1,scale=1))$clustpar
#' matclust_with_parents_marked(        parameters_fit[1],
#'                                     parameters_fit[2],
#'                                     3,
#'                                     realep[[66]]$window)


matclust_with_parents_marked <- function(kappa_par,
                                         scale_par,
                                         mu_par,
                                         window_){
  basepoint = rpoispp(lambda = kappa_par,
                      win = window_)

  basepoint_df = data.frame(X=basepoint$x,
                            Y = basepoint$y,
                            Tree = 1:npoints(basepoint),
                            Type = rep(1,npoints(basepoint),1))

  endpoints_x = c()
  endpoints_y = c()
  endpoints_trees = c()

  for(treeid in 1: npoints(basepoint)){
  cluster_size = rpois(lambda = mu_par,n=1)
  endpoints_trees = c(endpoints_trees,rep(treeid, 1,cluster_size))
  endpoint_ = runifpoint(cluster_size,
             disc(radius = scale_par,
                  centre  = as.numeric(basepoint_df[treeid,c("X","Y")])))

  endpoints_x = c(endpoints_x,endpoint_$x)
  endpoints_y = c(endpoints_y,endpoint_$y)
  }
  endpoints_df = data.frame (X = endpoints_x,
                             Y = endpoints_y,
                             Tree = endpoints_trees,
                             Type = rep(0,1,length(endpoints_x)))
  M_C = rbind(basepoint_df,endpoints_df)

  res = ppp(x=M_C$X,y=M_C$Y,window = window_,
            marks = data.frame(Tree=M_C$Tree,type = M_C$Type))

  return(res)

}



