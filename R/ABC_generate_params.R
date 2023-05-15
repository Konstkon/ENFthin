#'
#'
#'
#' @title Simulate parameters and summaries from a model.
#' @description Creates param and sumstat vector for an abc rejection algorithm.
#' The parameter is taken from a Uniform(0,0.2) dostrobution and S1 is calculated as
#' the radius r such that the OVERALL empty space function F(r)=0.5.
#' @param{sample_num} The sample number
#' @param{normal_patterns} A  list of two-dimensional point pattern for the end points to be thinned(object of class "ppp").
#' Should have a mark "Tree".
#' @param{normal_bp_patterns} A list of two-dimensional point patterns for the base points to be thinned(object of class "ppp").
#' Should have a mark "Tree".
#' @param{point_pattern_target} A list of two-dimensional point patterns for the end points
#'(object of class "ppp"). The number of point in this pattern give the target end point in the thinned pattern.
#' Should have a mark "Tree".
#' @param{point_pattern_bp_target} A list of two-dimensional point patterns for the base points to be thinned(object of class "ppp").
#' The number of points in this pattern give the target base point in the thinned pattern.
#' Should have a mark "Tree".
#' @param{weight_bp} A scaling parameter for the probability function
#' @param{simulate_healthy} A variable deciding whether to simulate or use the empirical healthy data as a starting point
#' @param{epsilon} Parameter controlling what healthy point patterns to be thinned.
#' The rule is that if a pattern has at least target_bp+ epsilon base points then it will be thinned.
#' @param{nsim} Number of simulations
#' @param{prior_max} Upper bound of the uniform prior distribution
#' @param{N_samples} How many samples to be considered when estimating F
#'  @return A list with columns P1 and S1.
#' @export
#'
#' @examples
#'ABC_SAMP = ABC_params(nsim=15000,sample_num = 1)



ABC_params <-function(sample_num=1,
                      point_pattern_target = realepmild,
                      point_pattern_bp_target = realbpMILD,
                      normal_patterns = realep,
                      normal_bp_patterns = realbp,
                      nsim =100,
                      epsilon=5,
                      simulate_healthy=TRUE,
                      prior_max = 0.5,
                      N_samples=1,
                      fixed.pattern = FALSE,
                      simulation.params =c(0.0009, 23,3),
                      uniform.prior=FALSE){

    parameter_vector = c()
    base_statistic = c()
    for ( i in 1:nsim){
      if((i %% 1000)==0){
        print(paste(i))}

        #propose point from prior
        if(uniform.prior)s_b = runif(1,0.01,prior_max)
        if(uniform.prior==FALSE) s_b= rtrunc(1,"exp",0.01,prior_max,rate=10)
        #NUMBER OF BASE POINTS IN THE TARGET MILD SAMPLE
       # target_bp = npoints(point_pattern_bp_target[[sample_num]])

        #GET RELEVANT HEALTHY PATTERNS( all healthy patterns with at least nb+5 base points)
       # nbp = lapply(normal_bp_patterns, function(x) npoints(x))
        #index_normal = which(nbp > 5 + target_bp )
        #index_normal = index_normal[sample.int(length(index_normal),1,replace=TRUE)]
        if(fixed.pattern){
        #initial_pattern = readRDS("C:/Users/konkons/Desktop/ENF/ENFdata/Konstantinos/paper3/ABC_related/Initial_pattern")
        }
        else{
          initial_pattern = matclust_with_parents_marked(simulation.params[1],
                                                       simulation.params[2],
                                                       simulation.params[3],
                                                       point_pattern_bp_target[[sample_num]]$window)
        }
        Thinned_patterns = simulate_thinned_basepointpatterns(N_samples = 1,
                                                              simulate_healthy = simulate_healthy,
                                                              simulate_initial_pattern = initial_pattern,
                                                              weight_bp = s_b)

#       Base_simulated =  lapply(Thinned_patterns, function(x)
#          get.basepoints_ENFtrees(x))
#       IDs_considered = sample.int(length(Base_simulated),
#                                   min(length(Base_simulated),N_samples))

#res = mean(unlist(lapply(Base_simulated,function(x)
#  seq(0,150,len=300)[which(Fest(x,r=seq(0,150,len=300))$cs>0.5)[1]])))

res = Fsummary(Thinned_patterns)
            # F_tot = vapply(Base_simulated,function(x)
      #    Fest(x,r=seq(0,150,len=300))$km,
      #    rep(0,length(Base_simulated),300))



      #  F_media = unlist(lapply(c(1:length(F_tot[,1])),
      #                          function(x) mean(F_tot[x,])))

      #  res =  seq(0,150,len=300)[which( F_media >0.5)[1]]




    parameter_vector = c(parameter_vector, s_b)

    base_statistic = c(base_statistic,res)
    }#end for
    res_list = list(P1 = parameter_vector,
                    S1 = base_statistic)
    res_list
  }#end function
#}

#EXAMPLE how to use
'
ABC_SAMP = ABC_params(nsim=15000,sample_num = 1,statistic_sb = "F")
target_pcf = unlist(lapply(realbpMILD, function(x) pcf(x)$r[which(pcf(x)$trans<1)[1]]))
ABC_object = abc(target = target_pcf[1],
                          param = data.frame(param=ABC_SAMP$param),
                          sumstat = data.frame(sumstat=ABC_SAMP$summary),
                          tol=0.01,
                          method="neuralnet")
plot(density(ABC_object$adj.values))
ABC_object$adj.values

abline(h=1/0.19)
'

