#'
#'
#'
#' @title Simulates thinned patterns
#' @description Thins given patterns according to a parametric thinning model. First two steps of the model in the paper.
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
#' @param{simulation.params} If simulate_healthy=TRUE. The healthy data are simulated from a Matern Cluster process
#' with simulation.params (kappa,scale,mu).
#' @param{hardcorestep} Whetther to use the hardcore step or not
#' @param{N_samples} How many samples to be considered in the thinning
#' @return A (list) of ENFtrees objects.
#' @export
#'
#' @examples
#'simulate_thinned_basepointpatterns()
#'simulate_thinned_basepointpatterns(normal_bp_patterns = realbp[[66]],normal_patterns = realep[[66]])




simulate_thinned_basepointpatterns  <- function( sample_num=1,
                                       point_pattern_target = realepmild,
                                       point_pattern_bp_target =realbpMILD,
                                       normal_patterns = realep,
                                       normal_bp_patterns = realbp,
                                       weight_bp=0.1,
                                       scheme="dependent",
                                       simulate_healthy = FALSE,
                                       simulate_initial_pattern =NULL,
                                       hardcorestep = FALSE,
                                       N_samples= 30){
#Decide whether you simulate healthy or use the empirical data
  if(simulate_healthy==TRUE){
    #simulate Healthy patterns
 if(is.null(simulate_initial_pattern )) stop("Provide an initial pattern")
    #  simulated_healthy  = lapply(1: N_samples,
    #                            function(x)
    #                  matclust_with_parents_marked(simulation.params[1],
    #                                               simulation.params[2],
    #                                               simulation.params[3],
    #                                               point_pattern_bp_target[[sample_num]]$window))

    simulated_healthy = list(simulate_initial_pattern)
     #End points
     normal_patterns = lapply(simulated_healthy,
                              function(x) x[x$marks$type==0])

     #Base points
     normal_bp_patterns = lapply(simulated_healthy,
                              function(x) x[x$marks$type==1])
          }

  #If the input is a ppp instead of a list of ppp objects
  if(class(normal_patterns) =="ppp"){
    normal_patterns = list(normal_patterns)
    normal_bp_patterns = list(normal_bp_patterns)
  }

  #NUMBER OF BASE POINTS IN THE TARGET MILD SAMPLE
  target_bp =npoints(point_pattern_bp_target[[sample_num]])

  #GET RELEVANT HEALTHY PATTERNS( all healthy patterns with at least nb+5 base points)
  nbp = lapply(normal_bp_patterns, function(x) npoints(x))
  index_normal = which(nbp > 5 + target_bp )

  #Choose N_samples of those patterns
  index_n = sample.int(length(index_normal),
                       min(length(index_normal),N_samples))

  #GET CORRESPONDING HEALTHY PATTERNS
  normal_patterns    =    normal_patterns[index_normal[index_n]]
  normal_bp_patterns = normal_bp_patterns[index_normal[index_n]]

  ############################################# STEP 1 ##########################
if(hardcorestep==TRUE){
  #GET EP AND BP HARDCORE PARAMETERS
  hardcore_bp =  get_hardcore( point_pattern_bp_target[[sample_num]] )

  #APPLY THE HARDCORE

  #HARDCORE TO BP PATTERNS
  normal_bp_patterns = lapply(seq_along(normal_bp_patterns),
                              function(x) remove.points_hardcore(normal_bp_patterns[[x]],
                                                                 hardcore_bp ))
  #CORRESPONDIN EP PATTERNS
  normal_patterns = lapply(seq_along(normal_bp_patterns),
                           function(x) get.endpoints_thinned(normal_patterns[[x]],
                                                             normal_bp_patterns[[x]]))

}
  ############################################### STEP 2 ############################

  #APPLY THINNING TO BASEPOINTS

  X_thin = lapply(seq_along(normal_patterns),
                  function(x)thin_to_sample_NEW(ppp_ep = normal_patterns[[x]],
                                                ppp_bp  = normal_bp_patterns[[x]],
                                            num_ep = 40000,
                                            num_bp = target_bp,
                                            epsilon =0,
                                            weight_bp = weight_bp,
                                            scheme=scheme))




   return(X_thin)
}

