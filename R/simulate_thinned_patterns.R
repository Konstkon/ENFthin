#'
#'
#'
#' @title Simulates thinned patterns
#' @description Thins given patterns to a target number of base and end points. We perform the three step thinning procedure described in the paper.
#' This is done is performed with thin_to_sample() and endpoint_regrowth() functions.
#' @param{sample_num} The sample number
#' @param{point_pattern} A list of two-dimensional point patterns for the end points to be thinned(object of class "ppp").
#' Should have a mark "Tree".
#' @param{point_pattern_bp} A list of two-dimensional point patterns for the base points to be thinned(object of class "ppp").
#' Should have a mark "Tree".
#' @param{point_pattern_target} A list of two-dimensional point patterns for the end points
#'(object of class "ppp"). The number of point in this pattern give the target end point in the thinned pattern.
#' Should have a mark "Tree".
#' @param{point_pattern_bp_targe} A list of two-dimensional point patterns for the base points to be thinned(object of class "ppp").
#' The number of points in this pattern give the target base point in the thinned pattern.
#' Should have a mark "Tree".
#' @param{weight_bp} A scaling parameter for the probability function
#' @param{scheme} A string determining the type of thinning to be applied. Either "dependent" or "random".
#' @param{regrowth} A boolean variable. If it is true a regrowth model(add or remove end points to match the target points) will be used
#' @param{R} The radius parameter of the regrowth model when we should add end points
#' @return A list of ENFtrees objects.
#' @export
#'
#' @examples
#'simulate_thinned_patterns(sample_num = 1,scheme="dependent")




simulate_thinned_patterns  <- function(sample_num,
                                       point_pattern_target = realepmild,
                                       point_pattern_bp_target = realbpMILD,
                                       normal_patterns = realep,
                                       normal_bp_patterns = realbp,
                                       weight_bp=0.3,
                                       scheme="dependent",
                                       regrowth = TRUE,
                                       R=20){

#NUMBER OF BASE AND END POINTS IN THE TARGET MILD SAMPLE
  target_bp = npoints(point_pattern_bp_target[[sample_num]])
  target_ep = npoints(point_pattern_target[[sample_num]])

#GET RELEVANT HEALTHY PATTERNS
nbp = lapply(normal_bp_patterns, function(x) npoints(x))
index_normal = which(nbp > 5 + target_bp )

#GET CORRESPONDING HEALTHY PATTERNS
  normal_patterns = normal_patterns[index_normal]
  normal_bp_patterns = normal_bp_patterns[index_normal]

  ############################################# STEP 1 ##########################

#GET EP AND BP HARDCORE PARAMETERS
  hardcore_bp =  get_hardcore( point_pattern_bp_target[[sample_num]] )
  hardcore_ep =  get_hardcore( point_pattern_target[[sample_num]] )


#APPLY THE HARDCORE

  #HARDCORE TO BP PATTERNS
  normal_bp_patterns = lapply(seq_along(normal_bp_patterns),
                           function(x) remove.points_hardcore(normal_bp_patterns[[x]],
                                                              hardcore_bp ))
  #CORRESPONDIN EP PATTERNS
  normal_patterns = lapply(seq_along(normal_bp_patterns),
                           function(x) get.endpoints_thinned(normal_patterns[[x]],
                                                             normal_bp_patterns[[x]]))

  #HARDCORE TO EP PATTERNS
  normal_patterns = lapply(seq_along(normal_bp_patterns),
                           function(x) remove.points_hardcore(normal_patterns[[x]],
                                                              hardcore_ep))

  ###############################################################################



  ############################################### STEP 2 ############################
#APPLY THINNING TO BASEPOINTS

  X_thin = lapply(seq_along(normal_patterns),
                  function(x)thin_to_sample(normal_patterns[[x]],
                                            normal_bp_patterns[[x]],
                                            num_ep = 40000,
                                            num_bp = target_bp,
                                            epsilon =0,
                                            weight_bp = weight_bp,
                                            scheme=scheme))

  #GET ENDPOINTS AND BASEPOINTS
  X_thin_end = lapply(X_thin, function(x) get.endpoints_ENFtrees(x))
  X_thin_base = lapply(X_thin, function(x) get.basepoints_ENFtrees(x))
  ################################################################################


  ################################################# STEP 3 #########################
  #remove PROBS MARK
  X_thin_base = lapply(X_thin_base, function(x) setmarks(x,marks(x)[,c("Tree","type")]))
  if(regrowth ==TRUE ){
  X_thin_end = lapply(X_thin_end, function(x) endpoint_regrowth(x, target_ep,R=R))
  }
  X_thin_end = lapply(X_thin_end, function(x) setmarks(x,marks(x)[,c("Tree","type")]))


  ##################################################################################

  #RETURN AN ENFTREES CLASS OBJECT
  return(lapply(seq_along(X_thin_base), function(x) new("ENFtrees",
             Basepoints = X_thin_base[[x]],
             Endpoints = X_thin_end[[x]])
             ))
}
#EXAMPLE

#simulate_thinned_patterns(sample_num = 1,scheme="dependent")

