#'
#'
#'
#' @title Thins patterns to specific number of points
#' @description Applies a certain thinning strategy to thin patterns to a target number of points.
#' The thinning probability is calculated
#' @param{ppp_ep}  Two-dimensional marked point pattern for the end point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @param{ppp_bp}  Two-dimensional marked point pattern for the base point patterns(object of class "ppp").
#' Should contain a mark "Tree".
#' @param{num_ep} Target number of end points. Notice that if num_ep is large, only the base points will be thinned
#' @param{num_bp} Target number of base points. Notice that if num_bp is large, only the end points will be thinned
#' @param{weight_bp} scale parameter in the PROBABILITY_THINNING() function
#' @param{weight_ep} scale parameter in the PROBABILITY_THINNING() function
#' @param{endpoint.thinning} Whether to thin end points
#' @param{scheme} A string determining the type of thinning to be applied. Either "dependent" or "random".
#' @param{epsilon} epsilon error allowed, i.e. thin to num_ep+epsilon  points instead
#' @return An ENFtrees object for the thinned pattern.
#' @export
#'
#' @examples
#' thin_to_sample_NEW(realep[[1]],
#'realbp[[1]],
#'num_bp = 40,
#'epsilon =0,
#'num_ep = 10000 ,
#'weight_bp = 0.005,
#'weight_ep = 0.008,
#'scheme="dependent")





thin_to_sample_NEW <-function(ppp_ep,
                          ppp_bp ,
                          num_ep ,
                          num_bp ,
                          epsilon = 0,
                          scheme="dependent",
                          weight_bp = 0.2,
                          weight_ep = 0.045,
                          endpoint.thinning = FALSE){

  #get number of base and end points from the samples
  num_points_ppp = npoints(ppp_ep)
  num_points_ppp_bp = npoints(ppp_bp)

  #initializing the thinned patterns
  thinned_ppp = ppp_ep
  thinned_ppp_bp = ppp_bp


  #GET TREE IDS OF THE ENDPOINTS
  marks_thinned_ppp = unique(thinned_ppp$marks$Tree)

  #REMOVE BASEPOINTS THAT HAVE NO ENDPOINTS
  thinned_ppp_bp = thinned_ppp_bp[ match( marks_thinned_ppp,thinned_ppp_bp$marks$Tree) ]


  #BASEPOINT THINNING
  while(num_points_ppp_bp  > (num_bp + epsilon) ){

    #remove iteretively trees at random
    if(scheme == "random"){
      #ASSIGN RANDOM MARKS
      thinned_ppp_bp = assign_random_marks (thinned_ppp_bp)
      #REMOVE POINTS AT RANDOM
      thinned_ppp_bp = thinned_ppp_bp [thinned_ppp_bp$marks$point_id<=floor(num_bp)]
    }

    #DEPENDENT DEPENDENT THINNING DEPENDS ON THE MINIMUM DISTANCES of basepoints

    if(scheme == "dependent"){
      #RETENTION PROBABILITIES
      PROBS =  PROBABILITY_THINNING_(thinned_ppp,
                                     thinned_ppp_bp ,
                                     type="BP",
                                     weight_bp = weight_bp)
      #Calculate thinning probability for each points

      probab = (1-PROBS)/sum(1-PROBS)
      index_removed = sample(length(probab), size=1, replace=TRUE, prob=probab)
      thinned_ppp_bp = thinned_ppp_bp[-index_removed]
         }

    num_points_ppp_bp = npoints(thinned_ppp_bp) #number of trees

  }#end while

  #GET TREE IDS OF THE REMAINING BASEPOINTS
  marks_thinned_ppp_bp = unique(thinned_ppp_bp$marks$Tree)

  #UPDATE CORRESPONDING ENDPOINTS patterns
  thinned_ppp = thinned_ppp[ which(is.na(match( thinned_ppp$marks$Tree,marks_thinned_ppp_bp))==FALSE ) ]


if(endpoint.thinning == TRUE){
  #ENDPOINT THINNING ITERATIONS
  while(npoints(thinned_ppp) > (num_ep+epsilon)){
    if(scheme== "random"){
      thinned_ppp = assign_id_to_pattern(thinned_ppp)
      thinned_ppp = thinned_ppp [thinned_ppp$marks$point_id<=floor(num_ep)]
      #thinned_ppp  = rthin(thinned_ppp , 0.95)
    }

    #DEPENDENT DEPENDENT THINNING DEPENDS ON THE MINIMUM DISTANCES

    if(scheme=="dependent"){
      PROBS_EP = PROBABILITY_THINNING_(thinned_ppp,
                                       ppp_bp ,
                                       type="EP",
                                       weight_ep=weight_ep)

      #ATTACH PROBABILITY MARKS
      thinned_ppp$marks$PROBS = PROBS_EP

      #REMOVE ONE POINT BASED ON MARKS
      thinned_ppp = thin_with_probability(thinned_ppp)


    }

    #GET TREE IDS OF THE REMAINING ENDPOINTS
    marks_thinned_ppp = unique(thinned_ppp$marks$Tree)

    #GET THE CORRESPONDING BASEPOINTS patterns
    thinned_ppp_bp = thinned_ppp_bp[ match( marks_thinned_ppp,thinned_ppp_bp$marks$Tree) ]


  }#END WHILE
}
  return(new("ENFtrees",
             Basepoints = thinned_ppp_bp,
             Endpoints = thinned_ppp))
}
