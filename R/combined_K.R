#'
#'
#'
#' @title Compined global envelopes
#' @description Creates compined global envelope plots given a set of r values a list of observed summary statistics and a list of matrices
#'  containing the corresponding summary statistics obtained from the simulations.
#' @param{radius}  Vector of the radiuses where the summary functions are evaluated
#' @param{observed_statistics} A list containing the values of the observed summary statistics at each value of r
#' @param{simulated_statistics} A list containing amatrix where each column consists of the values of the simulated summary statistics at each value of r
#' @param{r_min} minimum value of r to be included in the plot
#' @param{r_max} maximum value of r to be included in the plot
#' @param{is.base_end} A boolean variable. If true then the compined gloabal envelopes for the base and end points patterns will be created. Default value is true.
#' Otherwise the compined global envelopes for three summary functions(used in cylindrical functions) will be created.
#' @return An object of class "global_envelope" in the GET package.
#' @export
#'
#' @examples
#' A= combined_global_test(radius_bp_h,
#'observed_statistic_list = list(observed_statistics_bp_h,observed_statistics_h),
#'simulated_statistic_list = list(simulated_statistics_bp_h,simulated_statistics_h))
#'p = plot(A)+labs(x = expression(italic("r")), y = expression(italic("L(r) - r"))) +
#'  theme(
#'    text = element_text(size=12),
#'    axis.text.x  = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
#'    axis.text.y  = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
#'    axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#'    axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#'    legend.text  = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain")     )


combined_global_test  <- function(radius,
                                  observed_statistic_list,
                                  simulated_statistic_list,
                                  r_min =0,
                                  r_max=60,
                                  is.base_end =TRUE){
curve_set_list_=list()
Number_of_summaries = length(observed_statistic_list)
for (num in 1:Number_of_summaries){

  curve_list = list( r= radius ,
                     obs =  observed_statistic_list[[num]],
                     sim_m =simulated_statistic_list[[num]])
  curve_set <- create_curve_set(curve_list)
  curve_set_list_[[num]] <- crop_curves(curve_set, r_min = r_min, r_max = r_max)

}
if(is.base_end ==TRUE){
curve_set_list_final = list(Endpoints=curve_set_list_[[1]],
                            Basepoints=curve_set_list_[[2]])

}
else{
  curve_set_list_final = list(X=curve_set_list_[[1]],
                              Y=curve_set_list_[[2]],
                              Z=curve_set_list_[[3]])

}
result <- GET::global_envelope_test(curve_set_list_final)
result

}

