library(GET)
library(latex2exp)
library(ggplot2)

#'
#'
#'
#' @title Global envelope plots
#' @description Creates global envelope plots given a set of r values an observed summary statistic and a matrix containing the summary statistics obtained from the simulations.
#' @param{r}  Vector of the radiuses where the summary functions are evaluated
#' @param{observed_statistics} A vector with the values of the observed summary statistics at each value of r
#' @param{simulated_statistics} A matrix where each column consists of the values of the simulated summary statistics at each value of r
#' @param{r_min} minimum value of r to be included in the plot
#' @param{r_max} maximum value of r to be included in the plot
#' @param{type} The type of the envelope. If type=="rank" the rank envelope will be used and the test will give a p-interval otherwise a global envelop based on the "erl" measure will be contructed resulting in a p-value.
#' @return An object of class "global_envelope" in the GET package.
#' @export
#'
#' @examples
#' create_global_envelopes(r, observed_statistics, simulated_statistics )

create_global_envelopes<- function(r ,
                                   observed_statistics ,
                                   simulated_statistics,
                                   r_min = 0,
                                   r_max= 60,
                                   alpha = 0.05,
                                   type="notrank"){
    curve_list <-  list( r= r ,
                       obs =  observed_statistics,
                       sim_m = simulated_statistics)

    curve_set <- create_curve_set(curve_list)
  curve_set <- crop_curves(curve_set, r_min = r_min, r_max = r_max)
if(type=="rank"){
  result <- rank_envelope(curve_set)}
  else{
  result <- GET::global_envelope_test(curve_set)}
  result
}


