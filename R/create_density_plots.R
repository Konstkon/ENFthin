

#'
#'
#'
#' @title Prior/posterior predictive plots
#' @description Creates Prior/posterior predictive plots given vectors containing summary statistics S1 and S2
#' obtained from parameters from the prior or posterior
#' @param{V1} A numerical vector for summary statistic S1
#' @param{V2} A numerical vector for summary statistic S2
#' @param{observed} The observed c(S1,S2) summary statistic
#' @param{xwin} Limits of the x-dimension of the observational window
#' @param{ywin} Limits of the y-dimension of the observational window
#'
#' @return A density plot
#' @export
#'
#' @examples
#' create_density_plot (V1 = ABC_summary_1,V2=ABC_summary_2,observed=c(53.49,25.29))

create_density_plot <- function(V1,
                                V2,
                                observed,
                                xwin=c(30,100),
                                ywin=c(0,80)){
  A= ppp (V1,V2, owin(xwin,ywin))
  plot(density(A))
  points(observed,col=1,pch=20)

}

