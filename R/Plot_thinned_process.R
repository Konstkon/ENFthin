#'
#'
#'
#' @title Plots the thinned patterns
#' @description Function that plots the thinned patterns and compare it with the original pattern prior to thinning.
#' Used to visualize the thinned patterns and compare them with the original patterns before applying any thinning.
#' @param{ppp_real}  Two-dimensional point pattern for the end point patterns(object of class "ppp") prior to thinning.
#' @param{ppp_bp_real}  Two-dimensional  point pattern for the base point patterns(object of class "ppp") prior to thinning.
#' @param{ppp_thinned} Two-dimensional point pattern for the end point patterns(object of class "ppp") after thinning.
#' @param{ppp_bp_thinned} Two-dimensional  point pattern for the base point patterns(object of class "ppp") after thinning.
#' @param{plot.basepoints} Graphical parameter that controls whether to plot the base point patterns
#' @param{plot.legend} Graphical parameter that controls whether to add a legend.
#' @param{main} Graphical parameter that controls the main title.
#' @return Creates a plot
#' @export
#'
#' @examples
#' plot_thinned_process(realep[[1]],
#' realbp[[1]],
#'X_thin_end[[1]],
#'X_thin_base[[1]],
#'plot.basepoints = FALSE,
#'plot.legend = FALSE)

#Function that plots the thinned_process

plot_thinned_process <- function(ppp_real,
                                 ppp_bp_real,
                                 ppp_thinned,
                                 ppp_bp_thinned,
                                 plot.basepoints = TRUE,
                                 plot.legend = TRUE,
                                 main=""){

  par(mfrow=c(1,1))

  #Plot endpoints
  plot(ppp_real$x,ppp_real$y,pch=20,cex=1,main=main,col=2,xlab="X",ylab="Y",cex.lab =1.4)
  points(ppp_thinned$x,ppp_thinned$y,pch=20,cex=1,col=1,cex.lab =1.4)

  #Plot basepoints
  if(plot.basepoints==TRUE){
  plot(unmark(ppp_bp_real),add=TRUE,pch=2,cex=0.8,col=2)
  plot(unmark(ppp_bp_thinned),add=TRUE,col=1,pch=2,cex=0.8)
  if(plot.legend==TRUE){
  legend(par('usr')[2]-180, par('usr')[4]-30, bty='n', xpd=NA,
      c("Endpoints removed","Endpoints retained", "Basepoints removed", "Basepoints retained"),
      pch = c(20, 20, 2 , 2), cex = c(0.8,0.8,0.8,0.8), col = c(1,2,1,2))
  }
}
  if(plot.basepoints==FALSE){
   if(plot.legend==TRUE){
     legend(par('usr')[2]-180, par('usr')[4]-30, bty='n', xpd=NA,
           c("Endpoints removed","Endpoints retained"),
           pch=c(20, 20),col=c(1,2),cex=c(0.8,0.8))
   }
  }
}


