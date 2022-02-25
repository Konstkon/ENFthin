library(Kdirectional)
#'
#'
#'
#' @title Create an xsim object
#' @description Creates a list with $x~coordinates $bbox~bounding box to be used in the Kest_directional function
#' @param{X}  Three-dimensional point pattern (object of class "pp3").
#' @return A list with $x~coordinates $bbox~bounding box
#' @export
#'
#' @examples
#' create_xsim(rpoispp3(10))

create_xsim <- function(X){
  x_simu = cbind(X$data$x, X$data$y, X$data$z)
  x_simu_bbox = bbox_make(x_simu)
  res=list(x = x_simu, bbox= x_simu_bbox)
}
