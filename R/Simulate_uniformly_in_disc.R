#'Simulates uniform points in the disc with centre and Radius r
#'
#'
#' @title Uniform points in disc
#' @description Creates a dataframe with the $X~coordinates and $Y~coordinates for the simulated points
#' @param{n}  number of points to return.
#' @param{centre} the $x~coordinates and $y~coordinates for the disc centre
#' @param{r} the radius of the disc
#' @return a dataframe with the $X~coordinates and $Y~coordinates of the simulated points
#' @export
#'
#' @examples
#' simulate_uniformly_in_disc(1000,c(0,0),3)
#'
simulate_uniformly_in_disc <- function(n, centre, r ){

  rho <- sqrt(runif(n, 0, r^2))
  theta <- runif(n, 0, 2*pi)
  x <- centre[1] + rho * cos(theta)
  y <- centre[2] + rho * sin(theta)
  df = data.frame(X=x,Y=y)
  df
}


