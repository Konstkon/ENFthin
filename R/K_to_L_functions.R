#'
#'
#'
#' @title K to L function transformations
#' @description Transforms directional K functions to directional centred L functions. 
#' @param{K} A vector with the directional K functions evaluated at r
#' @param{epsilon} The fixed halfwidth of the cylinders (cylindrical functions)
#' . Central angle for the directed cone ( cone function)
#' @param{r} A vector of radiuses to evaluate the summary function.
#' @return A vector with the centred L functions evaluated at r
#' @export
#'
#' @examples
#' K_to_L_cone(K,r,epsilon)
#' K_to_L_cyli(K,r,epsilon)
#' L_to_K_cyli(K,r,epsilon)

K_to_L_cyli <- function(K,r,epsilon){
  a= K/(2*pi*epsilon^2) - r
  a
}


K_to_L_cone <- function(K,r,epsilon)
{
  a= ((3*K)/(4*pi*(1-cos(epsilon))))^(1/3)-r
  a
}


L_to_K_cyli<- function(L,r,epsilon){
  a= 2*pi*epsilon^2*(L+r)
  a
}