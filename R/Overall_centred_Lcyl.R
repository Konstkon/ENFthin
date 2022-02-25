#'
#'
#'
#' @title Overall cylindrical and centred L function
#' @description Computes the overall cylindical and centred L function  from a list of xsim objects directed towards the three main axes
#' The cylindrical K(r) function for each sample is weighted using square number points weights.
#' @param{list_of_xsims} A list containing xsim objects( a dataframe with collumns $x~coordinates and $bbox~bounding box)
#' @param{rad_p} The fixed halfwidth of the cylinders in the x and y directions
#' @param{rad_z} The fixed halfwidth of the cylinders in the z direction.
#' @param{r} A vector of radiuses to evaluate the summary function.
#' @return A dataframe with columns :
#' \itemize{
#' \item $r : radiuses where the summary functions are evaluated
#' \item $L_x : a vector containing the values of the summary statistc directed towards the x direction for each r
#' \item $L_y : a vector containing the values of the summary statistc directed towards the y direction for each r and
#' \item $L_z : a vector containing the values of the summary statistc directed towards the z direction for each r
#' }
#' @export
#'
#' @examples
#' Overall_centred_Lcyl(list_of_xsim)



Overall_centred_Lcyl <-function( list_of_xsim,
                              rad_p = 15,
                              rad_z = 15 ,
                              r=seq(0,30,length=200)){
  #Number of samples
  number_of_samples =  length(list_of_xsim)
  #samplewise K_cyl functions
  K = lapply(list_of_xsim,
             function(x)Kest_directional(x ,cylindrical = TRUE,
                                         epsilon=rad_p, r=r ))
  K_z = lapply(list_of_xsim,
             function(x)Kest_directional(x ,cylindrical = TRUE,
                                         epsilon=rad_z, r=r ))

  #Number of points per sample
  N = unlist(lapply( list_of_xsim, function(x) length(x$x[,1])))
  #Total number of points in subject
  Total_N_squared = sum(N^2)
  #compute the weights
  weights = N^2/Total_N_squared
  # Pooled K
  Pooled_K_x = 0
  Pooled_K_y = 0
  Pooled_K_z = 0
  for (num in 1:number_of_samples){
    Pooled_K_x = Pooled_K_x + weights[[num]]*K[[num]]$`(1,0,0)`
    Pooled_K_y = Pooled_K_y + weights[[num]]*K[[num]]$`(0,1,0)`
    Pooled_K_z = Pooled_K_z + weights[[num]]*K_z[[num]]$`(0,0,1)`
  }
  Pooled_L_x = K_to_L_cyli(  Pooled_K_x ,r = r,epsilon = rad_p)
  Pooled_L_y = K_to_L_cyli(  Pooled_K_y ,r = r,epsilon = rad_p)
  Pooled_L_z = K_to_L_cyli(  Pooled_K_z ,r = r,epsilon = rad_z)

  Pooled = data.frame ( r = r,
                        L_x =Pooled_L_x,
                        L_y =Pooled_L_y,
                        L_z =Pooled_L_z)
  Pooled


}

