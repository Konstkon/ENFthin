#'
#'
#'
#' @title Overall centre cylindrical curves from a list of xsim objects
#' @description Provided a list of size [N] [M], where N=number of samples and M=number of simulations
#' , at each iteration a random simulated pattern from each sample is selected and the overall centred cylindrical L
#' function is estimated for the three main axes. This procedure is repeated nsim times which produces nsim curves.  Square number
#' weights are used to weights the sample wise summary functions.
#' @param{x_sim_list} A list of size [N] [M] containing xsim objects( a list with $x~coordinates $bbox~bounding box)
#' @param{nsim}  Number of simulations. Default is nsim=2500.
#' @param{rad_p} The fixed halfwidth of the cylinders in the x and y directions
#' @param{rad_z} The fixed halfwidth of the cylinders in the z direction.
#' @return A list with columns:
#' \itemize{
#' \item $L_x : a matrix where each column is a pooled estimate of the cylindrical centred L function towards the x-axis from patterns that are obtained at random.
#' \item $L_y : a matrix where each column is a pooled estimate of the cylindrical centred L function towards the y-axis from patterns that are obtained at random.
#' \item $L_z : a matrix where each column is a pooled estimate of the cylindrical centred L function towards the z-axis from patterns that are obtained at random.
#' \item $radius : radiuses where the summary functions are evaluated}
#'
#' @export
#'
#' @examples
#' get.simulated_statistics_Ldir(simulations_xsim_list)
#'


get.simulated_statistics_Ldir <- function( x_sim_list,
                                           nsim=2500,
                                           rad_p =15,
                                           rad_z =15){
  #Get random indices
  INDEX = Random.samples(x_sim_list,nsim)
  #loop through nsim
  for( i in 1:nsim){
    #Calculate curves for each simulation
    simulated_samples = Create.list.samples(x_sim_list, INDEX[i,])

    #Change this
    simulated = Overall_centred_Lcyl(simulated_samples,rad_p = rad_p, rad_z = rad_z)
    if(i==1){
      simulated_statistics_x = simulated$ L_x
      simulated_statistics_y = simulated$ L_y
      simulated_statistics_z = simulated$ L_z
    }else{

      simulated_statistics_x = cbind(simulated_statistics_x, simulated$ L_x)
      simulated_statistics_y = cbind(simulated_statistics_y, simulated$ L_y)
      simulated_statistics_z = cbind(simulated_statistics_z, simulated$ L_z)
    }
  }
  simulated_statistics = list ( L_x = simulated_statistics_x ,
                                L_y = simulated_statistics_y,
                                L_z = simulated_statistics_z,
                                radius = simulated$r)

}
