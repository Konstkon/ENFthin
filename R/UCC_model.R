#'
#'
#'
#' @title UCC model
#' @description imulates EP codnitioned on BP using the NOC/UCC models
#' @param{Basepoints} Dataframe for the base points patterns
#' @param{Group} Normal of Mild
#' @param{type} "UCC" or "NOC"
#' @param{params} Data frame with the estimated parameters
#' @param{ID} ID of subject
#' @return A dataframe
#' @export
#'
#' @examples
#' simulateUCC(realbpdf[[1]],"Normal",type="NOC",parameters = params_subject,ID=SID[1])
#' simulateUCC(get.basepoints(Foot.1097.1.pgp1),"Normal")
#' simulateUCC(realbpdf[[1]],"Normal",parameters = Parameters_normal,ID=SID[1])
simulateUCC <- function(Basepoints,
                        Group,
                        type = "UCC",
                        parameters=0,
                        ID=99){
  end_p = data.frame()
  Basepoints=Basepoints[,1:3]
  Basepoints$Basepoint = TRUE
  Basepoints$Endpoint = FALSE
  par = subset(parameters, parameters$SID == ID)
  
  if(length(parameters)==1){print(paste("Parameters are missing!! Provide a data.frame with the parameters"))}
  Num_bp =    length(Basepoints$X)                                       # Number of basepoints
  # Simulating end points for every first base point
  for (k in 1:Num_bp) {
       Num_ep =     Simulate_offspring( Group,par )  + 1                            # Number of end points for branch point k
    Length_ep =  Simulate_Length_UCC ( Num_ep , Group,par )                    # Simulates Lengths
      if(type=="UCC"){
        angles_ep =  Simulate_Direction_UCC ( Num_ep , Group,par )                 # Simulates Directions
   }
    if(type=="NOC"){
        NOC_direction = get_direction_to_open_space(Basepoints)
      angles_ep = Simulate_Direction_first_branch(Num_ep, NOC_direction,par)
      }
    end_points = Simulate_points_UCC( Length_ep , angles_ep , Basepoints [k,]  ) # Simulates end points
    end_points$Endpoint = TRUE
    end_p = rbind ( end_p , end_points )
  }
  DF = rbind(end_p, Basepoints)
  DF
}



Simulate_offspring<-function( Group,par ){
 # if (Group=='Normal')
#  {
#    prob = 0.5631258
 #   size = 2.071027
#  }
#  else if (Group == 'Mild'){
#    prob = 0.6784139
#    size = 2.949334
#  }
  #else{ print(paste('Please provide a valid disease group'))
  #  return()
  #}
  prob=as.numeric(par[5])
  size= as.numeric(par[6])
  offspring = rnbinom(1 , size , prob )
  offspring
}

Simulate_Length_UCC <- function(Num_ep, Group,par){
  shape = as.numeric(par[1])   # Need to calculate that
  rate = as.numeric(par[2])
  Length = rgamma(Num_ep,shape,rate)
  Length
}

Simulate_Direction_UCC <- function( Num_ep , Group,par ){ # TO DO add group
  #k=0.1242842    # Need to calculate that
  k= as.numeric(par[3])
  mean_dir= runif( 1 , 0 , 2 * pi )

  phi = as.numeric(rvonmises( Num_ep , mean_dir, k  ))
  df = data.frame(  phi = phi )
  df
}




Simulate_points_UCC <-function( Length , angle , Motherpoints  ){
  X = Length * cos( angle$phi )  + Motherpoints$X
  Y = Length * sin( angle$phi )  + Motherpoints$Y
  df = data.frame( X = X, Y = Y ,  Tree = Motherpoints$Tree, Basepoint = FALSE )
  df
}


get_direction_to_open_space <-function(Basepoints){
  Basep = Basepoints[,c('X','Y')]
  basedist = as.matrix(dist(Basep))
  
  direction = data.frame()
  for ( i in 1:length(basedist[1,])){
    min_dist = sort(basedist[i,])[2]
    pos = which(basedist[i,] == min_dist)
    direction_temp = data.frame(X = Basep[i,]$X - Basep[pos,]$X , Y = Basep[i,]$Y-Basep[pos,]$Y)    
    direction = rbind( direction, direction_temp)
  }
  as.matrix(direction)
  
} 


#Angles first branch
#Angles first branch
Simulate_Direction_first_branch <- function ( Num_bp,phi ,par ){
  radius = sqrt ( runif ( Num_bp , 0 , 1 ) )
  kappa = par$kappa
  phi_ = c()
  for (l in 1:Num_bp){
    phi_ = c(phi_, as.numeric(rvonmises(1, phi[l], kappa)))
  }
  x = radius * cos( phi_ )
  y = radius * sin( phi_ )
  z = sqrt( 1 - x * x - y * y )
  theta = acos( z )
  df = data.frame( theta = theta , phi = phi_ )
  df
}

