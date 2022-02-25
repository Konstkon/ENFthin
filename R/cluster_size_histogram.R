#'
#'
#'
#' @title Creates a histogram of the cluster sizes.
#' @description Creates a histogram comparing the cluster sizes between patterns in three groups.
#' @param{point_pattern}  A list of two-dimensional marked point patterns (objects of class "ppp").
#' Should contain a mark "Tree". Default = realep (the list containing the healthy end point patterns)
#' @param{point_pattern_mild}  A list of two-dimensional marked point patterns (objects of class "ppp").
#' Should contain a mark "Tree". Default = realepmild (the list containing the mild diabetic end point patterns)
#' @param{point_pattern_thinned}  A list of two-dimensional marked point patterns (objects of class "ppp").
#' Should contain a mark "Tree". This list should contain the end point patterns obtained after thinning
#' @param{groupnames}  An argument defining the groups that are compared. The default value is groupnames = c("Healthy","Mild","Thinned")
#' @return  Plots a histogram
#' @export
#'
#' @examples
#' create_cluster_size_histogram(point_pattern_thinned = X_thin_end)

create_cluster_size_histogram <- function(point_pattern_mild = realepmild ,
                                          point_pattern = realep,
                                          point_pattern_thinned,
                                          groupnames = c("Healthy","Mild","Thinned")){

  cluster_sizes  = lapply(point_pattern,
                          function(x) get_cluster_size(x))
  cluster_sizes_mild =lapply(point_pattern_mild,
                             function(x) get_cluster_size(x))
  cluster_sizes_thinned =lapply(point_pattern_thinned,
                                function(x) get_cluster_size(x))

  data <- data.frame(
    type = c( rep(groupnames[2], length(unlist(cluster_sizes_mild)) ),
              rep(groupnames[1], length(unlist(cluster_sizes))),
              rep(groupnames[3],length(unlist(cluster_sizes_thinned)))),
    size = c(unlist(cluster_sizes_mild), unlist(cluster_sizes ),unlist(cluster_sizes_thinned))
  )

  p <- data %>%
    ggplot( aes(x=size,y=..density.., fill=type)) +
    geom_histogram( color="#e9ecef", alpha=0.4, position = 'dodge',binwidth = 0.5) +
    scale_fill_manual(values=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2),rgb(0,1,0,0.2))) +
    theme(text=element_text(size=20))+
    labs(fill="") +xlim(c(0.5,8.5))+
    xlab("Cluster sizes")
  p
}


