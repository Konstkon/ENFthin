% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cluster_size.R
\name{get_cluster_size}
\alias{get_cluster_size}
\title{Computes the size of each cluster.}
\usage{
get_cluster_size(point_pattern)
}
\arguments{
\item{{point_pattern}}{Two-dimensional marked point pattern (object of class "ppp").
Should contain a mark "Tree".}
}
\value{
A vector of the cluster sizes
}
\description{
Computes a vector of cluster sizes (number of points per tree) for each nerve tree from a two-dimensional marked point
pattern that should have a mark "Tree"
}
\examples{
get_cluster_size(realep[[1]])
}
