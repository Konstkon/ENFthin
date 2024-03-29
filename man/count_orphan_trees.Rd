% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Count_orphan_trees.R
\name{count_orphan_trees}
\alias{count_orphan_trees}
\title{Counts the number clusters with one end points}
\usage{
count_orphan_trees(point_pattern)
}
\arguments{
\item{{point_pattern}}{Two-dimensional marked point pattern (object of class "ppp").
The point pattern should have a "Tree" mark denoting the ID of the nerve tree.}
}
\value{
An integer representing the number of one end point trees
}
\description{
Counts the number of "orphan" end points
}
\examples{
count_orphan_trees(realep[[1]])
}
