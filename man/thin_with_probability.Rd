% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thin_with_probability.R
\name{thin_with_probability}
\alias{thin_with_probability}
\title{Mark based thinning}
\usage{
thin_with_probability(point_pattern)
}
\arguments{
\item{{point_pattern}}{Two-dimensional point pattern (object of class "ppp"). Should have a mark "PROBS"}
}
\value{
The thinned point pattern (object of class "ppp").
}
\description{
Thins a point pattern according to a "PROBS" marks.
}
\examples{
thin_with_probability(marked_point_pattern)
}
