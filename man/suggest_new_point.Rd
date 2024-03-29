% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suggest_new_point.R
\name{suggest_new_point}
\alias{suggest_new_point}
\title{Suggests a new point in a disc}
\usage{
suggest_new_point(point_pattern, isin.Tree, R = 20, hc = 0.62)
}
\arguments{
\item{{point_pattern}}{Two-dimensional point pattern for the end points(object of class "ppp").}

\item{{isin.Tree}}{A boolean vector. If true it means that the point is in a specific tree.}

\item{{hc}}{the hardcore parameter hc > 0. The point are accepted only if they respect the hardcore}

\item{{r}}{the radius of the disc}
}
\value{
A two-dimensional point pattern with the suggested point
}
\description{
Suggest a new point in the disc with radius R and centre estimated as the mean of the end points
in a specific tree. The point is accepted if it respects the hardcore parameter hc, i.e. if it is at least hc apart from the other points.
}
\details{
Simulates uniform points in the disc with centre and Radius r
}
\examples{
suggest_new_point(point_pattern, isin.Add_to_tree,hc = hc,R=R)

}
