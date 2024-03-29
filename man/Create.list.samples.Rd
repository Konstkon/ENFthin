% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_list_samples.R
\name{Create.list.samples}
\alias{Create.list.samples}
\title{Creates a list of samples}
\usage{
Create.list.samples(list_of_samples, Index_row)
}
\arguments{
\item{{list_of_samples}}{a list of list of two-dimensional point patterns(objects of class "ppp")}

\item{{Index_row}}{a vector of indices}
}
\value{
A list of two-dimensional point patterns
}
\description{
Creates a list of samples according to a vector of indeces and a list of size [number of samples][number of simulations] of ppp objects
}
\examples{
Create.list.samples(simulations, Random.samples(simulations))
}
