% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_global_envelops.R
\name{create_global_envelopes}
\alias{create_global_envelopes}
\title{Global envelope plots}
\usage{
create_global_envelopes(
  r,
  observed_statistics,
  simulated_statistics,
  r_min = 0,
  r_max = 60,
  alpha = 0.05,
  type = "notrank"
)
}
\arguments{
\item{{r}}{Vector of the radiuses where the summary functions are evaluated}

\item{{observed_statistics}}{A vector with the values of the observed summary statistics at each value of r}

\item{{simulated_statistics}}{A matrix where each column consists of the values of the simulated summary statistics at each value of r}

\item{{r_min}}{minimum value of r to be included in the plot}

\item{{r_max}}{maximum value of r to be included in the plot}

\item{{type}}{The type of the envelope. If type=="rank" the rank envelope will be used and the test will give a p-interval otherwise a global envelop based on the "erl" measure will be contructed resulting in a p-value.}
}
\value{
An object of class "global_envelope" in the GET package.
}
\description{
Creates global envelope plots given a set of r values an observed summary statistic and a matrix containing the summary statistics obtained from the simulations.
}
\examples{
create_global_envelopes(r, observed_statistics, simulated_statistics )
}
