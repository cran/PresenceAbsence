\name{SIM3DATA}
\alias{SIM3DATA}
\docType{data}
\title{ Simulated Presence-Absence Data }
\description{
This data set was generated using the \code{presence.absence.simulation()} function. It consists of plotID, observed presence-absence values, and the simulated probability predictions of three different models, for 1000 plots.
}
\usage{data(SIM3DATA)}
\format{
  A data frame with 1000 observations on the following 5 variables.
  \describe{
    \item{\code{plotID}}{a numeric vector}
    \item{\code{Observed}}{a numeric vector of 0-1 values}
    \item{\code{Predicted1}}{a numeric vector of predicted probabilities}
    \item{\code{Predicted2}}{a numeric vector of predicted probabilities }
    \item{\code{Predicted3}}{a numeric vector of predicted probabilities }
  }
}
\source{
  simulated with the \link{presence.absence.simulation} function . See \code{Example 2} from \link{presence.absence.simulation} help file for more details.
}
\examples{
data(SIM3DATA)
}
\keyword{datasets}

