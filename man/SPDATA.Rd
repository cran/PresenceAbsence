\name{SPDATA}
\alias{SPDATA}
\docType{data}
\title{ Species Presence/Absence Data }
\description{
This data set has Presence/Absence predictions for 13 species at 386 forested locations. It consists of species, observed presence-absence values, and the probability predictions of three different models.
}
\usage{data(SPDATA)}
\format{
  A data frame with 386 observations for 13 species with the following 5 variables.
  \describe{
    \item{\code{SPECIES}}{a character vector of species codes}
    \item{\code{OBSERVED}}{a numeric vector of 0-1 values}
    \item{\code{GAM}}{a numeric vector of predicted probabilities}
    \item{\code{See5}}{a numeric vector of predicted probabilities }
    \item{\code{SGB}}{a numeric vector of predicted probabilities }
  }
}
\source{
This dataset is from:

Moisen, G.G., Freeman, E.A., Blackard, J.A., Frescino, T.S., Zimmerman N.E., Edwards, T.C. Predicting tree species presence and basal area in Utah: A comparison of stochastic gradient boosting, generalized additive models, and tree-based methods. Ecological Modellng, 199 (2006) 176-187.   
}
\examples{
data(SPDATA)
}
\keyword{datasets}

