\name{SPPREV}
\alias{SPPREV}
\docType{data}
\title{ Overall Preavalences for Species Presence/Absence Data }
\description{
This data set is summary prevalence for Presence/Absence data for 13 species from 1930 forested locations. \code{SPPREV} is the prevalence data from the full dataset (training and test data). Note that \code{SPDATA} is the model predictions from the test data subset (20 percent of total plots) of this original dataset, and therefore the species in \code{SPDATA} have slightly different prevalence than the overall prevalence given in \code{SPPREV}.}
\usage{data(SPDATA)}
\format{
  A data frame with data for 13 species. Dataframe consists of species names, number of plots where the species was present, and the overall prevalence for each species:
  \describe{
    \item{\code{SPECIES}}{a character vector of species codes}
    \item{\code{NUMPLOTS}}{a numeric vector of plot counts}
    \item{\code{PREV}}{a numeric vector of prevalence}
  }
}
\source{
This dataset is from:

Moisen, G.G., Freeman, E.A., Blackard, J.A., Frescino, T.S., Zimmerman N.E., Edwards, T.C. Predicting tree species presence and basal area in Utah: A comparison of stochastic gradient boosting, generalized additive models, and tree-based methods. Ecological Modellng, 199 (2006) 176-187.   
}
\examples{
data(SPPREV)
}
\keyword{datasets}

