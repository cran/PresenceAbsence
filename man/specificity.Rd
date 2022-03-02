\name{specificity}
\alias{specificity}
\title{ Specificity }
\description{
  \code{specificity} calculates the specificity and (optionally) the associated standard deviation from a confusion matrix.

}
\usage{
specificity(CMX, st.dev = TRUE)
}
\arguments{
  \item{CMX}{ a confusion matrix - output from \code{cmx} }
  \item{st.dev}{ a logical indicating whether the associated standard deviation should be calculated }
}
\details{
Specificity is the proportion of observed negatives correctly predicted, and reflects a model's ability to predict an absence given that a species actually does not occur at a location. 
}
\value{
  if \code{st.dev} = FALSE, returns: \code{specificity}. \cr

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[1,1] \tab \code{specificity}   \tab                                         \cr
[1,2] \tab \code{specificity.sd}\tab standard deviation of \code{specificity} }
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{ \code{\link{cmx}}, \code{\link{pcc}}, \code{\link{sensitivity}}, \code{\link{Kappa}}, \code{\link{auc}} }
\examples{
data(SIM3DATA)

specificity(cmx(SIM3DATA))

specificity(cmx(SIM3DATA),st.dev=FALSE)
}
\keyword{ models } 
