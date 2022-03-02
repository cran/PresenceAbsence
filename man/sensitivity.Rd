\name{sensitivity}
\alias{sensitivity}
\title{ Sensitivity }
\description{
  \code{sensitivity} calculates the sensitivity and (optionally) the associated standard deviation from a confusion matrix.
}
\usage{
sensitivity(CMX, st.dev = TRUE)
}
\arguments{
  \item{CMX}{ a confusion matrix - output from \code{cmx} }
  \item{st.dev}{ a logical indicating whether the associated standard deviation should be calculated }
}
\details{
Sensitivity is the proportion of observed positives correctly predicted, and reflects a model's ability to predict a presence given that a species actually occurs at a location. 
}
\value{
  if \code{st.dev} = FALSE, returns: \code{sensitivity}. \cr

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[1,1] \tab \code{sensitivity}   \tab                                         \cr
[1,2] \tab \code{sensitivity.sd}\tab standard deviation of \code{sensitivity} }
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us}}

\seealso{ \code{\link{cmx}}, \code{\link{pcc}}, \code{\link{specificity}}, \code{\link{Kappa}}, \code{\link{auc}} }
\examples{
data(SIM3DATA)

sensitivity(cmx(SIM3DATA))

sensitivity(cmx(SIM3DATA),st.dev=FALSE)
}
\keyword{ models }
