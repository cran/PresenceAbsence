\name{Kappa}
\alias{Kappa}
\title{ Kappa }
\description{
  \code{Kappa} calculates Kappa and (optionally) the associated standard deviation from a confusion matrix.

}
\usage{
Kappa(CMX, st.dev = TRUE)
}

\arguments{
  \item{CMX}{ a confusion matrix - output from \code{cmx} }
  \item{st.dev}{ a logical indicating whether the associated standard deviation should be calculated }
}
\details{
The Kappa statistic summarizes all the available information in the confusion matrix. Kappa measures the proportion of correctly classified units after accounting for the probability of chance agreement.  
}
\value{
  if \code{st.dev} = FALSE, returns: \code{Kappa}. \cr

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[1,1] \tab \code{Kappa}        \tab                                    \cr
[1,2] \tab \code{Kappa.sd}     \tab standard deviation of \code{Kappa} }
}
\references{ }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }
\note{ 
}
\seealso{ \code{\link{cmx}}, \code{\link{pcc}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{auc}} }
\examples{
data(SIM3DATA)

Kappa(cmx(SIM3DATA))

Kappa(cmx(SIM3DATA),st.dev=FALSE)
}
\keyword{ models }
