\name{pcc}
\alias{pcc}
\title{ Percent Correctly Classified }
\description{
  \code{pcc} calculates the percent correctly classified and (optionally) the associated standard deviation from a confusion matrix.
}
\usage{
pcc(CMX, st.dev = TRUE)
}
\arguments{
  \item{CMX}{ a confusion matrix - output from \code{cmx} }
  \item{st.dev}{ a logical indicating whether the associated standard deviation should be calculated }
}
\details{
Percent Correctly Classified is simply the proportion of test observations that are correctly classified. 
}
\value{
  if \code{st.dev} = FALSE, returns: \code{PCC} percent correctly classified.
\cr

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[1,1] \tab \code{PCC}       	\tab percent correctly classified            \cr
[1,2] \tab \code{PCC.sd}     	\tab standard deviation of \code{PCC}          }
}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }
\note{ 
}
\seealso{ \code{\link{cmx}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{Kappa}}, \code{\link{auc}}}
\examples{
data(SIM3DATA)

pcc(cmx(SIM3DATA))

pcc(cmx(SIM3DATA),st.dev=FALSE)
}
\keyword{ models }

