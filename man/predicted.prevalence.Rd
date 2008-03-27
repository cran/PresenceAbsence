\name{predicted.prevalence}
\alias{predicted.prevalence}
\title{ Predicted Prevalence }
\description{
 \code{ predicted.prevalence } calculates the observed prevalence and predicted prevalence for one or models at one or more thresholds.
}
\usage{
predicted.prevalence(DATA, threshold = 0.5, which.model = (1:N.models), na.rm = FALSE)
}
\arguments{
    \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
   	\tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{threshold}{a cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. \code{threshold} can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate. }
  \item{which.model}{a number indicating which models from DATA should be used}
  \item{na.rm}{ a logical indicating whether missing values should be removed}
}
\details{
Function will work for one model and multiple thresholds, or one threshold and multiple models, or multiple models each with their own threshold.
}
\value{
returns a dataframe where: 
\tabular{lll}{
[,1] \tab \code{threshold}   \tab thresholds used for each row in the table \cr
[,2] \tab \code{Obs.Prevalence}  \tab this will be the same in each row\cr
[,3] \tab \code{Model 1} \tab Predicted prevalence for first model\cr
[,4] \tab \code{Model 2} \tab Predicted prevalence for second model, etc...}
}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us}  }
\note{ 
}
\seealso{  }
\examples{
data(SIM3DATA)

predicted.prevalence(SIM3DATA)
predicted.prevalence(SIM3DATA,threshold=11,which.model=1,na.rm=FALSE)
predicted.prevalence(SIM3DATA,threshold=c(.2,.5,.7),na.rm=FALSE)
}
\keyword{ models  }

