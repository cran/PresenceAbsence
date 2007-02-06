\name{presence.absence.accuracy}
\alias{presence.absence.accuracy}
\title{ Accuracy Table for Presence/Absence Data }
\description{
  Calculates five accuracy measures (pcc, sensitivity, specificity, Kappa, and AUC) for Presence/Absence data, and (optionally) their associated standard deviations.
}
\usage{
presence.absence.accuracy(DATA, threshold = 0.5, find.auc = TRUE, st.dev = TRUE, which.model = (1:(ncol(DATA) - 2)), na.rm = FALSE)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
   	\tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{threshold}{a cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. If calculations are to be performed on a single model prediction \code{threshold} can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate. If calculations are to be performed on multiple model predictions, then \code{threshold} must be either a single value between zero and one, or a vector of the same length as the number of models.}  
  \item{find.auc}{a logical indicating if area under the curve should be calculated}
  \item{st.dev}{a logical indicating if standard deviations should be calculated}
  \item{which.model}{a number indicating which models from \code{DATA} should be used}
  \item{na.rm}{ a logical indicating whether missing values should be removed}
}
\details{
\code{presence.absence.accuracy} calculates five standard accuracy measures for presence absence data, and (optionally) their associated standard deviations.

Function will work for one model and multiple thresholds, or one threshold and multiple models, or multiple models each with their own threshold.

Depending on the size of the dataset and the speed of the computer this function may take a couple of minutes to run. Finding the AUC is the slowest part of this function. The AUC can be suppressed by setting \code{find.auc} = FALSE.

\code{which.model} can be used to specify which of the prediction models from \code{DATA} should be used.
}
\value{
  if \code{st.dev} = FALSE, returns a dataframe where: 
\tabular{lll}{
 [,1] \tab \code{model}       \tab model name (column name from \code{DATA}) \cr
 [,2] \tab \code{threshold}   \tab thresholds used for each row in the table \cr
 [,3] \tab \code{PCC}       	\tab percent correctly classified              \cr
 [,4] \tab \code{sensitivity} \tab                                           \cr
 [,5] \tab \code{specificity} \tab                                           \cr
 [,6] \tab \code{Kappa}       \tab                                           \cr
 [,7] \tab \code{AUC}         \tab area under the curve                      
}

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[,1] \tab \code{model}       \tab model name (column name from \code{DATA}) \cr
[,2] \tab \code{threshold}   \tab thresholds used for each row in the table \cr
[,3] \tab \code{PCC}         \tab percent correctly classified              \cr
[,4] \tab \code{sensitivity} \tab                                           \cr
[,5] \tab \code{specificity} \tab                                           \cr
[,6] \tab \code{Kappa}       \tab                                           \cr
[,7] \tab \code{AUC}         \tab area under the curve                      \cr
[,8] \tab \code{PCC.sd}     	 \tab standard deviation of \code{PCC}        \cr
[,9] \tab \code{sensitivity.sd}\tab standard deviation of \code{sensitivity}\cr
[,10]\tab \code{specificity.sd}\tab standard deviation of \code{specificity}\cr
[,11]\tab \code{Kappa.sd}      \tab standard deviation of \code{Kappa}      \cr
[,12]\tab \code{AUC.sd}      \tab standard deviation of \code{AUC}      
}
  if \code{find.auc} = FALSE, then columns for \code{AUC} and \code{AUC.sd} are not returned.
}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }
\note{
}
\seealso{ 
\code{\link{cmx}}, \code{\link{pcc}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{Kappa}}, \code{\link{auc}}
}
\examples{

data(SIM3DATA)

### EXAMPLE 1 - multiple model predictions at one threshold###

presence.absence.accuracy(SIM3DATA)
presence.absence.accuracy(SIM3DATA,threshold=.4,st.dev=FALSE)
presence.absence.accuracy(SIM3DATA, which.model=c(1,3),st.dev=FALSE)

### EXAMPLE 2 - one model prediction at multiple thresholds ###

presence.absence.accuracy(SIM3DATA, threshold=c(.25,.5,.75), which.model=3)
presence.absence.accuracy(SIM3DATA, threshold=11, which.model=2)

### EXAMPLE 3 - multiple model predictions, each at it's own treshold ###

presence.absence.accuracy(SIM3DATA, threshold=c(.5,.5,.2), which.model=c(1,2,2))


}
\keyword{ models }
