\name{roc.plot.calculate}
\alias{roc.plot.calculate}
\title{ ROC Plot Calculations }
\description{
\code{roc.plot.calculate} calculates PCC, sensitivity, specificity, and Kappa for a single presence absence model at a series of thresholds in preparation for creating a ROC plot.
}
\usage{
roc.plot.calculate(DATA, threshold = 101, which.model = 1, na.rm = FALSE)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
     \tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{threshold}{cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. It can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate.}
  \item{which.model}{a number indicating which model from \code{DATA} should be used}
  \item{na.rm}{a logical indicating whether missing values should be removed}
}
\details{
\code{roc.plot.calculate} is a streamlined version of \code{presence.absence.accuracy} designed specifically to compute the accuracy measures needed to produce a ROC plot. \code{roc.plot.calculate} is less versatile, but more efficient than \code{presence.absence.accuracy}. 

Unlike \code{presence.absence.accuracy}, \code{roc.plot.calculate} will only work for a single set of model predictions. Therefore either \code{DATA} can contain only one model prediction, or \code{which.model} must be used to indicate a single model prediction from \code{DATA}. By default, if \code{DATA} contains more than one model prediction, and \code{which.model} is not specified, \code{roc.plot.calculate} will use the first model prediction (e.g. \code{DATA[,3]}).

\code{roc.plot.calculate} was written as a sub-function for the plotting functions(i.e. \code{error.threshold.plot}, \code{auc.roc.plot}, but can be used on its own to produce a simple table of how the accuracy measures vary with choice of threshold.

To produce attractive plots requires a large number of thresholds. The default value of \code{threshold} = 101 is a good compromise between speed and resolution.
}
\value{
Returns a dataframe where: 
\tabular{lll}{
[,1] \tab \code{threshold}   \tab thresholds used for each row in the table \cr
[,2] \tab \code{PCC}         \tab percent correctly classified              \cr
[,3] \tab \code{sensitivity} \tab                                           \cr
[,4] \tab \code{specificity} \tab                                           \cr
[,5] \tab \code{Kappa}       \tab                                           }
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{ 
\link{cmx}, \link{pcc}, \link{sensitivity}, \link{specificity}, \link{Kappa}, \link{presence.absence.accuracy}, \link{error.threshold.plot}, \link{auc.roc.plot}
}
\examples{
data(SIM3DATA)

roc.plot.calculate(SIM3DATA,which.model=2)
}
\keyword{ models }
