\name{presence.absence.summary}
\alias{presence.absence.summary}
\title{ Presence/Absence Summary Plots }
\description{
  Produces four types of Presence/Absence accuracy plots for a single set of model Predictions.
}
\usage{
presence.absence.summary(DATA, threshold = 101, find.auc = TRUE, which.model = 1, na.rm = FALSE, main = NULL, model.names = NULL, alpha = 0.05, N.bins = 5, N.bars = 10, truncate.tallest = FALSE, opt.thresholds = NULL, opt.methods = NULL, req.sens, req.spec, obs.prev = NULL, smoothing = 1, vert.lines = FALSE, add.legend = TRUE, add.opt.legend=TRUE, legend.cex = 0.6, opt.legend.cex = 0.6, pch = NULL, FPC, FNC, cost.line = FALSE)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
   	\tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{threshold}{cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. It can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate.}
  \item{find.auc}{a logical indicating if area under the curve should be calculated}
  \item{which.model}{a number indicating which model from \code{DATA} should be used}
  \item{na.rm}{a logical indicating whether missing values should be removed}
  \item{main}{ an overall title for the plot }
  \item{model.names}{ a vector of the names of each model included in \code{DATA} to be used in the legend box }
  \item{alpha}{ alpha value for confidence intervals for \code{calibration.plot}}
  \item{N.bins}{ integer giving number of bins for predicted probabilities for \code{calibration.plot}}
  \item{N.bars}{ number of bars in histogram  }
  \item{truncate.tallest}{ a logical indicating if the tallest bar should be truncated to fit for \code{presence.absence.hist}}
    \item{opt.thresholds}{ a logical indicating whether the optimal thresholds should be calculated and plotted }
  \item{opt.methods}{ what methods should be used to optimize thresholds. Argument can be given either as a vector of method names or method numbers. Possible values are:
  \tabular{rll}{
  1  \tab \code{Default}   \tab threshold=0.5 \cr
  2  \tab \code{Sens=Spec} \tab sensitivity=specificity \cr
  3  \tab \code{MaxSens+Spec} \tab maximizes (sensitivity+specificity)/2 \cr
  4  \tab \code{MaxKappa}  \tab maximizes Kappa \cr
  5  \tab \code{MaxPCC}    \tab maximizes PCC (percent correctly classified) \cr
  6  \tab \code{PredPrev=Obs} \tab predicted prevalence=observed prevalence \cr
  7  \tab \code{ObsPrev}   \tab threshold=observed prevalence \cr
  8 \tab \code{MeanProb } \tab mean predicted probability \cr 
  9  \tab \code{MinROCdist}\tab minimizes distance between ROC plot and (0,1)\cr
  10 \tab \code{ReqSens} \tab user defined required sensitivity \cr
  11 \tab \code{ReqSpec} \tab user defined required specificity }}
  \item{req.sens}{ a value between zero and one giving the user defined required sensitivity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1-maximum allowable errors for points with positive observations).}
  \item{req.spec}{ a value between zero and one giving the user defined required sspecificity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1- maximum allowable errors for points with negative observations).}  
  \item{obs.prev}{observed prevalence for \code{opt.method} = \code{"PredPrev=Obs"} and \code{"ObsPrev"}. Defaults to observed prevalence from \code{DATA}.}
  \item{smoothing}{ smoothing factor for maximizing/minimizing. Only used if \code{opt.thresholds} = \code{TRUE}. Instead of find the threshold that gives the max/min value, function will average the thresholds of the given number of max/min values.}
  \item{vert.lines}{ a logical where: \code{TRUE} means vertical lines added to plot at optimal thresholds; \code{FALSE} means no vertical lines, instead optimal thresholds marked along error statistics plots. Only used if \code{opt.thresholds} = \code{TRUE}. }
  \item{add.legend}{ logical indicating if a legend should be included on the plot }
  \item{add.opt.legend}{ logical indicating if optimization criteria legend should be included on the plot }
  \item{legend.cex}{ cex for legends }
  \item{opt.legend.cex}{ cex for optimization criteria legend }
  \item{pch}{ plotting "character", i.e., symbol to use for the thresholds specified in \code{MARK}.  \code{pch} can either be a single character or an integer code for one of a set of graphics symbols.  See \code{help(points)} for details. }
  \item{FPC}{ False Positive Costs, or for C/B ratio C = 'net costs of treating nondiseased individuals'.}
  \item{FNC}{ False Negative Costs, or for C/B ratio B = 'net benefits of treating diseased individuals'.}
    \item{cost.line}{ a logical indicating if the line representing the realtive cost ratio should be added to the plot.}
}
\details{
\code{presence.absence.summary} produces a set of summary plots for a single model, along with calculating AUC and optimal thresholds. \code{presence.absence.summary} is not quite as flexible as the individual plot functions, as some arguments are preset so that the plots will be comparable, but the remaining arguments have the same meaning. See the individual plot functions \link{error.threshold.plot}, \link{auc.roc.plot}, \link{calibration.plot}, and \link{presence.absence.hist} for further details.
}
\value{
creates a graphical plot}
\references{  }
\author{Elizabeth Freeman \email{eafreeman@fs.fed.us} }
\note{ 
}
\seealso{\link{optimal.thresholds}, \link{error.threshold.plot}, \link{auc.roc.plot}, \link{calibration.plot}, \link{presence.absence.hist}}
\examples{
data(SIM3DATA)

presence.absence.summary(SIM3DATA)

presence.absence.summary(	SIM3DATA,
					threshold=101,
					find.auc=TRUE,
					which.model=2,
					na.rm=FALSE,
					main=NULL,
					model.names=NULL,
					alpha=0.05,
					N.bins=5,
					N.bars=10,
					truncate.tallest=FALSE,
					opt.thresholds=TRUE,
					opt.methods=c(1,2,4),
					req.sens=0.85,
					req.spec=0.85,
					obs.prev=NULL,
					smoothing=1,
					vert.lines=FALSE,
					add.legend=TRUE,
					add.opt.legend=TRUE,
					legend.cex=0.6,
					opt.legend.cex=0.6,
					pch=NULL)
}
\keyword{ models } 
