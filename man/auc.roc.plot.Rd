\name{auc.roc.plot}
\alias{auc.roc.plot}
\title{ AUC ROC Plot }
\description{
  \code{auc.roc.plot} creates a ROC plot for one dataset and one or more model predictions. Prints AUC for each model as part of the legend. \code{auc.roc.plot} also includes an option to mark several types of optimal thresholds along each ROC plot.
}
\usage{
auc.roc.plot(DATA, threshold = 101, find.auc = TRUE, which.model = (1:(ncol(DATA) - 2)), na.rm = FALSE, xlab = "1-Specificity (false positives)", ylab = "Sensitivity (true positives)", main = "ROC Plot", model.names = NULL, color = NULL, line.type = NULL, lwd = 1, mark = 0, mark.numbers = TRUE, mark.color = NULL, opt.thresholds = NULL, opt.methods = NULL, req.sens, req.spec, obs.prev = NULL, smoothing = 1, add.legend = TRUE, legend.text = model.names, legend.cex = 0.8, add.opt.legend = TRUE, opt.legend.text = NULL, opt.legend.cex = 0.7, counter.diagonal = FALSE, pch = NULL, FPC, FNC, cost.line = FALSE)
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
  \item{xlab}{ a title for the x axis}
  \item{ylab}{ a title for the y axis}
  \item{main}{ an overall title for the plot }
  \item{model.names}{ a vector of the names of each model included in \code{DATA} to be used in the legend box }
  \item{color}{ should each model be plotted in a different color. It can be a logical value (where \code{TRUE} = color and \code{FALSE} = black and white), or a vector of color codes specifying particular colors for each line.}
  \item{line.type}{ should each model be plotted in a different line type. It can be a logical value (where \code{TRUE} = dashed lines and \code{FALSE} = solid lines), or a vector of codes specifying particular line types for each line.}
  \item{lwd}{ line width }
  \item{mark}{ particular thresholds to mark along each roc plot, given in same format as \code{threshold}. Note: if \code{optimal.thresholds} = \code{TRUE}, argument \code{mark} will be ignored.}
  \item{mark.numbers}{a logical indication if the threshold values of each marked point along the ROC curved should be labeled next to the points}
  \item{mark.color}{should the marked thresholds be plotted in a different color for each model. A logical value where \code{TRUE} equals same colors as the lines, and \code{FALSE} = marks are always black. Can also be specified as a vector of color codes. Note that is this case, it is one color per model, not one color per threshold.}
  \item{opt.thresholds}{ logical indicating whether the optimal thresholds should be calculated and plotted, or a vector specifying thresholds to plot }
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
  \item{add.legend}{a logical indicating if a legend for AUC lines should be added to plot}   
  \item{legend.text}{a two item vector of text for presence/absence legend. Defaults to 'model.names'. }   
  \item{legend.cex}{cex for AUC legend}
  \item{add.opt.legend}{logical indicating if a legend for optimal threshold criteria should be included on the plot}  
  \item{opt.legend.text}{a vector of text for optimimal threshold criteria legend. Defaults to text corresponding to 'opt.methods'.}   
  \item{opt.legend.cex}{cex for optimization criteria legend} 
  \item{counter.diagonal}{ should a counter-diagonal line be plotted. Note: each ROC plot crosses this line at the point where sensitivity equals specificity for that model. }
  \item{pch}{ plotting "character", i.e., symbol to use for the thresholds specified in \code{mark}.  \code{pch} can either be a single character or an integer code for one of a set of graphics symbols.  See \code{help(points)} for details. }
  \item{FPC}{ False Positive Costs, or for C/B ratio C = 'net costs of treating nondiseased individuals'.}
  \item{FNC}{ False Negative Costs, or for C/B ratio B = 'net benefits of treating diseased individuals'.}
  \item{cost.line}{ a logical indicating if the line representing the realtive cost ratio should be added to the plot.}
}
\details{
  Receiver Operating Curves (ROC plots) provide a threshold independent method of evaluating the performance of presence/absence models. In a ROC plot the true positive rate (sensitivity) is plotted against the false positive rate (1.0-specificity) as the threshold varies from 0 to 1. A good model will achieve a high true positive rate while the false positive rate is still relatively small; thus the ROC plot will rise steeply at the origin, and then level off at a value near the maximum of 1. The ROC plot for a poor model (whose predictive ability is the equivalent of random assignment) will lie near the diagonal, where the true positive rate equals the false positive rate for all thresholds. Thus the area under the ROC curve (AUC) is a good measure of overall model performance, with good models having an AUC near 1, while poor models have an AUC near 0.5.

  \code{mark} can be used to mark particular thresholds along each ROC plot, alternativly, if \code{optimal.thresholds} = \code{TRUE} the function will find optimal thresholds by several criteria and plot them along each ROC curve.

See \link{optimal.thresholds} for more details on the optimization methods, and on the arguments \code{ReqSens}, \code{ReqSpec}, \code{obs.prev} \code{smoothing}, \code{FPC}, \code{FNC}, and \code{cost.line}.

Note: if too many methods are included in \code{opt.methods}, the graph will get very crowded.
}
\value{
creates a graphical plot}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }
\note{ 
}
\seealso{ \link{optimal.thresholds},\link{presence.absence.accuracy}, \link{roc.plot.calculate}, \link{error.threshold.plot}, \link{presence.absence.summary} }
\examples{

data(SIM3DATA)

auc.roc.plot(SIM3DATA)

auc.roc.plot(	SIM3DATA,
			opt.thresholds=TRUE,
			opt.methods=c("Default","Sens=Spec","MinROCdist"))

auc.roc.plot(	SIM3DATA,
			threshold=101,
			which.model=c(2,3),
			model.names=c("model a","model b","model c"),
			na.rm=TRUE,
			xlab="1-Specificity (false positives)",
			ylab="Sensitivity (true positives)",
			main="ROC Plot", 
			color=TRUE,
			line.type=TRUE,
			lwd=1,
			mark=0,
			mark.numbers=TRUE,
			opt.thresholds=TRUE,
			opt.methods=c(1,2,4),
			req.sens=0.85,
			req.spec=0.85,
			obs.prev=NULL,
			add.legend=TRUE,
			legend.text=NULL,
			legend.cex=0.8,
			add.opt.legend=TRUE,
			opt.legend.text=NULL,
			opt.legend.cex=0.7,
			counter.diagonal=TRUE,
			pch=NULL)
}
\keyword{ models }
