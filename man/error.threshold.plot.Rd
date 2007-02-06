\name{error.threshold.plot}
\alias{error.threshold.plot}
\title{ Error Threshold Plot }
\description{
  \code{error.threshold.plot} takes a single model and plots the sensitivity and specificity as a function of threshold. It will optionally add other error statistics such as PCC and/or Kappa to the plot. Optionally, it will also optimize the choice of threshold by several criteria, return the results as a dataframe, and mark the optimized thresholds on the plot.
}
\usage{
error.threshold.plot(DATA, threshold = 101, which.model = 1, na.rm = FALSE, xlab = "Threshold", ylab = "Accuracy Measures", main = NULL, model.names = NULL, color = NULL, line.type = NULL, lwd = 1, plot.it = TRUE, opt.thresholds = NULL, opt.methods = NULL, req.sens, req.spec, obs.prev = NULL, smoothing = 1, vert.lines = FALSE, add.legend = TRUE, legend.text = legend.names, legend.cex = 0.8, add.opt.legend = TRUE, opt.legend.text = NULL, opt.legend.cex = 0.7, pch = NULL, FPC, FNC)
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
  \item{xlab}{ a title for the x axis}
  \item{ylab}{ a title for the y axis}
  \item{main}{ an overall title for the plot }
  \item{model.names}{ a vector of the names of each model included in \code{DATA} to be used in the legend box }
  \item{color}{ should each error statistic be plotted in a different color. It can be a logical value (where \code{TRUE} = color and \code{FALSE} = black and white), or a vector of color codes specifying particular colors for each line.}
  \item{line.type}{ should each model be plotted in a different line type. It can be a logical value (where \code{TRUE} = dashed lines and \code{FALSE} = solid lines), or a vector of codes specifying particular line types for each line.}
  \item{lwd}{ line width }
  \item{plot.it}{ a logical indicating if a graphical plot should be produced}
  \item{opt.thresholds }{ logical indicating whether the optimal thresholds should be calculated and plotted, or a vector specifying thresholds to plot }
  \item{opt.methods}{ what methods should be used to optimize thresholds. Given either as a vector of method names or method numbers. Possible values are:
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
  \item{req.sens}{a value between zero and one giving the user defined required sensitivity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1-maximum allowable errors for points with positive observations).}
  \item{req.spec}{a value between zero and one giving the user defined required sspecificity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1- maximum allowable errors for points with negative observations).}  
  \item{obs.prev}{observed prevalence for \code{opt.method} = \code{"PredPrev=Obs"} and \code{"ObsPrev"}. Defaults to observed prevalence from \code{DATA}.}
  \item{smoothing}{smoothing factor for maximizing/minimizing. Only used if \code{opt.thresholds} = \code{TRUE}. Instead of find the threshold that gives the max/min value, function will average the thresholds of the given number of max/min values.}
  \item{vert.lines}{a logical where: \code{TRUE} means vertical lines added to plot at optimal thresholds; \code{FALSE} means no vertical lines, instead optimal thresholds marked along error statistics plots. Only used if \code{opt.thresholds} = \code{TRUE}. }
  \item{add.legend}{logical indicating if a legend for accuracy statistics should be included on the plot }
  \item{legend.text}{a vector of text for accuracy statistics legend. Defaults to name of each accuracy statistic. }   
  \item{legend.cex}{cex for presence/absence legend}
  \item{add.opt.legend}{logical indicating if a legend for optimal threshold criteria should be included on the plot}  
  \item{opt.legend.text}{a vector of text for optimimal threshold criteria legend. Defaults to text corresponding to 'opt.methods'.}   
  \item{opt.legend.cex}{cex for optimization criteria legend} 
  \item{pch}{plotting "character", i.e., symbol to use for the thresholds specified in \code{MARK}.  \code{pch} can either be a single character or an integer code for one of a set of graphics symbols.  See \code{help(points)} for details. }
   \item{FPC}{ False Positive Costs, or for C/B ratio C = 'net costs of treating nondiseased individuals'.}
  \item{FNC}{ False Negative Costs, or for C/B ratio B = 'net benefits of treating diseased individuals'.}
}
\details{

\code{error.threshold.plot} serves two purposes. First, if \code{plot.it} = \code{TRUE}, it produces a graphical plot. Second, if \code{opt.thresholds} = \code{TRUE} it will find optimal thresholds by several criteria. These optimal thresholds, along with basic accuracy measures for each type of optimal threshold will be returned as a dataframe. If a plot is produced, these optimal thresholds will be added to the plot.

The graphical plot will always include lines showing sensitivity and specificity as a function of threshold. In addition, for \code{opt.methods} = \code{"MaxKappa"}, \code{"MaxPCC"}, \code{"MinROCdist"}, or \code{"MaxSens+Spec"} additional lines will be added to show the statistic being maximized/minimized. 

These lines will be added to graph even if \code{opt.thresholds} = \code{FALSE}. So for example, to produce a graph showing sensitivity, specificity, and Kappa as functions of threshold, with out marking the optimal thresholds, set \code{opt.thresholds} = \code{FALSE}, and \code{opt.methods} = \code{"MaxKappa"}.

See \link{optimal.thresholds} for more details on the optimization methods, and on the arguments \code{ReqSens}, \code{ReqSpec}, \code{obs.prev}, \code{smoothing}, \code{FPC}, and \code{FNC}.

When \code{opt.thresholds} = \code{TRUE}, the default is to plot the optimal thresholds directly along the corresponding error statistics (or along the sensitivity line if the method has no corresponding error statistic). If the argument \code{vert.lines} = \code{TRUE}, a vertical line is drawn at each optimal threshold, and the lines are labeled across the top of the plot.

Note: if too many methods are included in \code{opt.methods}, the graph will get very crowded.
}
\value{
If \code{plot.it} = \code{TRUE} creates a graphical plot.

If \code{FIND.THRESH} = \code{TRUE}, returns a dataframe of information about the optimal thresholds where:
\tabular{lll}{
  \code{[,1]} \tab \code{legend.names} \tab type of optimal threshold \cr
  \code{[,2]} \tab \code{threshold} \tab optimal threshold\cr
  \code{[,3]} \tab \code{PCC} \tab at that threshold \cr
  \code{[,4]} \tab \code{sensitivity} \tab at that threshold \cr
  \code{[,5]} \tab \code{specificity} \tab at that threshold \cr
  \code{[,6]} \tab \code{Kappa} \tab at that threshold \cr}
}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us}  }
\note{ 
}
\seealso{ \link{optimal.thresholds}, \link{presence.absence.accuracy}, \link{roc.plot.calculate}, \link{presence.absence.summary} }
\examples{

data(SIM3DATA)

error.threshold.plot(SIM3DATA,opt.methods=c(1,2,5))

error.threshold.plot(	SIM3DATA, 
				which.model=2, 
				opt.thresholds=TRUE, 
				opt.methods=c("Default", "Sens=Spec", "MinROCdist"), 					vert.lines=TRUE)


error.threshold.plot(	SIM3DATA,
				threshold=101,
				which.model=2,
				na.rm=TRUE,
				xlab="Threshold",
				ylab="Accuracy Measures",
				main="Error Rate verses Threshold",
				model.names=NULL, 
				pch=NULL,
				color= c(3,5,7),
				line.type=NULL,
				lwd=1,
				plot.it=TRUE,
				opt.thresholds=TRUE,
				opt.methods=1:4,
				req.sens=0.85,
				req.spec=0.85,
				obs.prev=NULL,
				smoothing=1,
				vert.lines=FALSE,
				add.legend=TRUE,
				legend.cex=0.8)


}
\keyword{ models } 
