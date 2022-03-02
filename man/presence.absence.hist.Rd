\name{presence.absence.hist}
\alias{presence.absence.hist}
\title{ Presence/Absence Histogram }
\description{
  Produces a histogram of predicted probabilities with each bar subdivided by observed values. \code{presence.absence.hist} also includes an option to mark several types of optimal thresholds along each plot.
}
\usage{
presence.absence.hist(DATA, which.model = 1, na.rm = FALSE, 
xlab = "predicted probability", ylab = "number of plots", 
main = NULL, model.names = NULL, color = NULL, N.bars = 20, 
truncate.tallest = FALSE, ylim = 1.25 * range(0, apply(counts, 2, sum)),
opt.thresholds = NULL, threshold = 101, opt.methods = NULL, 
req.sens, req.spec, obs.prev = NULL, smoothing = 1, add.legend = TRUE, 
legend.text=c("present","absent"), legend.cex = 0.8, add.opt.legend = TRUE, 
opt.legend.text = NULL, opt.legend.cex = 0.7, pch = NULL, FPC, FNC)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
   	\tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{which.model}{a number indicating which model from \code{DATA} should be used}
  \item{na.rm}{a logical indicating whether missing values should be removed}
  \item{xlab}{a title for the x axis}
  \item{ylab}{a title for the y axis}
  \item{main}{an overall title for the plot }
  \item{model.names}{a vector of the names of each model included in \code{DATA}}
  \item{color}{colors for presence/absence. Defaults to Presence = dark gray, Absence = light gray.}
  \item{N.bars}{number of bars in histogram }
  \item{truncate.tallest}{ a logical indicating if the tallest bar should be truncated to fit on plot}
  \item{ylim}{limit for y axis. To allow room for legend box \code{ylim} should be somewhat larger than largest bar.}
  \item{opt.thresholds}{a logical indicating whether the optimal thresholds should be calculated and plotted, or a vector specifying thresholds to plot}
  \item{threshold}{cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. It can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate. To get reasonably good optimizations, there should be a large number of thresholds. (Only used if \code{opt.thresholds} = \code{TRUE}.}
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
  \item{smoothing}{smoothing factor for maximizing/minimizing. Only used if \code{opt.thresholds} = \code{TRUE}. Instead of find the threshold that gives the max/min value, function will average the thresholds of the given number of max/min values.}
  \item{add.legend}{a logical indicating if a legend for presence/absence should be added to plot}   
  \item{legend.text}{a two item vector of text for presence/absence legend. Defaults to "present" and "absent". }   
  \item{legend.cex}{cex for presence/absence legend}
  \item{add.opt.legend}{logical indicating if a legend for optimal threshold criteria should be included on the plot}  
  \item{opt.legend.text}{a vector of text for optimimal threshold criteria legend. Defaults to text corresponding to 'opt.methods'.}   
  \item{opt.legend.cex}{cex for optimization criteria legend} 
  \item{pch}{ plotting "character", i.e., symbol to use for the thresholds specified in \code{mark}.  \code{pch} can either be a single character or an integer code for one of a set of graphics symbols.  See \code{help(points)} for details. }
   \item{FPC}{ False Positive Costs, or for C/B ratio C = 'net costs of treating nondiseased individuals'.}
  \item{FNC}{ False Negative Costs, or for C/B ratio B = 'net benefits of treating diseased individuals'.}
}
\details{
When examining a Presence/Absence histogram to evaluate model quality, a good model will produce a clear separation of 'present' and 'absent' with little overlap in any bars.

The \code{truncate.tallest} argument is useful when one bar (often the bar for predicted probability of zero) is much larger than all the other bars. If \code{truncate.tallest} = \code{TRUE}, the tallest bar is truncated to slightly taller than the next highest bar, and the actual count is plotted above the bar. The truncated bar is also crosshatched to avoid confusion by making it more obviously different from the other bars.

if \code{optimal.thresholds} = \code{TRUE} the function will find optimal thresholds by several methods and plot them along the X axis. See \link{optimal.thresholds} for more details on the optimization methods, and on the arguments \code{ReqSens}, \code{ReqSpec}, \code{obs.prev}, \code{smoothing}, \code{FPC}, and \code{FNC}.

Note: if too many methods are included in \code{opt.methods}, the graph will get very crowded.
}
\value{
  creates a graphical plot 
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{ \link{optimal.thresholds},\link{presence.absence.summary} }
\examples{
data(SIM3DATA)

### EXAMPLE 1 - Comparing three models ###
par(mfrow=c(1,3))
for(i in 1:3){
	presence.absence.hist(	SIM3DATA,
					which.model=i,
					na.rm=TRUE,
					model.names=c("Model 1","Model 2","Model 3"),
					N.bars=10,
					truncate.tallest=FALSE,
					opt.thresholds=TRUE,
					opt.methods=c("Default","Sens=Spec","MaxKappa"))}

### EXAMPLE 2 - Effect of 'truncate.tallest' argument ###
par(mfrow=c(1,2))
presence.absence.hist(	SIM3DATA,
				which.model=1,
				model.names=c("Model 1","Model 2","Model 3"),
				N.bars=10,
				truncate.tallest=FALSE,
				main="truncate.tallest=FALSE")
presence.absence.hist(	SIM3DATA,
				which.model=1,
				model.names=c("Model 1","Model 2","Model 3"),
				N.bars=10,
				truncate.tallest=TRUE,
				main="truncate.tallest=TRUE")
}
\keyword{ models } 
