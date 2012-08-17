\name{optimal.thresholds}
\alias{optimal.thresholds}
\title{ Calculate Optimal Thresholds }
\description{
  \code{optimal.thresholds} calculates optimal thresholds for Presence/Absence data by any of several methods.
}
\usage{
optimal.thresholds(DATA = NULL, threshold = 101, which.model = 1:(ncol(DATA)-2), model.names = NULL, na.rm = FALSE, opt.methods = NULL, req.sens, req.spec, obs.prev = NULL, smoothing = 1, FPC, FNC)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
     \tabular{llllll}{
	  \tab \tab \tab \code{DATA[,1]} \tab plot ID \tab text\cr
	  \tab \tab \tab \code{DATA[,2]} \tab observed values \tab zero-one values\cr
	  \tab \tab \tab \code{DATA[,3]} \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab \code{DATA[,4]} \tab predicted probabilities from second model, etc... \tab  }}
  \item{threshold}{cutoff values between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. It can be a single value between zero and one, a vector of values between zero and one, or a positive integer representing the number of evenly spaced thresholds to calculate. To get reasonably good optimizations, there should be a large number of thresholds.}
  \item{which.model}{a number or vector indicating which models from \code{DATA} should be used}
  \item{model.names}{ a vector of the names of each model included in \code{DATA} to be used as column names}
  \item{na.rm}{a logical indicating whether missing values should be removed}
  \item{opt.methods}{ what methods should be used to optimize thresholds. Given either as a vector of method names or method numbers. Possible values are:
  \tabular{rll}{
  1  \tab \code{Default}   \tab threshold=0.5 \cr
  2  \tab \code{Sens=Spec} \tab sensitivity=specificity \cr
  3  \tab \code{MaxSens+Spec} \tab maximizes (sensitivity+specificity)/2 \cr
  4  \tab \code{MaxKappa}  \tab maximizes Kappa \cr
  5  \tab \code{MaxPCC}    \tab maximizes PCC (percent correctly classified) \cr
  6  \tab \code{PredPrev=Obs} \tab predicted prevalence=observed prevalence \cr
  7  \tab \code{ObsPrev}   \tab threshold=observed prevalence \cr
  8  \tab \code{MeanProb} \tab mean predicted probability \cr 
  9  \tab \code{MinROCdist}\tab minimizes distance between ROC plot and (0,1)\cr
  10 \tab \code{ReqSens} \tab user defined required sensitivity \cr
  11 \tab \code{ReqSpec} \tab user defined required specificity \cr
  12 \tab \code{Cost} \tab user defined relative costs ratio }}
  \item{req.sens}{ a value between zero and one giving the user defined required sensitivity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1-maximum allowable errors for points with positive observations).}
  \item{req.spec}{ a value between zero and one giving the user defined required sspecificity. Only used if \code{opt.thresholds} = \code{TRUE}. Note that \code{req.sens} = (1- maximum allowable errors for points with negative observations).}
  \item{obs.prev}{observed prevalence for \code{opt.method} = \code{"PredPrev=Obs"} and \code{"ObsPrev"}. Defaults to observed prevalence from \code{DATA}.}
  \item{smoothing}{ smoothing factor for maximizing/minimizing. Only used if \code{opt.thresholds} = \code{TRUE}. Instead of find the threshold that gives the max/min value, function will average the thresholds of the given number of max/min values.}
  \item{FPC}{ False Positive Costs, or for C/B ratio C = 'net costs of treating nondiseased individuals'.}
  \item{FNC}{ False Negative Costs, or for C/B ratio B = 'net benefits of treating diseased individuals'.}
}
\details{
The 'opt.methods' argument is allows the user to choose optimization
methods.  The methods can be specified by number (\code{opt.methods} = \code{1:12} or \code{opt.methods} = \code{c(1,2,4)}) or by name (\code{opt.methods} = \code{c("Default","Sens=Spec","MaxKappa")}).

There are currently twelve optimization criteria available:

\code{"Default"} First, the default criteria of setting 'threshold = 0.5'

\code{"Sens=Spec"} The second criteria for optimizing threshold choice is by finding the threshold where sensitivity equals specificity. In other words, find the threshold where positive observations are just as likely to be wrong as negative observations. 

Note: when threshold is optimized by criteria \code{"Sens=Spec"} it is correlated to prevalence, so that rare species are given much lower thresholds than widespread species. As a result, rare species may give the appearance of inflated distribution, if maps are made with thresholds that have been optimized by this method (Manel, 2001).

\code{"MaxSens+Spec"} The third criteria chooses the threshold that maximizes the sum of sensitivity and specificity. In other words, it is minimizing the mean of the error rate for positive observations and the error rate for negative observations. This is equivalent to maximizing (sensitivity + specificity - 1), otherwise know as the Youden's index, or the True Skill Statistic. Note that while Youden's Index is independent of prevalence, using Youden's index to select a threshold does have an effect on the predicted prevalence, causing the distribution of rare species to be over predicted.

\code{"MaxKappa"} The forth criteria for optimizing the threshold choice is to find the threshold that gives the maximum value of Kappa. Kappa makes full use of the information in the confusion matrix to asses the improvement over chance prediction.

\code{"MaxPCC"} The fifth criteria is to maximize the total accuracy (PCC - Percent Correctly Classified).

Note: It may seem like maximizing total accuracy would be the obvious goal, however, there are many problems with using PCC to assess model accuracy. For example, with species with very low prevalence, it is possible to maximize PCC simply by declaring the species a absent at all locations -- not a very useful prediction!

\code{"PredPrev=Obs"} The sixth criteria is to find the threshold where the Predicted prevalence is equal to the Observed prevalence. This is a useful method when preserving prevalence is of prime importance.

\code{"ObsPrev"} The seventh criteria is an even simpler variation, where you simply set the threshold to the Observed prevalence. It is nearly as good as method six at preserving prevalence and requires no computation.

\code{"MeanProb"} The eighth criteria also requires no threshold computation. Method eight sets the threshold to the mean probability of occurrence from the model results.

\code{"MinROCdist"} The ninth criteria is to find the threshold that minimizes the distance between the ROC plot and the upper left corner of the unit square.

\code{"ReqSens"} The tenth criteria allows the user to set a required sensitivity, and then finds the highest threshold that will meet this requirement. In other words, the user can decide that the model must miss no more than, for example 15 percent of the plots where the species is observed to be present. Therefore they require a sensitivity of at least 0.85. This may be useful if, for example, the goal is to define a management area for a rare species, and they want to be certain that the management area doesn't leave unprotected too many populations.

\code{"ReqSpec"} The eleventh criteria allows the user to set a required specificity, and then finds the lowest threshold that will meet this requirement. In other words, the user can decide that the model must miss no more than, for example 15 percent of the plots where the species is observed to be absent. Therefore they require a specificity of at least 0.85. This may be useful if, for example, the goal is to determine if a species is threatened, and they want to be certain not to over inflate the population by over declaring true absences as predicted presences.

Note: for \code{"ReqSens"} and \code{"ReqSpec"}, if your model is poor, and your requirement is too strict, it is possible that the only way to meet it will be by declaring every single plot to be Present (for ReqSens) or Absent (for ReqSpec) -- not a very useful method of prediction! Conversely, if the model is good, and the requirement too lax, the resulting thresholds will result in unnecessary levels on inaccuracy. If a threshold exists where sensitivity equals specificity at a value greater than the required accuracy, then the user can raise their required specificity (or sensitivity) without sacrificing sensitivity (or specificity).

\code{"Cost"} The twelth criteria balances the relative costs of false positive predictions and false negative predictions. A slope is calculated as (FPC/FNC)((1 - prevalence)/prevalence). To determine the threshold, a line of this slope is moved from the top left of the ROC plot, till it first touches the ROC curve.

Note: the criteria \code{"Cost"} can also be used for C/B ratio analysis of diagnostic tests. In this case \code{FPC} = \code{C} (the net costs of treating nondiseased individuals) and \code{FNC} = \code{B} (the net benafits of treating diseased individuals). For further information on \code{"Cost"} see Wilson et. al. (2005) and Cantor et. al. (1999).

For all the criteria that depend on observed prevalence (\code{"PredPrev=Obs"}, \code{"ObsPrev"} and \code{cost}) , the default is to use the observed prevalence from \code{DATA}. However, the argument \code{obs.prev} can be used to substiture a predetermined value for observed prevalence, for example, the prevalence from a larger dataset.

\code{error.threshold.plot} is a rough and ready function. It optimizes thresholds simply by calculating a large number of evenly spaced thresholds and looking for the best ones. This is good enough for graphs, but to find the theoretically 'best' thresholds, would require calculating every possible unique threshold (not necessarily evenly spaced!).

Details on \code{smoothing} argument: when the statistic being maximized (e.g. Kappa) is relatively flat but erratic, just picking the threshold that gives single maximum value is somewhat arbitrary. \code{smoothing} compensates for this by taking an average of the thresholds that give a set number of the highest values (e.g. the 10 highest Kappa's, or the 20 highest Kappa's).
}
\value{
  If \code{DATA} is not provided function will return a vector of the possible optimization methods. 

  Otherwise, returns a dataframe where:
\tabular{ll}{
  \code{[,1]} \tab \code{Method} - names of optimization methods \cr
  \code{[,2]} \tab optimal thresholds for the first model\cr
  \code{[,3]} \tab optimal thresholds for the second model, etc...}
}
\references{ 
S.B. Cantor, C.C. Sun, G. Tortolero-Luna, R. Richards-Kortum, and M. Follen. A comparison of C/B ratios from studies using receiver operating characteristic curve analysis. Journal of Clinical Epidemiology, 52(9):885-892, 1999.

S. Manel, H.C. Williams, and S.J. Ormerod. Evaluating presence-absence models in ecology: the need to account for prevalence. Journal of Applied Ecology, 38:921-931, 2001.
K.A. Wilson, M.I. Westphal, H.P. Possingham. and J. Elith. Sensitivity of conservation planning to different approaches to using predicted species distribution data. Biological Conservation, 22(1):99-112, 2004.
}
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{ \link{error.threshold.plot}, \link{presence.absence.accuracy}, \link{roc.plot.calculate}, \link{presence.absence.summary} }
\examples{
data(SIM3DATA)
optimal.thresholds(SIM3DATA)
}
\keyword{ models } 
