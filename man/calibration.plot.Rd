\name{calibration.plot}
\alias{calibration.plot}
\title{ Calibration Plot }
\description{
  \code{calibration.plot} produces a goodness-of-fit plot for Presence/Absence data.
}
\usage{
calibration.plot(DATA, which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, xlab = "Predicted Probability of Occurrence", ylab = "Observed Occurrence as Proportion of Sites Surveyed", main = NULL, color= NULL, model.names= NULL)
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
  \item{alpha}{ alpha value for confidence intervals }
  \item{N.bins}{ number of bins to split predicted probabilities into }
  \item{xlab}{ a title for the x axis}
  \item{ylab}{ a title for the y axis}
  \item{main}{ an overall title for the plot }
  \item{color}{ a logical or a vector of color codes }
  \item{model.names}{ a vector of the names of each model included in \code{DATA}}

}
\details{
Takes a single model and creates a goodness-of-fit plot of observed verses predicted values. The plots are grouped into bins based on their predicted values, and then the bin prevalence (the ratio of plots in this bin with observed values of present verses the total number of plots in this bin) is calculated for each bin. The confidence interval for each bin is also plotted, and the total number of plots is labeled above each the bin.

Confidence intervals are calculated for the binomial bin counts using the F distribution.

Unlike a typical goodness-of-fit plot from a linear regression model, with Presence/Absence data having all the points lay along the diagonal does not necessarily imply a good quality model. The ideal calibration plot for Presence/Absence data depends on the intended use of the model.

If the model is to be used to produce probability maps, then it is indeed desirable that (for example) 80 percent of plots with predicted probability of 0.8 actually do have observed Presence. In this case, having all the bins along the diagonal does indicate a good model.

However, if model is to be used simply to predict species presence, then all that is required is that some threshold exists (not necessarily 0.5) where every plot with a lower predicted probability is observed Absent, and every plot with a higher predicted probability is observed Present. In this case, a good model will not necessarily (in fact, will rarely) have all the bins along the diagonal. (Note: for this purpose \code{presence.absence.hist} may produce more useful diagnostics.)

If all the bins lie above the diagonal, or all the bins lie below the diagonal, it may indicate that the training and test datasets have different prevalence. In this case, it may be worthwhile to re-examine the initial data selection.

}
\value{
creates a graphical plot 

returns a dataframe of information about the bins where:
\tabular{lll}{
  \code{[,1]} \tab \code{BinCenter} \tab center of bin \cr
  \code{[,2]} \tab \code{NBin} \tab Number of plots in Bin \cr
  \code{[,3]} \tab \code{BinObs} \tab Proportion of Bin observed as Present \cr
  \code{[,4]} \tab \code{BinPred} \tab Average prediction for Bin \cr
  \code{[,5]} \tab \code{BinObsCIlower} \tab Lower bound of confidence Interval for \code{BinObs} \cr
  \code{[,6]} \tab \code{BinObsCIupper} \tab Upper bound of confidence Interval for \code{BinObs}  \cr}

}

\note{ 
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us}  }

\references{ 
Vaughan, I. P., Ormerod, S. J. 2005. The continuing challenges of testing species distribution models.  J. Appl. Ecol., 42:720-730.

Reineking, B. and \enc{Schröder}{Schroder}, B. 2006. Constrain to perform: regularization of habitat models. Ecological Modelling 193: 675-690.
 }


\seealso{ \link{presence.absence.summary}, \link{presence.absence.hist} }
\examples{
data(SIM3DATA)

calibration.plot(SIM3DATA)

calibration.plot(	DATA=SIM3DATA,
			which.model=3,
			na.rm=TRUE,
			alpha=0.05,
			N.bins=10,
			xlab="Predicted Probability of Occurence",
			ylab="Observed occurence as proportion of sites surveyed",
			model.names=NULL,
			main=NULL)
}
\keyword{ models } 
\encoding{latin1}
