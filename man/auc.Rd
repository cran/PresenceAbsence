\name{auc}
\alias{auc}
\title{ Area Under the Curve }
\description{
 \code{auc} calculates the area under the ROC curve approximated with a Mann-Whitney U statistic, and (optionally) the associated standard deviation. 
}
\usage{
auc(DATA, st.dev = TRUE, which.model = 1, na.rm = FALSE)
}
\arguments{
  \item{DATA}{a matrix or dataframe of observed and predicted values where each row represents one plot and where columns are:
   	\tabular{llllll}{
	  \tab \tab \tab DATA[,1] \tab plot ID \tab text\cr
	  \tab \tab \tab DATA[,2] \tab observed values \tab zero-one values\cr
	  \tab \tab \tab DATA[,3] \tab predicted probabilities from first model \tab numeric (between 0 and 1)\cr
	  \tab \tab \tab DATA[,4] \tab predicted probabilities from second model, etc... \tab  
	}
  }
  \item{st.dev}{ a logical indicating whether the associated standard deviation should be calculated }
  \item{which.model}{a number indicating which model from DATA should be used}
  \item{na.rm}{ a logical indicating whether missing values should be removed}
}
\details{
\code{auc} approximates the area under the ROC curve with a Mann-Whitney U statistic (Delong et al., 1988) to calculate the area under the curve.

The standard errors from \code{auc} are only valid for comparing an individual model to random assignment (i.e. AUC=.5). To compare two models to each other it is necessary to account for correlation due to the fact that they use the same test set. If you are interested in pair wise model comparisons see the Splus ROC library from Mayo clinic. \code{auc} is a much simpler function than what is available from the Splus ROC library from Mayo clinic. 

The observed values (column 2 in \code{DATA}) can be given as 0/1 values to represent \code{absence} and \code{presence}. If this column contains actual values (i.e. basal area, biomass, etc...), any value of zero will be treated as \code{absence} and any value greater than zero will be treated as \code{presence}.

If observed values are all the same, in other words, if the data consists entirely of observed Presences or entirely of observed Absences, \code{auc} will return \code{NaN}.
}
\value{
  if \code{st.dev} = FALSE, returns: \code{AUC} area under the curve.\cr

  if \code{st.dev} = TRUE, returns a dataframe where:
\tabular{lll}{
[1,1] \tab \code{AUC}   \tab area under the curve \cr
[1,2] \tab \code{AUC.sd}\tab standard deviation of \code{AUC}}
}
\references{ 
DeLong, E.R., Delong, D.M. and Clarke-Pearson, D.L., 1988. Comparing areas under two or more correlated Receiver Operating Characteristic curves: a nonparametric approach. Biometrics, 44(3):837-845.

Splus ROC library developed by Beth Atkinson and Doug Mahoney at the Mayo Clinic is available at: \url{http://www.stats.ox.ac.uk/pub/MASS3/Winlibs/} for windows.
}
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{ 
\code{\link{cmx}}, \code{\link{pcc}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{Kappa}}, \code{\link{auc.roc.plot}}
}
\examples{
data(SIM3DATA)

auc(SIM3DATA)

auc(SIM3DATA,st.dev=FALSE,which.model=2)
}
\keyword{ models }
