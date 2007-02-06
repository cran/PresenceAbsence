\name{cmx}
\alias{cmx}
\title{ Confusion Matrix }
\description{
  \code{cmx} calculates the confusion matrix for a single model.
}
\usage{
cmx(DATA, threshold = 0.5, which.model = 1, na.rm = FALSE)
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
  \item{threshold}{a cutoff value between zero and one used for translating predicted probabilities into 0 /1 values, defaults to 0.5. It must be a single value between zero and one.}
  \item{which.model}{a number indicating which model from DATA should be used}
  \item{na.rm}{ a logical indicating whether missing values should be removed}
}
\details{


 \code{cmx} calculates the confusion matrix for a single model at a single threshold. 

If \code{DATA} contains more predictions from more than one model  \code{WHICH.DATA} can be used to specify which model should be used. If \code{WHICH.DATA} is not given, \code{cmx} will use predictions from the first model by default.

When calculating the confusion matrix, any plot with a predicted probability greater than \code{threshold} is considered to be predicted \code{Present}, while any plot with a predicted probability less than or equal to \code{threshold} is considered to be predicted \code{Absent}. The only exception is when \code{threshold} equals zero. In that case, all plots are considered to be predicted \code{Present}.

Unlike other functions in this library, \code{threshold} can not be a vector or an integer greater than one. Instead, \code{threshold} must be given as a single number between zero and one.

If \code{na.rm} equals \code{FALSE} and \code{NA}'s are present in the \code{DATA} function will return \code{NA}.

If \code{na.rm} equals \code{TRUE} and \code{NA}'s are present in the \code{DATA}, function will remove all rows where any of the values in the row consist of \code{NA}. Function will also print the number of rows that have been removed. 

}
\value{
the confusion matrix is returned in the form of a table where:
	\item{columns}{observed values}
	\item{rows}{predicted values}
}
\references{  }
\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\seealso{
\code{\link{pcc}}, \code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{Kappa}}
}
\examples{

### EXAMPLE 1 ###
     ### generate simulated data ###
     set.seed(666)
     N=1000
     SIMDATA<-matrix(0,N,3)
     SIMDATA<-as.data.frame(SIMDATA)
     names(SIMDATA)<-c("plotID","Observed","Predicted")
     SIMDATA$plotID<-1:N
     SIMDATA$Observed<-rbinom(n=N,size=1,prob=.2)
     SIMDATA$Predicted[SIMDATA$Observed==1]<-rnorm(n=length(SIMDATA$Observed[SIMDATA$Observed==1]),mean=.8,sd=.15)
     SIMDATA$Predicted[SIMDATA$Observed==0]<-rnorm(n=length(SIMDATA$Observed[SIMDATA$Observed==0]),mean=.2,sd=.15)
     SIMDATA$Predicted<-(SIMDATA$Predicted-min(SIMDATA$Predicted))/(max(SIMDATA$Predicted)-min(SIMDATA$Predicted))

     ### plot simulated data
     hist(SIMDATA$Predicted,100)

     ### calculate confusion matrix ###
     cmx(SIMDATA)

### EXAMPLE 2 ###

     data(SIM3DATA)

     cmx(SIM3DATA)
     cmx(SIM3DATA,which.model=2)
     cmx(SIM3DATA,which.model=3,threshold=.2)

}
\keyword{ models }
