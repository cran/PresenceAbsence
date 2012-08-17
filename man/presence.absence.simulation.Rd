\name{presence.absence.simulation}
\alias{presence.absence.simulation}
\title{Presence/Absence Data Simulation}
\description{
  \code{presence.absence.simulation} simulates presence/absence data as one set of observed values, and one or more prediction models. First, Observed values are generated as a binomial distribution, then for each model two beta distributions are used to generate predicted values, one beta distribution for the data points where the simulated observed value is present, and a second for points where it is absent. 
}
\usage{
presence.absence.simulation(n, prevalence, N.models = 1, shape1.absent, shape2.absent, shape1.present, shape2.present)
}
\arguments{
  \item{n}{ number of plots (i.e. rows) in simulated dataset }
  \item{prevalence}{ probability species is present for binomial observed values }
  \item{N.models}{ number of models to simulate predictions for }
  \item{shape1.absent}{ first parameter for beta distribution for plots where observed value is absent }
  \item{shape2.absent}{ second parameter for beta distribution for plots where observed value is absent }
  \item{shape1.present}{ first parameter for beta distribution for plots where observed value is present }
  \item{shape2.present}{ second parameter for beta distribution for plots where observed value is present }
}
\details{
  \code{presence.absence.simulation} will generate predicted probabilities for one or more models. If \code{N.models} = 1, then shape parameters should be of length 1. If \code{N.models} > 1, then shape parameters can be either length 1 or vectors of length \code{N.models}.

The beta distribution is extremely flexible and is capable of generating data with unrealistic behavior. The following rules of thumb will help generate realistic datasets:

The mean of the beta distribution equals shape1/(shape1+shape2). To get reasonable predictions (e.g. better than random), the mean for the plots where the observed value is present should be higher than that for the plots where the species is absent:

	mean(present) > mean(absent)

The overall mean probability should be approximately equal to the prevalence. In other words:

	prevalence*mean(present) + (1-prevalence)*mean(absent) = prevalence
 
}
\value{
  \code{presence.absence.simulation} returns a dataframe where:
	\item{column 1}{\code{plotID} - plot ID numbers}
	\item{column 2}{\code{Observed} - 0/1 values}
	\item{column 3}{\code{Predicted 1} - predicted probabilities for model 1}
	\item{column 4}{\code{Predicted 2} - predicted probabilities for model 2, etc...}
}

\author{ Elizabeth Freeman \email{eafreeman@fs.fed.us} }

\examples{
### EXAMPLE 1 ###
### a graph illustrating effect of shape parameters on beta distribution

set.seed(666)
shapes<-c(1,2,5,10,20)
par(mfrow=c(5,5),mar=c(2,2,2,2),oma=c(0,3,3,0))

for(i in 1:5){
for(j in 1:5){
     SIMDATA<-presence.absence.simulation( n=1000,
                                           prevalence=1,
                                           N.models=1,
                                           shape1.absent=1,
                                           shape2.absent=1,
                                           shape1.present=shapes[i],
                                           shape2.present=shapes[j])
	#Note: by setting prevalence=1, all observed values will be 'present' 
	#	 therefore only one beta distribution will be simulated.	
	hist(SIMDATA[,3],breaks=50,main="",xlab="",ylab="",xlim=c(0,1))
	if(i==1){mtext(paste("shape2 =",shapes[j]),side=3,line=2,cex=.8)}
	if(j==1){mtext(paste("shape1 =",shapes[i]),side=2,line=3,cex=.8)}
}}

### EXAMPLE 2 ###
### generate observed data along with 3 sets of model predictions 
### for models of varying predictive ability.
### Note: This is the code used to generate sample dataset SIM3DATA.

set.seed(666)
SIM3DATA<-presence.absence.simulation(	n=1000,
							prevalence=.2,
							N.models=3,
							shape1.absent=c(1,1,1),
							shape2.absent=c(14,7,5), 
							shape1.present=c(6,2,1),
							shape2.present=c(2,2,2))
}
\keyword{ datagen }
