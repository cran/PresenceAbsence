\name{PresenceAbsence-package}
\alias{ PresenceAbsence-package}
\alias{ PresenceAbsence }
\docType{package}
\title{
Presence-Absence model evaluation 
}
\description{
Provides a set of functions useful when evaluating the results of presence-absence models. Package includes functions for calculating threshold dependent measures such as confusion matrices, pcc, sensitivity, specificity, and Kappa, and produces plots of each measure as the threshold is varied. It also includes functions to plot the threshold independent ROC curves along with the associated AUC (area under the curve).
}
\details{
\tabular{ll}{
Package: \tab PresenceAbsence\cr
Type: \tab Package\cr
Version: \tab 1.1.11\cr
Date: \tab 2023-01-05\cr
License: \tab This code was written and prepared by a U.S. Government employee on official time, and therefore it is in the public domain and not subject to copyright.\cr
}
This library provides a collection of functions useful for evaluating Presence/Absence data, both analytically as well as graphically. It also includes a function that uses beta distributions to produce simulated Presence/Absence data.

Data should be in the form of a matrix or data frame where each row represents one data point or plot location, and where column 1 is plot ID, column 2 is observed values, column 3 is predictions from the first model, column 4 is predictions from the second model, etc...

If the observed values are not already in the form of zero/one values (i.e. they are instead measurements such as basal area or tree counts) the functions will automatically translate them into zero-one values where any number greater than zero is treated as Present.

The library is most useful if the predictions are in the form of probabilities. This allows one to investigate how the model predictions vary as the threshold is varied. If all that is available is predicted Presence/Absence values, the summary statistics can still be calculated, but most of the graphs are not possible.

Functions that will still work when all that is available is predicted Presence/Absence: \link{cmx}, \link{pcc}, \link{sensitivity}, \link{specificity}, \link{Kappa}, \link{presence.absence.accuracy} with \code{find.auc} set to false, \link{predicted.prevalence}, and the graphical function \link{presence.absence.hist} with \code{N.bar} set to 2.

Most functions take the dataframe of observed and predicted values (\code{DATA}) as input. The exceptions are the sub-functions that calculate single accuracy statistics: \link{pcc}, \link{sensitivity}, \link{specificity}, and \link{Kappa}. These sub-functions take the confusion matrix from \link{cmx} as input.

Some functions only evaluate one set of model predictions at a time, while others will work with multiple sets of model predictions. Even if the function only works on single models, the dataframe \code{DATA} can still contain multiple model predictions. Just use the argument \code{which.model} to indicate the desired column. 

Functions that will only work on single models: \link{cmx}, \link{auc}, \link{roc.plot.calculate}, \link{presence.absence.hist}, \link{error.threshold.plot}, \link{calibration.plot}, and \link{presence.absence.summary}.

Functions that will work with multiple models: \link{presence.absence.accuracy}, \link{optimal.thresholds}, \link{predicted.prevalence}, and \link{auc.roc.plot}.

Note that this library provides graphical and tabular comparisons between models. It does not provide significance testing of model differences. The standard deviations given by \link{presence.absence.accuracy} are for each model individually. To test AUC for differences between models it is necessary to account for correlation. If you are interested in AUC significance testing, both pair-wise and overall, the Splus ROC library from Mayo clinic provides such a test. See \link{auc} for more details.

This code was written and prepared by a U.S. Government employee on official time, and therefore it is in the public domain and not subject to copyright.
}
\author{
Author:Elizabeth Freeman <elizabeth.a.freeman@usda.gov>

Maintainer: Elizabeth Freeman <elizabeth.a.freeman@usda.gov>
}
\references{
Fielding, A.H. and Bell, J.F., 1997. A review of methods for the assessment of prediction errors in conservation presence/absence models. Environ. Conserv., 24(1):38-49.

Manel, S., Ceri Williams, H., and Ormerod, S.J., 2001. Evaluating presence/absence models in ecology: the need to account for prevalence. J. Appl. Ecol., 38:921-931.

Moisen, G.G., Freeman, E.A., Blackard, J.A., Frescino, T.S., Zimmerman N.E., Edwards, T.C. Predicting tree species presence and basal area in Utah: A comparison of stochastic gradient boosting, generalized additive models, and tree-based methods. Ecological Modellng, 199 (2006) 176-187.
}
\keyword{ package }

\examples{
data(SIM3DATA)
auc.roc.plot(SIM3DATA)
presence.absence.summary(SIM3DATA,which.model=1)
}
