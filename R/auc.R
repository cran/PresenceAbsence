
auc <- function(DATA,st.dev=TRUE,which.model=1,na.rm=FALSE){
### Calculates the area under the curve approximated with a Mann-Whitney U
### statistic and its standard deviation. This is a simplification of 
### a function from the Splus ROC library from Mayo clinic.
###
### Note: the standard errors are for comparing an individual model to random
###       assignment (i.e. AUC=.5), to compare two models to each other it is 
###       necessary to account for correlation due to the fact that they 
###       use the same test set.
###
### DATA is a matrix (or dataframe) of observed and predicted values where: 
### 	the first column is the plot id, 
###   the second column is the observed values (either 0/1 or actual values),
###   the remaining columns are the predicted probabilities for the model.
###
###	DATA		matrix nrow=number of plots,
###				col1=PLOTID
###				col2=observed   (0 / 1)
###			  	col3=prediction probabilities from first model
###			  	col4=prediction probabilities from second model, etc...
###	st.dev	should the standard deviation be calculated
###	which.model a number indicating which model in DATA should be used for
###			calculating the confusion matrix
###	na.rm		should rows containing NA's be removed from the dataset
###			NOTE:	if ra.rm=FALSE, and NA's are present, 
###				function will return NA

### check logicals

if(is.logical(st.dev)==FALSE){
	stop("'st.dev' must be of logical type")}

if(is.logical(na.rm)==FALSE){
	stop("'na.rm' must be of logical type")}

### check for and deal with NA values:

	if(sum(is.na(DATA))>0){
     		if(na.rm==TRUE){
            	NA.rows<-apply(is.na(DATA),1,sum)
            	warning(      length(NA.rows[NA.rows>0]),
                         	  " rows ignored due to NA values")
           		DATA<-DATA[NA.rows==0,]
      	}else{return(NA)}}

### Check that 'which.model' is a single integer, and not greater than number of models in DATA

	if(length(which.model)!=1){
		stop("this function will only work for a single model, 'which.model' must be of length one")}
	if(which.model<1 || round(which.model)!=which.model){
		stop("'which.model' must be a positive integer")}
	if(which.model+2 > ncol(DATA)){
		stop("'which.model' must not be greater than number of models in DATA")}

### Pull out data from single model

	DATA<-DATA[,c(1,2,which.model+2)]
    	
### translate observations from values to presence/absence

	DATA[DATA[,2]>0,2]<-1

### begin AUC calculations

        OBS <- DATA[,2]
        PRED <- DATA[,3]

### check for impossible OBS

	  if(length(OBS[OBS==1])==0 || length(OBS[OBS==1])==nrow(DATA)){
		if(st.dev==FALSE){return(NaN)
		}else{return(data.frame(AUC=NaN,AUC.sd=NaN))}}
	  rm(DATA)

### continue auc calculations
        x1 <- (PRED[OBS==1])
        x0 <- (PRED[OBS==0])
        num1 <- sum(OBS)
        num0 <- length(OBS) - sum(OBS)
        eps <- 10^(-16)
        rm(PRED)
        rm(OBS)

### creates num0 by num1 matrix
          diffarray <- outer(x1,rep(1,num0)) - outer(rep(1,num1),x0)
          rm(x1)
          rm(x0)
          countmat <- sum(diffarray>eps) + 0.5*sum(abs(diffarray)<=eps)

### note: Mann-Whitney U statistic is equal to (num1*num0) - countmat   
  
        area <- countmat/(num1*num0)
        area[area<.5] <- 1 - (countmat/(num1*num0))[area<.5]
        rm(countmat)
  
  if(st.dev==FALSE){
		return(auc=area)
   }else{

###Calculate the S matrix
       countmtx <- (sign(diffarray)+1)/2
       rm(diffarray)
       V.x1 <- apply(countmtx,1,sum)/num0
       V.x0 <- apply(countmtx,2,sum)/num1
       rm(countmtx)
     	 S.1 <- var(V.x1)
       rm(V.x1)
     	 S.0 <- var(V.x0)
       rm(V.x0)
     	 S <- (S.1/num1) + (S.0/num0)
###Calculate the standard error of the area under the curve
   	 var.area <- S
   	 se.area <- sqrt(var.area)

       return(data.frame(AUC=area,AUC.sd=se.area))}
}

