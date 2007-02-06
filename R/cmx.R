
cmx<-function(DATA,threshold=0.5,which.model=1,na.rm=FALSE){
### Calculates the confusion matrix for a single model.
###
### DATA is a matrix (or dataframe) of observed and predicted values where: 
###   the first column is the plot id, 
###   the second column is the observed values (either 0/1 or actual values),
###   the remaining columns are the predicted probabilities for the model.
###
###   DATA        matrix nrow=number of plots,
###                     col1=PLOTID
###                     col2=observed (0 / 1)
###                     col3=prediction probabilities from first model
###                     col4=prediction probabilities from second model, etc...
###   threshold   a cutoff value between zero and one used for translating 
###               predicted probabilities into 0 /1 values, defaults to 0.5
###               It can be either a single value or a vector of the same length as
###               the number of models in DATA. Note: unlike other functions in this
###               library, it can not be an integer greater than one.
###   which.model a number indicating which model in DATA should be used for
###               calculating the confusion matrix
###   na.rm       should rows containing NA's be removed from the dataset
###               NOTE: if ra.rm=FALSE, and NA's are present, 
###                     function will return NA

### check logicals

if(is.logical(na.rm)==FALSE){
	stop("'na.rm' must be of logical type")}

### check for and deal with NA values:

	if(sum(is.na(DATA))>0){
     		if(na.rm==TRUE){
            	NA.rows<-apply(is.na(DATA),1,sum)
            	warning(	length(NA.rows[NA.rows>0]),
					" rows ignored due to NA values")
           		DATA<-DATA[NA.rows==0,]
      	}else{return(NA)}}

### Check that which.model is a single integer, and not greater than number of models in DATA
	if(length(which.model)!=1){
		stop("this function will only work for a single model, 'which.model' must be of length one")}
	if(which.model<1 || round(which.model)!=which.model){
		stop("'which.model' must be a positive integer")}
	if(which.model+2 > ncol(DATA)){
		stop("'which.model' must not be greater than number of models in 'DATA'")}

### translate observations from values to presence/absence

	DATA[DATA[,2]>0,2]<-1

### Check that 'threshold' is valid

	if(length(threshold)!=1){
		stop("'threshold' must be a single number between zero and one")}
		 
	if(max(threshold)>1){
		stop("'threshold' must be a single number between zero and one")}

	if(min(threshold)<0){
		stop("'threshold' must be a single number between zero and one")}

### Pull out data from single model

	DATA<-DATA[,c(1,2,which.model+2)]

### Calculate confusion matrix

	PRED.ind<-rep(0,nrow(DATA))

	if(threshold==0){
		PRED.ind[DATA[,3]>=threshold]=1
	}else{
		PRED.ind[DATA[,3]>threshold]=1}
	return(table(	predicted=factor(PRED.ind,levels=c(1,0)),
				observed=factor(DATA[,2],levels=c(1,0))	)	)
}
