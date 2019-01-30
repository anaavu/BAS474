# First, read in all data:
source("http://ballings.co/hidden/aCRM/code/chapter2/read_data_sets.R")

if (!require("AUC")) {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}

#Fast Nearest Neighbor Search Algorithms and Applications
if (!require("FNN")) {
  install.packages('FNN',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('FNN')
}

# The main function we will be using is the knnx.index function.
# It find the k-nearest neighbors.The knnx function requires 
# all indicators to be numeric so we first convert our data. 
# In this case they are already numeric, but this is an example
# of how to do it:
trainKNN <- data.frame(sapply(BasetableTRAIN, function(x) as.numeric(as.character(x))))
trainKNNbig <- data.frame(sapply(BasetableTRAINbig, function(x) as.numeric(as.character(x))))   
valKNN <- data.frame(sapply(BasetableVAL, function(x) as.numeric(as.character(x))))
testKNN <- data.frame(sapply(BasetableTEST, function(x) as.numeric(as.character(x))))

?sapply
?knnx.index

# The distance function (e.g., Euclidean distance) is sensitive
# to the scale of the variables. For example, if we have the 
# variable 'frequency' measured as number of shop visits, and
# monetary value in dollars the latter will have a higher 
# influence on the distance measured. Therefore we need to 
# standardize the variables first.

stdev <- sapply(trainKNN,sd)
means <- sapply(trainKNN,mean)

trainKNNbig <- data.frame(t((t(trainKNNbig)-means)/stdev))
trainKNN <- data.frame(t((t(trainKNN)-means)/stdev))
valKNN <- data.frame(t((t(valKNN)-means)/stdev))
testKNN <- data.frame(t((t(testKNN)-means)/stdev))

#note that all computations take place in the prediction phase

#example for 10 nearest neighbors:
k <- 10
#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=trainKNNbig, 
                                       query=testKNN, 
                                       k=k))
#retrieve the actual y from the training set
predKNN <- as.integer(as.character(yTRAINbig[indicatorsKNN]))
#if k > 1 then we take the proportion of 1s
predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                      ncol=k,
                                      nrow=nrow(testKNN))))

auc(roc(predKNN,yTEST))
#if we want to tune:
#tuning comes down to evaluating which value for k is best
auc <- numeric()
for (k in 1:nrow(trainKNN)) {
  #retrieve the indicators of the k nearest neighbors of the query data 
  indicatorsKNN <- as.integer(knnx.index(data=trainKNN, 
                                         query=valKNN, 
                                         k=k))
  #retrieve the actual y from the training set
  predKNN <- as.integer(as.character(yTRAIN[indicatorsKNN]))
  #if k > 1 then we take the proportion of 1s
  predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                        ncol=k,
                                        nrow=nrow(valKNN))))
  
  #COMPUTE AUC 
  auc[k] <- AUC::auc(roc(predKNN,yVAL))
  
  #Print progress to the screen
  if((k %% max(floor(nrow(trainKNN)/10),1))==0) 
    cat(round((k/nrow(trainKNN))*100),"%\n")
}

plot(1:nrow(trainKNN),auc,type="l", xlab="k")
#very low values of k result in a very flexible classifier: low bias, high variance
#very high values of k result a very inflexbile classifier: high bias, low variance
#when k equals the the number of training instances then all response values are selected once per new (i.e., validation) data point. 
#Then all values of predKNN will have mean(as.integer(as.character(yTRAIN))).

#the next step would be to train again on trainKNNbig using the best value of k and predict on testKNN


(k <- which.max(auc))
#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=trainKNNbig, 
                                       query=testKNN, 
                                       k=k))
#retrieve the actual y from the tarining set
predKNNoptimal <- 
  as.integer(as.character(yTRAINbig[indicatorsKNN]))
#if k > 1 then we take the proportion of 1s
predKNNoptimal <- 
  rowMeans(data.frame(matrix(data=predKNNoptimal,
                             ncol=k,
                             nrow=nrow(testKNN))))

auc(roc(predKNNoptimal,yTEST))
plot(roc(predKNNoptimal,yTEST))
?roc
