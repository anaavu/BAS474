rm(list=ls())
source("http://ballings.co/hidden/aCRM/code/chapter2/read_data_sets.R")

#check what is in our environment
ls()

#load the package AUC to evaluate model performance
if(require('AUC')==FALSE)  {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}

#load the package rpart to create decision  trees
if(require('rpart')==FALSE)  {
  install.packages('rpart', 
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE)
  require('rpart')
}

#Create one tree:
tree <- rpart(yTRAIN ~ ., BasetableTRAIN)
predTREE <- predict(tree,BasetableTEST)[,2]

AUC::auc(roc(predTREE,yTEST))

#######################################################################

#Let's try 200
ensemblesize <- 200

ensembleoftrees <- vector(mode='list',length=ensemblesize)

for (i in 1:ensemblesize){
    bootstrapsampleindicators <- sample.int(n=nrow(BasetableTRAIN),
                                           size=nrow(BasetableTRAIN),
                                           replace=TRUE)
    ensembleoftrees[[i]] <- rpart(yTRAIN[bootstrapsampleindicators] ~ .,
                                  BasetableTRAIN[bootstrapsampleindicators,])
}


baggedpredictions <- data.frame(matrix(NA,ncol=ensemblesize,
                                      nrow=nrow(BasetableTEST)))

for (i in 1:ensemblesize){
  
  baggedpredictions[,i] <- as.numeric(predict(ensembleoftrees[[i]],
                                              BasetableTEST)[,2])
}

#SOLUTION:

for (i in 1:ncol(baggedpredictions)){
  finalprediction <- rowMeans(baggedpredictions[,1:i,drop=FALSE])
  aucstore[i] <- AUC::auc(roc(finalprediction,yTEST))
}

plot(aucstore, type="l")
#This shows us that after approximately 
#25 trees there's not much improvement.
#So 25 trees is approximately the optimal
#number of trees for this given model.


