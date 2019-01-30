source("http://ballings.co/hidden/aCRM/code/chapter2/read_data_sets.R")

#load the package AUC to evaluate model performance
if(require('AUC')==FALSE)  {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}

#load the package randomForest 
if (!require("randomForest")) {
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('randomForest')
}

#always first look at the documentation
?randomForest

#create a first random forest model
rFmodel <- randomForest(x=BasetableTRAIN,
                        y=yTRAIN,  
                        ntree=1000, 
                        importance=TRUE)

#look at the importance of the variables
importance(rFmodel, type=1)
varImpPlot(rFmodel,type=1)
varImpPlot(rFmodel,type=2)

#For more imformation about the measures: 
?importance

#prediction
#firt look at the documentation
?predict.randomForest
predrF <- predict(rFmodel,BasetableTEST,type="prob")[,2]
#assess final performance
AUC::auc(roc(predrF,yTEST))

#What is the optimal value for ntree

#plotting learing curve

rFmodel <- randomForest(x=BasetableTRAIN,
                        y=yTRAIN, 
                        xtest=BasetableVAL, 
                        ytest=yVAL, 
                        ntree=1000, 
                        importance=TRUE)
plot(rFmodel)
#red dashed line: class error 0
#green dotted line: class error 1
#black solid line: OOB error
#we see that class 0 has lower error than class 1. 
#This is because there are much more zeroes to learn from.

#create the model with the optimal ensemble size
rFmodel <- randomForest(x=BasetableTRAINbig,
                          y=yTRAINbig,  
                          ntree=which.min(rFmodel$test$err.rate[,1]), 
                          importance=TRUE)
predrF <- predict(rFmodel,BasetableTEST,type="prob")[,2]
#Final performance
AUC::auc(roc(predrF,yTEST))