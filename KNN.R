#STEP 1 Read in all data:
setwd('C:/Users/A628103/Desktop/NYSE_2001')
NYSE <- do.call('rbind',lapply(list.files(full=TRUE),read.csv,header=FALSE))
colnames(NYSE) <- c('Symbol','Date','OpenOfTheDay','HighOfTheDay','LowOfTheDay','CloseOfTheDay','Volume')

#STEP 2
#Dependent Variable - Today
#DV = Close/Open --> If stock >= 1.01, 1... If stock < 1.01, 2
NYSE$DV <- (NYSE$CloseOfTheDay / NYSE$OpenOfTheDay)
ifelse(NYSE$DV>=1.01,1,0)

#Response Variable - Tomorrow
#DV_Lead Moves DV up 1 row, makes last row NA (opposite of lag)
NYSE$DV_Lead <- unlist(tapply(NYSE[,'DV'],NYSE[,'Symbol'], function(x) c(x[-1],NA)))
#------------------------------------------------------------------------------------------
#set up data
Basetable <- NYSE
str(Basetable,vec.len=0.5)
Basetable$Symbol <- NULL

#create idicators
#randomize order of indicators
allind <-sample(x=1:nrow(Basetable),size=nrow(Basetable))

#split in three parts
trainind <- allind[1:round(length(allind)/3)]
valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))]
testind <- allind[round(length(allind)*(2/3)+1):length(allind)]

BasetableTRAIN <- Basetable[trainind,]
BasetableVAL <- Basetable[valind,]
BasetableTEST <- Basetable[testind,]
BasetableTRAINbig <-rbind(BasetableTRAIN,BasetableVAL)

rm(Basetable)

#Remove the Symbols
BasetableTRAIN$Symbol <- BasetableVAL$Symbol <- NULL
BasetableTEST$Symbol <- BasetableTRAINbig$Symbol <- NULL

#Isolate the response variable 
#This makes it easier to call some functions later on
yTRAIN <- BasetableTRAIN$DV_Lead
BasetableTRAIN$DV_Lead <- NULL

yVAL <- BasetableVAL$DV_Lead
BasetableVAL$DV_Lead <- NULL

yTEST <- BasetableTEST$DV_Lead
BasetableTEST$DV_Lead <- NULL

yTRAINbig <- BasetableTRAINbig$DV_Lead
BasetableTRAINbig$DV_Lead <- NULL

table(yTRAIN);table(yVAL);table(yTEST)

dim(BasetableTRAIN)
#[1] 116057      7
dim(BasetableVAL)
#[1] 116057      7
dim(BasetableTEST)
#[1] 116057      7Lead
BasetableTRAINbig$DV_Lead <- NULL
#------------------------------------------------------------------------------------------
# packages
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

require('AUC')
#------------------------------------------------------------------------------------------
#KNN
trainKNN <- data.frame(sapply(BasetableTRAIN, function(x) as.numeric(as.character(x))))
trainKNNbig <- data.frame(sapply(BasetableTRAINbig, function(x) as.numeric(as.character(x))))   
valKNN <- data.frame(sapply(BasetableVAL, function(x) as.numeric(as.character(x))))
testKNN <- data.frame(sapply(BasetableTEST, function(x) as.numeric(as.character(x))))

#creating the Standard Deviation and Means 
stdev <- sapply(trainKNN,sd)
means <- sapply(trainKNN,mean)

trainKNNbig <- data.frame(t((t(trainKNNbig)-means)/stdev))
trainKNN <- data.frame(t((t(trainKNN)-means)/stdev))
valKNN <- data.frame(t((t(valKNN)-means)/stdev))
testKNN <- data.frame(t((t(testKNN)-means)/stdev))

#remove NAs
trainKNN <- trainKNN[!is.na(trainKNN)]
trainKNNbig <- trainKNNbig[!is.na(trainKNNbig)]
valKNN <- valKNN[!is.na(valKNN)]
testKNN <- testKNN[!is.na(testKNN)]

#example for 10 nearest neighbors:
k <- 10

#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=trainKNNbig, 
                                       query=testKNN, 
                                       k=k))


#retrieve the actual y from the training set
predKNN <- as.integer(as.character(yTRAINbig[indicatorsKNN]))

#remove NAs
predKNN <- predKNN[!is.na(predKNN)]
testKNN <- testKNN[!is.na(testKNN)]

#if k > 1 then we take the proportion of 1s
predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                      ncol=k,
                                      nrow=length(testKNN))))

auc(roc(predKNN,yTEST))

# [1] 0.8813233
# must be in between 0.5 and 1
#-------------------------------------------------------------------------
# if we want to tune:
# tuning comes down to evaluating which value for k is best
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
# [1] 18

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
# [1] 0.9029777

plot(roc(predKNNoptimal,yTEST))
