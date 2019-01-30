#STEP 1 Read in all data:
setwd('C:/Users/A628103/Desktop/NYSE_2001')
NYSE <- do.call('rbind',lapply(list.files(full=TRUE),read.csv,header=FALSE))
colnames(NYSE) <- c('Symbol','Date','OpenOfTheDay','HighOfTheDay','LowOfTheDay','CloseOfTheDay','Volume')
sum(is.na(NYSE))
#STEP 2
#Dependent Variable - Today
#DV = Close/Open --> If stock >= 1.01, 1... If stock < 1.01, 2
NYSE$DV <- (NYSE$CloseOfTheDay / NYSE$OpenOfTheDay)
ifelse(NYSE$DV>=1.01,1,0)

#Response Variable - Tomorrow
#DV_Lead Moves DV up 1 row, makes last row NA (opposite of lag)
NYSE$DV_Lead <- unlist(tapply(NYSE[,'DV'],NYSE[,'Symbol'], function(x) c(x[-1],NA)))
NYSE <- NYSE[!is.na(NYSE$DV_Lead),]

#impute missings
install.packages('imputeMissings',
                 repos="https://cran.rstudio.com/", 
                 quiet=TRUE)
require('imputeMissings')
NYSE <- imputeMissings::impute(NYSE)
#------------------------------------------------------------------------------------------
#LOGISTIC REGRESSION
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
#[1] 116057      7

#Convert Numeric to Large Factor
yTEST <- as.factor(yTEST)
yTRAIN <- as.factor(yTRAIN)
yVAL <- as.factor(yVAL)
yTRAINbig <- as.factor(yTRAINbig)

#Makes factor of 2 levels = 0,1 ###Don't use this portion
yTRAIN = c(0,1)
yTRAIN = factor(yTEST)

yVAL = c(0,1)
yVAL = factor(yTEST)

yTEST = c(0,1)
yTEST = factor(yTEST)

yTRAINbig = c(0,1)
yTRAINbig = factor(yTEST)

#load packages
if (!require("AUC")) {
install.packages('AUC',
                 repos="https://cran.rstudio.com/",
                 quiet=TRUE)
require('AUC')
}

if (!require("glmnet")) {
  install.packages('glmnet',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('glmnet')
}
#yTRAINBIG <- NULL
#------------------------------------------------------------------------------------------
#Option 1: Logistic regression with stepwise variable selection
#Logistic Regression
(LR <- glm(yTRAINbig ~.,
           data=BasetableTRAINbig,
           family=binomial("logit")))
LR <- glmnet(x=data.matrix(BasetableTRAINbig),
             y=as.factor(yTRAINbig),
             family="binomial")
str(BasetableTRAINbig)
?glmnet
str(BasetableTRAINbig)
length(yTRAINbig)
#Warning message:
# Error in model.frame.default(formula = yTRAINbig ~ ., data = BasetableTRAINbig,  : 
# variable lengths differ (found for 'Date')

#factor of 2 levels 
#LR <- glmnet(x=data.matrix(BasetableTRAIN),y=as.factor(yTRAIN),family="binomial")

#Stepwise Variable Selection
LRstep <- step(LR, direction="both", trace = FALSE) ###6. ??
#There were 50 or more warnings (use warnings() to see the first 50)

#Categorical vars

#Use the model to make a prediction on test data.
predLRstep <- predict(LRstep, newdata=BasetableTEST, type="response")
LR_pred <- read.csv("~/Desktop/Anagha/College/BAS_474/Project/Round5/pred5.csv", sep=",", colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric"), header=FALSE)
colnames(LR_pred) <- c('Symbol','Date','OpenOfTheDay','HighOfTheDay','LowOfTheDay','CloseOfTheDay','Volume')
predLRstep2 <- predict(LRstep, newdata=LR_pred, type="response")


#Assess the performance of the model
AUC::auc(roc(LRstep,yTRAIN))
#------------------------------------------------------------------------------------------
#SUBMISSION
symbols_to_predict <- c()
PredictionsRound2 <- data.frame(symbols=as.character(c("c","b","a","e","g")),
                                 predictions=c(0.1,0.4,0.4,1,0.05),
                                 stringsAsFactors=FALSE) 
PredictionsRound2 <- predLRstep[predLRstep[,'Symbol'] %in% symbols_to_predict,]
PredictionsRound2 <- predLRstep[predLRstep$Symbols %in% symbols_to_predict,]
matched <- match(PredictionsRound2$Symbols,symbols_to_predict)
matched <- matched[!is.na(matched)]
PredictionsRound2[matched,]
