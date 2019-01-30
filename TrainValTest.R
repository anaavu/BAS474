classes <- c("character",
             "numeric",
             "numeric",
             "numeric",
             "numeric",
             "numeric",
             "numeric",
             "numeric",
             "factor")

Basetable <- read.csv("http://ballings.co/hidden/aCRM/data/chapter2/Basetable.csv",
                      colClasses=classes)
#look at the data
str(Basetable, vec.len=0.5)

#We don't need the customer ID in the modeling phase
#so we can safely remove it
Basetable$CustomerID <- NULL

#Before starting the analyses, clean up by removing all 
# objects except the Basetable
rm(list=setdiff(ls(),c("Basetable","classes"))) 
#setdiff returns which elements of the first argument 
#are not in the second argument
ls()

#Cut up the data in three: train, val and test.

#create idicators
#randomize order of indicators
allind <- sample(x=1:nrow(Basetable),size=nrow(Basetable))
#split in three parts 
trainind <- allind[1:round(length(allind)/3)]
valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))]
testind <- allind[round(length(allind)*(2/3)+1):length(allind)]

BasetableTRAIN <- Basetable[trainind,]
BasetableVAL <- Basetable[valind,]
BasetableTEST <- Basetable[testind,]
#if no tuning is required than we
#use TRAIN + VAL as training set
BasetableTRAINbig <- rbind(BasetableTRAIN,BasetableVAL)

#To make sure that the results in the book match our 
#results as closely as possible it's better to work on these
#preprepared sets:
BasetableTRAIN <- 
  read.csv("http://ballings.co/hidden/aCRM/data/chapter2/BasetableTRAIN.csv",
                      colClasses=classes)
BasetableVAL <- 
  read.csv("http://ballings.co/hidden/aCRM/data/chapter2/BasetableVAL.csv",
                      colClasses=classes)
BasetableTEST <-  
  read.csv("http://ballings.co/hidden/aCRM/data/chapter2/BasetableTEST.csv",
                      colClasses=classes)
BasetableTRAINbig <- 
  read.csv("http://ballings.co/hidden/aCRM/data/chapter2/BasetableTRAINbig.csv",
                      colClasses=classes)

#At this point we can remove the Basetable
rm(Basetable)

BasetableTRAIN$CustomerID <- BasetableVAL$CustomerID <- NULL
BasetableTEST$CustomerID <- BasetableTRAINbig$CustomerID <- NULL

#Isolate the response variable 
#This makes it easier to call some functions later on
yTRAIN <- BasetableTRAIN$Churn
BasetableTRAIN$Churn <- NULL

yVAL <- BasetableVAL$Churn
BasetableVAL$Churn <- NULL

yTEST <- BasetableTEST$Churn
BasetableTEST$Churn <- NULL

yTRAINbig <- BasetableTRAINbig$Churn
BasetableTRAINbig$Churn <- NULL

#Check the distribution of the dependent variable.
#It should be similar in the three sets.
table(yTRAIN);table(yVAL);table(yTEST)
 
#check whether we didn't make a mistake
dim(BasetableTRAIN)
dim(BasetableVAL)
dim(BasetableTEST)