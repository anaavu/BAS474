#Download the data
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

#always first look at the main manual page 
?rpart
#estimate a tree model
(tree <- rpart(yTRAIN ~ ., BasetableTRAIN))

# Here is some of the output explained:
# ?rpart.object
# split= means the predictor split
# n= number of observations reaching the node
# loss= how many observations are for the wrong class 
#       (loss=yprob(other class than yval)*n
# yval= if you run a new example through the tree it will have this value
# yprob= the proportion of examples having respectively 0 and 1

#We can also visualize the tree:
par(xpd = TRUE)
plot(tree, compress = TRUE)
text(tree, use.n = TRUE)
# How to interpret the tree?
# The first split is whether TotalDiscount < 109.9
# If yes go to the left, otherwise go to the right.
# If we go to the right we encounter the same 
# variable but now another split is used:
# TotalDiscount >= 146.6. If yes, go to the left.
# If no, go to the right.
# The numbers can be interpreted as follows:
#  -the top number is the predicted label (0 or 1). 
#  -and in a/b, a is the number of 0s and b is 
#   the number of 1s. For example 11/1 means   
#   that there were 11 zeroes and 1 ones.
# Whether the predicted label is 0 or 1 depends on
# whether there are more 0s or 1s. 
# The probability of 1 is the proportion of 1s.

#Tuning trees

# It is possible that, given a certain combination of variables, 
# there exists no split that reduces the overall lack of fit to a 
# sufficient degree. Any split that does not improve the fit by cp 
# (complexity parameter) will not be pursued. For example, if the 
# fit is not improved by 50%, do not split the tree. This results 
# in no splits:
(tree <- rpart(yTRAIN ~ ., 
               control=rpart.control(cp = 0.5), 
               BasetableTRAIN))

# This results in one and the same prediction for all observations
# If we make a prediction using that model we will get for all 
# observations the proportion of the majority class:

table(yTRAIN)[2]/sum(table(yTRAIN))
table(predTree <- predict(tree,BasetableTEST)[,2])


#So we may decrease the required improvement in fit to get splits
#by decreasing the cp parameter
(tree <- rpart(yTRAIN ~ ., 
              control=rpart.control(cp = 0.001), 
              BasetableTRAIN))
table(predTree <- predict(tree,BasetableTEST)[,2])
# we see more unique values

# The importance of variables is the improvement in node 
# impurity thanks to a variable. In binary classification 
# the Gini index is used as measure of node 
#impurity: p(1-p)=p-p^2, where p is the proportion of ones.
#The Gini function looks like this:
plot(seq(0,1,0.1),seq(0,1,0.1)-seq(0,1,0.1)^2,type="l",
     ylab="Gini index",xlab="Proportion of ones")
#This means that Gini is at maximum when a node is equally 
# divided amongst both classes

# How to get variable importance for all variables?

#look at the names in the object
names(tree)
#there is an element called variable.importance
tree$variable.importance #higher values means higher importance
#normalize the values:
tree$variable.importance/sum(tree$variable.importance)

#more information can be obtained through:
#summary(tree)

#Let's cross validate the cp parameter 
#Tuning the cp

candidates <- seq(0.00001,0.2,by=0.0010)
aucstore <- numeric(length(candidates))
j <- 0
for (i in candidates) {
  j <- j + 1  
  tree <- rpart(yTRAIN ~ ., 
                control=rpart.control(cp = i), 
                BasetableTRAIN)
  predTree <- predict(tree,BasetableVAL)[,2]
  aucstore[j] <- AUC::auc(roc(predTree,yVAL))
  if (j %% 20==0) cat(j/length(candidates)*100,"% finished\n")
}

plot(aucstore,type="l")
#what is the position of the best auc?
which.max(aucstore)

#next we train the model on TRAINbig with the optimal cp 
#and confirm final performance on the test set
(tree <- rpart(yTRAINbig ~ ., 
              control=rpart.control(cp = candidates[which.max(aucstore)]), 
              BasetableTRAINbig))
predTREE <- predict(tree,BasetableTEST)[,2]

#Final model performance:
AUC::auc(roc(predTREE,yTEST))



#BAGGED TREES
#Download the data
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

#Create 10 trees
ensemblesize <- 10

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

finalprediction <- rowMeans(baggedpredictions)

AUC::auc(roc(finalprediction,yTEST))
#drastic improvement

