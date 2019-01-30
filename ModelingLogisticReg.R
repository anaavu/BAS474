# Read in all data:
source("http://www.ballings.co/hidden/aCRM/code/chapter2/read_data_sets.R")

#Remove the IDs
BasetableTEST$CustomerID <- BasetableTRAINbig$CustomerID <- NULL
BasetableVAL$CustomerID <- BasetableTRAIN$CustomerID <- NULL

#load the AUC package
if (!require("AUC")) {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}


# Please note that logistic regression is one form of a
# generalized generalized linear model and therefore 
# the functions that we will use have the acronym glm 
# in their names.


#Option 1: Logistic regression with stepwise variable selection.
#We will not look at forward and backward selection as stepwise 
#selection is most robust to overfitting.

# As you will see, the glm function will issue a warning.
# We should always investigate what is going on.
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# There were 50 or more warnings (use warnings() to see the first 50)
# There are many possible causes of this warning, such as a perfect predictor
# variable, correlated predictors or the assumption of a linear relationship
# between the independents and the log odds of the dependent. 
# In our case it seems to be the assumption of independence of predictors. 
# However, the glm function only issues a warning and not an error. 
# That is not a big deal for us, since we are only interested in the 
# prediction and not in parameter values. We want to determine the
# performance of logistic regression on our data, regardless of 
# the assumptions of the algorithm.

(LR <- glm(yTRAINbig ~ ., 
           data=BasetableTRAINbig, 
           family=binomial("logit")))

#Note that '~' means 'regress on' and '~ .' means 'regress on all variables'.
 
#stepwise variable selection
(LRstep <- step(LR, direction="both", trace = FALSE))

#Use the model to make a prediction on test data.
predLRstep <- predict(LRstep, newdata=BasetableTEST, type="response")

#Next we assess the performance of the model
AUC::auc(roc(predLRstep,yTEST))


#Option 2: Regularized Logistic Regression.

#Regularization refers to the introduction of a penalty for complexity 
#in order to avoid overfitting This boils down to keeping all variables 
#in the equation but shrinking their coefficients towards 0. Regularization 
#is often called shrinkage or lasso (least absolute shrinkage and selection
#operator). More concretely, we set a bound on the sum of the absolute values 
#of the coefficients.

#We use the glmnet package:

if (!require("glmnet")) {
  install.packages('glmnet',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('glmnet')
}

# When we load the glmnet package we see this:
# The following object is masked from ‘package:AUC’:
# 
#     auc

#This means that there is naming conflict between the AUC package and the 
#glmnet package. Therefore we need to append the name of the package with ::
#AUC::auc instead of auc.

(LR <-  glmnet(x=data.matrix(BasetableTRAIN), 
               y=yTRAIN, 
               family="binomial"))

#Df is the number of variables that are active (i.e., coefficient > 0)
#%Dev is an evaluation #metric we are not really interested in.
#Lambda is the degree to which the sum of the absolute values of the 
#coefficients is penalized. Hhigher lambda means that coefficients will 
#be smaller.

#For every lambda there is a set of coefficients. The number of active
#variables can also be found by looking at LR

#Let's look at the first and last 2 values of lambda
#For the highest value of lambda we only have an intercept, 
#for the second highest value of lambda TotalPrice becomes
#non-zero, ...
coef(LR)[,1:2]
coef(LR)[,72:73]

#?plot.glmnet
plot(LR) 
#L1 norm= sum of the absolute values of the coefficients

plot(LR,xvar="lambda") 
#Bigger lambda results in more coefficients shrunk to zero. 
#min(LR$lambda) max(LR$lambda)


#Cross- validate lambda
#Note that we are not re-estimating the model. We are merely
#redeploying (predict) the model. This makes cross-validation
#very efficient.

aucstore <- numeric()

for (i in 1:length(LR$lambda) ) {
          predglmnet <- predict(LR,
                                newx=data.matrix(BasetableVAL),
                                type="response",
                                s=LR$lambda[i])
          aucstore[i] <- AUC::auc(roc(as.numeric(predglmnet),yVAL))
}

#Let's determine the optimal lambda value
plot(1:length(LR$lambda),aucstore,type="l")       
(LR.lambda <- LR$lambda[which.max(aucstore)])
           
              
#Now that we know the optimal lambda we can fit the model
#on BasetableTRAINbig.
LR <-  glmnet(x=data.matrix(BasetableTRAINbig), 
              y=yTRAINbig, 
              family="binomial")

#We then use that model with the optimal lambda.
predLRlas <- as.numeric(predict(LR,
                                newx=data.matrix(BasetableTEST),
                                type="response",
                                s=LR.lambda))


#Finally we assess the performance of the model
AUC::auc(roc(predLRlas,yTEST))