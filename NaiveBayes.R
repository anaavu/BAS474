(df <- 
  data.frame(
churn=c(0,0,1,1,1,0,1,0,1,1,1,1,1,0),
tenure=c("long","long","short","medium",
  "medium","medium","short","long",
  "long","medium","long","short",
  "short","medium"),
spend=c("high","high","high","medium"
  ,"low","low","low","medium",
  "low","medium","medium","medium",
  "high","medium")))

# churn: compute the percentage
table(df[,1])
table(df[,1])/sum(table(df[,1]))

#tenure: compute the percentage by column total
table(df[,2],df[,1])
t(t(table(df[,2],df[,1]))/colSums(table(df[,2],df[,1])))

#spend: compute the percentage by column total
table(df[,3],df[,1])
t(t(table(df[,3],df[,1]))/colSums(table(df[,3],df[,1])))

# The above contingency tables are the
# inputs for our model.

# Consider a new instance with the following characteristics:
#   tenure=long
#   spend=high


# What is the probability of churn?

# P (Y=churn | X) = 

# Nominator:
#       P( Xtenure=long | Y = 1 )
#     x P( Xspend=high | Y = 1 )
#     x P( Y = 1) =
(Nominator <- 0.2222222 * 0.2222222 * 0.6428571)
# Denominator:
#     P ( X )=
#     P( Xtenure=long | Y = 0 )
#     x P( Xspend=high | Y = 0 )
#     x P( Y = 0) 
#   +   P( Xtenure=long | Y = 1 )
#     x P( Xspend=high | Y = 1 )
#     x P( Y = 1) 
(Denominator <- 
(0.6000000 * 0.4000000 * 0.3571429) +
(0.2222222 * 0.2222222 * 0.6428571))

Nominator/Denominator
# Let's verify this with the naiveBayes
# function in the e1071 package

if (!require("e1071")) {
  install.packages('e1071',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('e1071')
}

head(df)

NB <- naiveBayes(x=df[,2:3], y=df[,1])
(predNB <- predict(NB,df[2:3], type = "raw", threshold = 0.001)[,2])

?naiveBayes
# As we can see, the first row in df is
# equivalent to the example we computed manually

#Let's now compute the performance of
#the naive bayes classifier
if (!require("AUC")) {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}

auc(roc(predNB,as.factor(df[,1])))

predNB
