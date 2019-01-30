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

partition <- function(df) 
{
  for (i in 1:5)
  {
    zero <- Basetable[Basetable$Churn == 0, ]
    one <- Basetable[Basetable$Churn == 1, ]
    trainInd <- c(sample(x=1:nrow(zero),size=(nrow(zero)/2)),sample(x=1:nrow(one),size=(nrow(one)/2)))
    testInd <- Basetable[-trainInd, ]
    #Use training and test
    testInd2 <- trainInd
    trainInd2 <- testInd
    #Use training and test again
  }
  
}

table(Basetable$Churn)
table(train)
table(testInd$Churn)
