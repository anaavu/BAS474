# Stratified 5x2fcv for binary classification

# y: the response factor. Must be a factor
# p: the percentage of instances that have to go in the training set
# times: how many times?
# In 5x2fcv p=0.5 and times=5
partition <- function(y,p=0.5,times=5) {
  
  #STEP 1: split up 0 and 1
  class1_ind <- which(y==as.integer(levels(y)[1]))
  class2_ind <- which(y==as.integer(levels(y)[2]))
  
  l <- list()
  for (i in 1:times){
  
  #STEP 2: take subsamples for both 0 and 1
  class1_ind_train <- sample(class1_ind, floor(p*table(y)[1]),replace=FALSE)
  class2_ind_train <- sample(class2_ind, floor(p*table(y)[2]),replace=FALSE)

  class1_ind_test <- class1_ind[!class1_ind %in% class1_ind_train]
  class2_ind_test <- class2_ind[!class2_ind %in% class2_ind_train]

  #STEP 3: combine 0 and 1 for both train and test
  
  l[[i]] <- list(fold1=c(class1_ind_train,class2_ind_train),fold2=c(class1_ind_test,class2_ind_test))
  names(l)[i] <- paste("Time",i,sep="")
  }
  l
}


y <- as.factor(round(runif(100),0))

partition(y,p=0.5,times=5)
str(partition(y,p=0.5,times=5))
