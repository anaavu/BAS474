#######################################################
#Contents of this file:

#DUMMIES
#MANUALLY
#AUTOMATICALLY
#BY PROCESSING/ AGGREGATING / SUMMARIZING
#MERGING
#Merging 2 data frames
#Merging multiple data frames
#APPLYING FUNCTIONS TO DATA FRAMES
#BAD SOLUTION
#BETTER SOLUTION
#BEST SOLUTION
#WORKING WITH DATES
#CREATING FUNCTIONS
#CONDITIONAL PROCESSING
#LOOPS
#TIMING CODE


#######################################################

#Create dummies manually
(x <- data.frame(ID=c(1,1,2,2,3,3),
                 V1=c(1,2,3,4,4,4)))

x[,3] <- ifelse(x$V1==1,1,0)
x[,4] <- ifelse(x$V1==2,1,0)
x[,5] <- ifelse(x$V1==3,1,0)
x[,6] <- ifelse(x$V1==4,1,0)
x
(names(x)[3:6] <- c('V1_1','V1_2','V1_3','V1_4'))


#Create dummy variables automatically
if (!require(dummy)){
  install.packages('dummy', 
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require(dummy) #same as library(dummy)
}

(x <- data.frame(ID=c(1,1,1,1,2,2,2),
                 V1=as.character(c(1,2,3,4,4,4,4)),
                 V2=as.factor(c(1,2,3,4,4,4,4))))

dummy(x)


#######################################################
#Aggregating (by-processing)

#Create a data frame
(x <- data.frame(ID=c(1,1,1,1,2,2,2),
                 V1=c(1,2,3,4,4,4,4),
                 V2=c(1,2,3,4,4,4,4)))
?aggregate

#sum
aggregate(x[,names(x) != 'ID'],by=list(ID=x$ID),sum)
#counts
aggregate(x[,names(x) != 'ID'],by=list(x$ID),length) 
#Notice the difference in the by argument 
#between the first and last aggregate

#first or last observation
aggregate(x[,names(x) != 'ID'],by=list(x$ID),head,n=1)
aggregate(x[,names(x) != 'ID'],by=list(x$ID),tail,n=1)


#######################################################

# Merging

#Merging 2 data frames
# ?merge

#Create two data frames
(x <- data.frame(ID=c(1,1,2,2,3,3),
                 V1=c(1,2,3,4,4,4),
                 V2=c(1,2,3,4,4,4)))

(y <- data.frame(ID=c(2,2,3,3,4,4),
                 V3=c(1,2,3,4,4,4),
                 V4=c(1,2,3,4,4,4)))


(x_by <- aggregate(x[,names(x) != 'ID'],
                   by=list(ID=x$ID),sum))

(y_by <- aggregate(y[,names(y) != 'ID'],
                   by=list(y$ID),sum))

names(y_by)[1] <- 'ID'

str(x_by)
str(y_by)


#Inner Join
merge(x_by,y_by,by='ID')

#Left outer join
merge(x_by,y_by,by='ID',all.x=TRUE)

#Right outer join
merge(x_by,y_by,by='ID',all.y=TRUE)

#Full outer join
merge(x_by,y_by,by='ID',all=TRUE)

#Merging multiple data frames

#create a third dataset
(z <- data.frame(ID=c(2,2,3,3,4,4),
                 V5=c(1,2,3,4,4,4),
                 V6=c(1,2,3,4,4,4)))
(z_by <- aggregate(z[,names(z) != 'ID'],
                   by=list(ID=z$ID),sum))


#Approach 1: Use a loop (the bad approach, correct but slower)
datalist <- list(x_by,y_by,z_by)
for (i in 1:length(datalist)) {
  if (i==1) intermediate <- datalist[[1]] 
  if (i>1) intermediate <- merge(intermediate,
                                 datalist[[i]],by='ID')
}
intermediate
?list

#Approach 2: Reduce (the good approach)
#?Reduce

#The Reduce function successively combines the elements 
#of a given vector. To see the process in action, 
#set accumulate=TRUE
str(datalist)
Reduce(function(x, y) merge(x, y, by='ID'),datalist,accumulate=TRUE)

#To keep only the final merge drop the 
#accumulate (default is accumulate=FALSE)
Reduce(function(x, y) merge(x, y, by='ID'),datalist)
?Reduce
sapply
#######################################################

#Applying functions to data frames
#create some data
x <- data.frame(matrix(runif(10000000),ncol=100000)) 
#this yields 100 rows and 100k columns
dim(x)

#Monitor speed of execution (look at elapsed time)
#system.time('here goes your code')

#Objective: compute the column sums

#Bad solution: Use a Loop to take the sum of 
#each variable and save it (22 seconds on our system)
x_sum1 <- integer()
system.time(
  for (i in 1:ncol(x)) x_sum1[i] <- sum(x[,i])
)
head(x_sum1)


#Better solution: Pre-allocate the object and 
#use a Loop to take the sum of each variable and 
#save it (6 seconds on our system)
x_sum2 <- integer(length=100000)
system.time(
  for (i in 1:ncol(x)) x_sum2[i] <- sum(x[,i])
)
head(x_sum2)


#Best solution: Use sapply (0.1 seconds on our system)
#Conceptually sapply successively takes the columns of 
#the data frame, applies a function to each column, 
#returns a vector of the same length as the number of 
#columns.
system.time(
  x_sum3 <- sapply(x,sum)
)
head(x_sum3)

#There are some ready made function that rely on sapply
head(colSums(x))
head(colMeans(x))

#######################################################
# Working with dates

#When we read in data, dates are read in as a 
#character vector. The function as.Date() creates 
#a Date object. Most systems store dates internally 
#as the number of days (numeric vector) since some 
#origin. When using as.Date on a numeric vector we 
#need to set the origin. The origin on this system 
#is "1970/01/01"
help(as.Date)

#This is how you find out what the origin is:
Sys.Date()-as.integer(Sys.Date())

#This is Today's date:
Sys.Date()

#Converting an integer requires an origin:
as.Date(10,origin="1970/01/01")

# formats
# %d  day as a number 
# %a  abbreviated weekday 
# %A  unabbreviated weekday	
# %m	month
# %b  abbreviated month
# %B	unabbreviated month
# %y  2-digit year
# %Y	4-digit year 

#More information:
# help(strftime)
# help(ISOdatetime)

#Specifying a date format:
f <- "%d/%m/%Y"

#Subtraction of two dates as Date object 
#using the format:
as.Date('02/01/1970',format=f) - 
  as.Date('01/01/1970',format=f)

#Subtraction of two dates as numeric objects:
as.numeric(as.Date('02/01/1970',format=f)) - 
  as.numeric(as.Date('01/01/1970',format=f))

#Adding zero days to the origin should be 0: 
as.numeric(as.Date(0,origin="1970/01/01"))


#Read in the data with dates as characters
URL_subs <- "http://ballings.co/hidden/aCRM/data/chapter2/subscriptions.txt"

subscriptions <- 
  read.table(URL_subs,
             header=TRUE, sep=";", 
             colClasses=c("character",
                          "character",
                          "character",
                          "character",
                          "character",
                          "character",
                          "integer",
                          "integer",
                          "character",
                          "factor",
                          "factor",
                          "character",
                          "character",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric"))

#Transform the dates in the subscriptions data frame
#First look at the data
str(subscriptions,vec.len=0.5)
head(subscriptions$StartDate)

#Get the positions of the date variables
(vars <- which(names(subscriptions) %in% 
                 c("StartDate","EndDate","PaymentDate","RenewalDate")))

#Transform to dates
subscriptions[,vars] <- sapply(vars,function(vars) {
  as.Date(subscriptions[,vars],format="%d/%m/%Y")},
  simplify=FALSE)
#Note: in this case 'simplify' means simplify the result 
#to a numeric vector, which is just as good here
str(subscriptions,vec.len=0.5)
head(subscriptions$StartDate)

#Reading in dates directly (as opposed to changing them afterwards)
#what is the current default date format?
Sys.Date()
#looks like it is YYYY-MM-DD

#Currently all the dates are character vectors
#The date format in our data is DD/MM/YYYY (e.g.,"27/01/2010")


#If we use Date, values are not imported adequately.
#Let's try it. Look at StartDate
subscriptions <- read.table(URL_subs,
                            header=TRUE, sep=";", 
                            colClasses=c("integer",
                                         "integer",
                                         "character",
                                         "character",
                                         "Date",
                                         "character",
                                         "integer",
                                         "integer",
                                         "character",
                                         "factor",
                                         "factor",
                                         "character",
                                         "integer",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric"))
str(subscriptions,vec.len=0.5)
head(subscriptions$StartDate)
#StartDate clearly read incorrectly

#Take care of default date format
f <- "%d/%m/%Y"
setClass('fDate')
setAs(from="character",
      to="fDate", 
      def=function(from) as.Date(from, format=f) )

#Let's use fDate instead of Date for StartDate
#Look at StartDate
subscriptions <- read.table(URL_subs,
                            header=TRUE, sep=";", 
                            colClasses=c("integer",
                                         "integer",
                                         "character",
                                         "character",
                                         "fDate",
                                         "character",
                                         "integer",
                                         "integer",
                                         "character",
                                         "factor",
                                         "factor",
                                         "character",
                                         "integer",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric",
                                         "numeric"))
str(subscriptions,vec.len=0.5)
head(subscriptions$StartDate)
#StartDate is correct now

#######################################################
#Functions
#Make a new function:

# functionname <- fuction(parametername){
#   code 
# }

#Example:
MySum <- function(a,b){
  c <- a+b
  c #since this is the last line, c will be returned
}

MySum(1,2)

#This is identical:
MySum <- function(a,b){
  c <- a+b
  return(c)
}

MySum(1,2)

#Everything we do in a function stays in 
#the function, unless we make it global. 

outerfunc <- function(a,b){
  
  innerfunc <- function(a,b){
    a+b
  }
  v <- 1
}

outerfunc(1,2) #Nothing is returned

#What comes next will not work. 
#R will complain that it 
#cannot find the function and object
innerfunc(1,2)
v


#Make the innerfunc and v global
outerfunc <- function(a,b){
  
  innerfunc <<- function(a,b){
    a+b
  }
  v <<- 1
}

outerfunc(1,2)
innerfunc(1,2)
v

#However, this is generally considered bad practice.
#It's better to have a function return another function
outerfunc <- function(){
  
  innerfunc <- function(a,b){
    a+b
  }
  v <- 1
  return(list(innerfunc,v))
}

f <- outerfunc()
f
f[[1]](1,2)
f[[2]]

#######################################################
#Conditional processing
#Consider the following self explanatory if then construct:
x <- 4

if (x <= 2){
  print("Smaller than or equal to 2")
} else if (x == 3) {
  print("Equal to 3")
} else {
  print("Greater than 3")
}

#######################################################
#Loops

#Consider three different loop constructs that print 10 numbers to the screen.

#For loop
for (i in 1:10){
  print(i)
}

#While loop
i <- 0
while (i < 10){
  i <- i + 1
  print(i) 
}

#Repeat loop
i <- 0
repeat{
  i <- i + 1
  print(i)
  if (i==10) break
}

#######################################################
#Timing code

#How long does one second take?
start <- Sys.time()
Sys.sleep("1")
round(Sys.time() - start)

#or
system.time(Sys.sleep("1"))
