#First read in data about product reviews by customers.
URL_prod_rev <- 
  "http://www.ballings.co/hidden/aCRM/data/chapter2/productreviews.csv"
reviews <- read.csv(URL_prod_rev, 
                    header=FALSE, 
                    sep="\n",
                    stringsAsFactors=FALSE)
#When using sep="\n" it is assumed that one wants
#to read in entire lines verbatim.

#Take a subset of these reviews for demonstration purposes
reviews <- reviews[1:5,,drop=FALSE]

#How many reviews are there
nrow(reviews)

#Let's look at the first few reviews.
#Only display the first 50 characters.
sapply(head(reviews),function(x) substr(x,1,50))

#How many characters does each product review contain?
hist(nchar(reviews[,1]),
     main="Number of characters per product review")
#Most of the reviews contains 500 characters or less

#How many words does each product review contain?
hist(sapply(strsplit(reviews[,1]," "),length), 
    main="Number of words per product review")
#Most of the reviews contains 100 words or less

#Since we have only one column we
#store it as a character vector:
reviews <- reviews[,1]

#Now that we have a good understanding of the data
#we can move on to data preparation

# Install the tm (i.e., text mining) package. 
# The tm package expect the SnowballC package to be installed.

if (!require("SnowballC")) {
  install.packages('SnowballC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('SnowballC')
}

if (!require("tm")) {
  install.packages('tm',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('tm')
}

# Good documentation about tm can be found here:
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# STEP 1: Raw text cleaning: case conversion
#(transform everything to lower case), remove
#punctuation and stopwords, and do a spelling check

# Load data
(reviews <- Corpus(VectorSource(reviews)))
# It tells us that we have 86 reviews.
# VCorpus stands for Volatile Corpus (meaning it 
# is stored in RAM (instead of stored on disk).
# Note that corpus refers to a collection of 
# documents. It is the main structure for 
# managing documents in the tm package.
# VectorSource means we are reading in a vector
# It also tells us that there is no metadata on
# the corpus level or document level.

# Let's look at the first document:
reviews[[1]]
# We see that there are 7 metadata fields
# These can be accessed using str(reviews[[1]])
# We also see the number of characters

#In what follows we try to reduce the number 
#of unique words. We measure the mean number of
#words for a document in the corpus as follows:
mean_word_count <- function(data){
  if (any(class(data) %in% c("VCorpus", "Corpus"))) {
      data <- unlist(sapply(data,'[',"content"))
  }
  uniquewords <- sapply(
              strsplit(as.character(data)," "),
              unique)
  mean(sapply(uniquewords,length))
}
mean_word_count(reviews)

#Now we can apply the functions in the tm package:

#Case conversion transform all values to lower case
#Use mc.cores=1 to avoid any system specific problems
#The tm_map function is similar to the apply()
#function in that it applies a function to each
#element (document in this case).
#The function content_transformer allows to adapt 
#the tolower() base function to work with the
# documents.
reviews <- tm_map(reviews, 
                  content_transformer(tolower), 
                  mc.cores=1)
mean_word_count(reviews)
#The mean number of words per document decreased

#remove punctuation
reviews <- tm_map(reviews, 
                  removePunctuation, 
                  mc.cores=1)
mean_word_count(reviews)
#The mean number of words per document decreased

#remove numbers
reviews <- tm_map(reviews, 
                  removeNumbers,  
                  mc.cores=1)
mean_word_count(reviews)
#The mean number of words per document decreased

#remove stopwords
forremoval <- stopwords('english')
head(forremoval)
reviews <-  tm_map(reviews, 
                   removeWords, 
                   c(forremoval), 
                   mc.cores=1)
mean_word_count(reviews)
#The mean number of words per document decreased

#Collapse multiple white space in a 
#single whitespace
reviews <-  tm_map(reviews, 
                   stripWhitespace, 
                   mc.cores=1)

#If we would want to look at the result,
#(e.g., the first document) 
# we could use as.character()
# as.character((reviews[[1]]))

#Spell checking
#Based on Rasmus Baath, research blog, 
#http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/

#Download a list of words sorted by frequency of 
#appearance in natural language

wordlist <- 
  readLines("http://www.ballings.co/hidden/aCRM/data/chapter2/wordlist.txt")

#Function to correct misspelled words
correct <- function(word) { 
  # How dissimilar is this word from all words in the wordlist?
  edit_dist <- adist(word, wordlist)
  # Is there a word that reasonably similar? 
  # If yes, which ones?
  # If no, append the original word to
  # Select the first result (because wordlist is sorted 
  # from most common to least common)
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1] 
}

#try out the function:
correct("speling")
correct("goodd")
correct("corect")
correct("corrrect")
correct("corrct")

#Now convert the data back to a 
#normal character vector. If we would look
#at a document using str we would see
#that it is a list with the actual text 
#stored in the element "content".

reviews <- unlist(sapply(reviews,'[',"content"))

#Pre-allocate a vector where we will store the
#spell-checked reviews
reviews_spell_checked <- character(length(reviews))

#This loop takes a while:
start <- Sys.time()
for (i in 1:length(reviews)){
#Instead of applying correct() to each 
#word, we fist make them unique
#The strsplit() function splits the string in words
count <- table(strsplit(reviews[i],' ')[[1]])
words <- names(count)

#Then we apply our correct function to each 
# unique word
words <- as.character(sapply(words,correct))

#Next we reconstruct the original vector, but
#now spell-checked. We do this because we are
#going to exploit the tm package, which will
#count the words for us.
words <- rep(words,count)

#Concatenate back to a string
reviews_spell_checked[i] <- 
  paste(words, collapse=" ")

#Print progress to the screen
if((i %% max(floor(length(reviews)/10),1))==0) 
    cat(round((i/length(reviews))*100),"%\n")
}
Sys.time()-start
mean_word_count(reviews_spell_checked)
# The word count is further reduced, but is 
# it worth the long processing time?
no

# STEP 2: stemming

(reviews_spell_checked <- Corpus(VectorSource(reviews_spell_checked)))

reviews_spell_checked <- tm_map(reviews_spell_checked, 
                                stemDocument, 
                                mc.cores=1)
mean_word_count(reviews_spell_checked)
#The number of unique words is further reduced by stemming

# STEP 3, 4, 5: Create document-by-term matrix, Term filtering and
# Document vector weighting

#Only include words with a minimum length of 2 characters
(dtm_reviews <- DocumentTermMatrix(
          reviews_spell_checked, 
          control = list(wordLengths = c(2, Inf),
                         weighting=function(x) weightTfIdf(x))
                                  )
)
#We see that the dtm is created. We also get the sparsity.
#Normalize means that 

#Remove terms with too much sparsity. How much sparsity do we allow?
#Let's try different values for the sparse parameter

#Allow that 30% of the documents do not use a word
#At least 70% of the documents need to use the word for the 
#word to stay in the dtm.
(dtm_reviews_dense <- removeSparseTerms(dtm_reviews, sparse= 0.3))
inspect(dtm_reviews_dense)

#Allow that 60% of the documents do not use a word
(dtm_reviews_dense <- removeSparseTerms(dtm_reviews, sparse= 0.6))

#Allow that 30% of the documents do not use a word
(dtm_reviews_dense <- removeSparseTerms(dtm_reviews, sparse= 0.9))

#There really is no way to know in advance how dense the dtm should be
#Therefore it is a reasonable approach not to remove too many sparse terms,
#and rely on the dimension reduction step that comes next.



# Dimension reduction


#Let's start from dtm_reviews_dense

reviews_mat <- as.matrix(dtm_reviews_dense)
dim(reviews_mat)

#First center the data (it might help us when
# determining how many singular vectors we need to 
# retain).
reviews_mat <- t(t(reviews_mat)-colMeans(reviews_mat))

#Perform svd
s <- svd(reviews_mat)

str(s)

# u is the document-by-concept matrix
# This is the matrix we are interested in
dim(s$u)
#ncol(s$u) will be equal to ncol(reviews_mat)
#if nrow(reviews_mat) >= ncol(reviews_mat),
#otherwise ncol(s$u) will be equal to nrow(reviews_mat)

#d represents the strength of each concept
length(s$d)

#v is the term-to-concept matrix, it shows how the 
# terms are related to the concepts
dim(s$v)

#We will use u in our basetable.
head(s$u)

#However, it is important to note that we will want 
#to deploy our our model on future data. We can do
#that as follows:
head(reviews_mat %*% s$v  %*% solve(diag(s$d)))
#The only thing we need to do is replace reviews_mat
#with the new dtm.
#Note: solve returns the inverse of diag(s$d), 
#it is equivalent to 1/sv$d.

# d is proportional to the variance that is explained 
# To compute the variance we proceed as follows:
(s$d^2)/(nrow(reviews_mat) - 1)

# Finally we can plot the explained variance per 
# singular vector in a scree plot.
# % Variance explained of total variance by svd
plot(s$d^2/sum(s$d^2), 
     type="b", 
     ylab="% variance explained",
     xlab="Singular vectors",
     xaxt="n"
)
axis(1,at=1:length(s$d^2/sum(s$d^2)))

#The first singular vector explains 44.8% of the variance
#The first two singular vectors explain 90.8% of the variance. 
#We could drop the last two without loosing much information.

# This shows that we can go from 171 columns 
# (ncol(reviews_mat)) to two without loosing much information.