#==============filtering mobile phone spam with the naive Bayes algorithm===========

#==================Importing data==============#
sms_raw <- read.delim("D:/elementary learners/machine learning/MODULES/Module-3 Classification/Dataset/sms_raw.txt", header=FALSE)
View(sms_raw)
#-----------------Exploring and preparing data----------------------------------------
names(sms_raw)<-c("type","text")
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)

#------------------processing text data for analysis----------------------------------
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
length(sms_corpus)


#---------------------Clean Data---------------------------------------------------
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)

# Note:The DocumentTermMatrix() function will take a corpus and create a data structure called a sparse matrix,
# in which the rows of the matrix indicate documents (that is, SMS messages) and the columns indicate terms (that is, words). Each cell in the matrix stores a number indicating a count of the times the word indicated by the column appears in the document indicated by the row
sms_dtm <- DocumentTermMatrix(corpus_clean)

#----------------------creating training and test datasets----------------------------

# Take 75% percent data for training and for testing we take 25% 
# splitting the raw data frame
sms_raw_train <- sms_raw[1:2388, ]
sms_raw_test  <- sms_raw[2389:3184, ]

# Take 75% percent data for training and for testing we take 25% 
sms_dtm_train <- sms_dtm[1:2388, ]
sms_dtm_test  <- sms_dtm[2389:3184, ]

# And finally, the corpus
sms_corpus_train <- corpus_clean[1:2388]
sms_corpus_test  <- corpus_clean[2389:3184]

#------------------creating indicator features for frequent words-------------------------

Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}

sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))

sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test,
                                list(dictionary = sms_dict))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

#-------------training a model on the data------------------------------------------------
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

#--------------evaluating model performance-----------------------------------------------

sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

