# libraries
library(tm)
library(wordcloud)
library(e1071)

# set working directory
setwd("E:\\My Documents\\RStudio\\lending_club")

# read in the data
loan.data <- read.csv("data\\LoanStats_2012_2013.csv", skip=1, nrows=188181)

# select only rows that include descriptions
has.desc <- which(loan.data$desc != "")
loan.data <- loan.data[has.desc, ]

# define column for having gone bad
bad.statuses <- c("Charged Off", "Default", "Late (16-30 days)",
                  "Late (31-120 days)")
is.bad.loan <- loan.data$loan_status %in% bad.statuses
is.bad.loan <- factor(is.bad.loan, levels=c(TRUE, FALSE),
                      labels=c("Bad", "Good"))

# keep only the descriptions and indicator for badness
loan.data <- data.frame(description=loan.data$desc, is.bad.loan=is.bad.loan)

# clean up the descriptions a bit
# all descriptions include the leading line:
#    "Borrower added on MM/DD/YY > "
# let's remove that portion
loan.data$description <- gsub("Borrower added on ../../.. >", "",
                              loan.data$description)
# also remove the <br> tags
loan.data$description <- gsub("<br>", "", loan.data$description)


# ready to start processing the descriptions using tm library

# build corpus of descriptions
desc.corpus <- Corpus(VectorSource(loan.data$description))

# lowercase all the descriptions
cleaned.corpus <- tm_map(desc.corpus, tolower)

# remove stop words, e.g. to, and, but, or
cleaned.corpus <- tm_map(cleaned.corpus, removeWords, stopwords())

# remove punctuation
cleaned.corpus <- tm_map(cleaned.corpus, removePunctuation)

# remove extra whitespace
cleaned.corpus <- tm_map(cleaned.corpus, stripWhitespace)


# for fun, build some word clouds from the cleaned corpus
# entire corpus
wordcloud(cleaned.corpus, min.freq=5000, random.order=FALSE)
# only good loans
wordcloud(subset(cleaned.corpus, loan.data$is.bad.loan == "Good"),
          min.freq=5000, random.order=FALSE)
# only bad loans
wordcloud(subset(cleaned.corpus, loan.data$is.bad.loan == "Bad"),
          min.freq=1000, random.order=FALSE)


# use 10-fold cross-validation to evaluate the model
set.seed(1337)
idxs <- sample(1:length(cleaned.corpus))
n.in.fold <- ceiling(length(cleaned.corpus) / 10)
crosstab <- matrix(numeric(4), ncol=2)
for (k in 1:10) {
    # select the elements in the fold
    first <- (k - 1) * n.in.fold + 1
    last <- min(k * n.in.fold, length(cleaned.corpus))
    in.fold <- first:last
    
    # separate into train and test sets
    # unprocessed data frame
    is.bad.train <- loan.data$is.bad.loan[-in.fold]
    is.bad.test <- loan.data$is.bad.loan[in.fold]
    # corpus
    corpus.train <- cleaned.corpus[-in.fold]
    corpus.test <- cleaned.corpus[in.fold]
    
    # build document-term matrix using only frequently-used words
    desc.dtm.train <- DocumentTermMatrix(corpus.train)
    desc.dict <- findFreqTerms(desc.dtm.train,
                               ceiling(length(corpus.train) * .001))
    desc.dtm.train <- DocumentTermMatrix(corpus.train,
                                         list(dictionary=desc.dict))
    desc.dtm.test <- DocumentTermMatrix(corpus.test,
                                        list(dictionary=desc.dict))
    
    # convert the counts in the document-term matrix to booleans
    ConvertCounts <- function (x) {
        x <- ifelse(x > 0, 1, 0)
        x <- factor(x, levels=c(0, 1), labels=c("No", "Yes"))
        return(x)
    }
    desc.dtm.train <- apply(desc.dtm.train, 2, ConvertCounts)
    desc.dtm.test <- apply(desc.dtm.test, 2, ConvertCounts)
    
    # train the model!
    loans.nbclassifier <- naiveBayes(desc.dtm.train, is.bad.train)
    
    # evaluate it on the test set
    loans.test.pred <- predict(loans.nbclassifier, desc.dtm.test)
    
    # record results from the fold
    classification <- c("Bad", "Good")
    for (i in 1:2) {
        for (j in 1:2) {
            crosstab[i, j] <- (crosstab[i, j]
                               + sum((is.bad.test == classification[i]
                                      & loans.test.pred == classification[j])))
        }
    }
}

# print out the confusion matrix
crosstab <- cbind(crosstab, rowSums(crosstab))
crosstab <- rbind(crosstab, colSums(crosstab))
rownames(crosstab) <- c("actual.bad", "actual.good", "sum")
colnames(crosstab) <- c("predicted.bad", "predicted.good", "sum")
print(crosstab)
print(round(crosstab / crosstab[3, 3], digits=4))

# results from running this script with the seed 1337
# cross table
#                predicted
#              bad    good
#actual   bad   62   12247
#        good  412   68757

