Predicting Default on Lending Club Loans
================
Stuart Liu-Mayo

Lending Club is a peer-to-peer loans business that has been in the news recently both for their novel take on loan structuring and for some high-profile failures of their data analytics. Their business model is to serve as a platform that connects individual investors with borrowers. Investors are attracted by the opportunity to earn higher interest rates than most other investment vehicles can offer, and borrowers are attracted who either cannot get loans from banks or get unacceptably high interest rates from banks.

A central feature of Lending Club's business model is their data analytics. Every loan they originate is given a risk label that corresponds to how risky they believe the loan is. This label is the principle determinant of the interest rate the borrowers receive. However, investors pick and choose which loans to invest in through the platform, so are incentivized to try to further predict which loans will default and which will remain in good standing. Indeed, many small business have sprung up which offer predictive analytics services to help investors choose which loans to invest in.

Additionally, there is reason to believe that Lending Club's rating system is not as accurate as claimed. On 9 May, 2016, the CEO of Lending Club was forced to resign over the sale of packaged loans to institutional investors that were not accurately risk-assessed. Clearly, accurate classification is still a challenge for Lending Club itself.

Lending Club Loan Data
======================

Lending Club provides complete loan data for all loans issued between 2007 and the present. Represented are 111 features, including financial information collected by Lending Club, borrower-reported information, and various details about the individual loans.

We will examine only the data from 2014. Because the terms of the loans that Lending Club originates are either 36 or 60 months, far fewer loans from more recent time periods are finalized as either fully paid or defaulted. By examining 2014 we will be working with more complete data.

Data Exploration and Preparation
================================

We have 235,629 observations of 111 features, as expected. However, we have a lot of clean up to do before classification can be attempted.

A number of variables are entirely made up of `NA` values. These columns will simply be dropped.

The `loan_status` variable includes a number of possible statuses other than simply default or good standing. A loan can also be fully paid, charged off (given up as lost by lending club), in a grace period (1-15 days late), or late by up to 30 days or up to 120 days. For loans originating in 2014, 70,690 have been fully paid off, 20207 are in default or charged off, 138,006 are in good standing, and the remainder are late. To keep things simple, we will begin by simply predicting whether a loan remains in good standing or not. To this end we will create a new label variable that classifies loans as good if they are current, fully paid, or in the grace period, and bad if they are late, in default, or charged off.

Many variables are inappropriate for use in classification, such as the unique id assigned to each borrower and a number of variables relating to the cash flows after loan origination. These will simply be dropped.

Lastly, we must address the complications which are the borrower-created fields. Specifically, these are the loan application title and description, and employment title. The title largely overlaps with the purpose, another variable we have, but is less consistent since the purpose is one of 13 options while the title is borrower-written. We will simply drop the title. The description may contain useful information that could be extracted through techniques such as sentiment analysis, however for this exercise we will attempt to classify without it and this column will also be dropped. Finally, the employment title contains 75,355 unique values which means that on average there are 3 loans in the data set per job title. Treating job title as a factor would likely lead to overfitting by simply identifying the unique job titles that corresponded to the most defaults, so for now we will simply drop the column.

We are then left with 69 features and 1 classifier, which we will try to predict.

Fitting a Model
===============

First we will try a boosted logistic regression model. We hope this will provide satisfactory results because logistic regression is not a computationally intensive technique and therefore we are able to make use of a good amount of the data for training.

Training the logistic regression model with the `caret` package providing 10-fold cross-validation on a training set of size 23,564 observations takes 3 minutes and creates a model in which the interest rate, grade assigned by Lending Club, and certain location information provide the most predictive power.

Evaluating the Model
====================

On the test set, the boosted logistic regression achieves 89.3693% accuracy, 50.2577% sensitivity, and 89.5129% specificity.

| predicted |  bad  |  good  |  total |
|:---------:|:-----:|:------:|:------:|
|    bad    |  390  |   386  |   776  |
|    good   | 22158 | 189131 | 211289 |
|   total   | 22548 | 189517 | 212065 |

Note that the test set contains 22,548 bad loans and 189,517 loans so the naive model that assumes all loans will be good achieves 89.3674% accuracy. We have barely managed to beat that benchmark with our boosted logistic regression model.

Improving Model Performance
===========================

To improve performance, we must turn to more exotic models. We fit a random forest with the `caret` package providing 2-fold cross-validation on a training set of size 2357 observations. Training the model takes 20 minutes.

The random forest model selected is in fact the naive model that predicts all loans will be good loans. This means that with only 2357 observations we were unable to find any decision rules that improve prediction over simply predicting the majority class. We thus have 89.3674% accuracy, 0% sensitivity, and 100% specificity.

| predicted |  bad  |  good  |  total |
|:---------:|:-----:|:------:|:------:|
|    bad    |   0   |    0   |    0   |
|    good   | 24803 | 208469 | 233272 |
|   total   | 24803 | 208469 | 233272 |

Random forests are a very computationally intensive class of model to fit, so while these results are disappointing, they are compromised by our unwillingness to wait more than 30 minutes for an algorithm to be evaluated. If more computational power or time were available, a random forest could be fit on a greater proportion of the test data and with more folds of cross- validation. It is likely that doing so would see an actual improvement in predictive power.

Final Conclusions
=================

It is possible that this is in fact an intractable problem. Note that Lending Club does a fairly effective job of rating loans prior to opening them up for investment. An examination of the default rate by assigned grade reveals that the default rate increases steadily from A to G grade loans:

| Grade | Default Rate |
|:-----:|:------------:|
|   A   |     .0312    |
|   B   |     .0662    |
|   C   |     .1094    |
|   D   |     .1503    |
|   E   |     .2014    |
|   F   |     .2451    |
|   G   |     .3015    |

Clearly, therefore, Lending Club's analysis captures the vast majority of the available information. Note also that even G-grade loans have a default rate below 50%, meaning that if there is no more available information to predict default then the best possible model must simply predict the majority class, as our random forest model did.

Appendix
========

*Code used in the creation of this report*

``` r
# read in loan data
print("Reading data...")
setwd("E:\\My Documents\\STATs\\6620\\lendingclub")
df.loans <- read.csv("LoanStats3c.csv", skip=1, nrows=235629)
print("...done!")

# drop NA columns
print("Massaging data...")
to.drop <- c(54:56, 60:70, 72:74)
df.loans <- df.loans[-to.drop]

# create indicator for good or bad loans
good <- df.loans$loan_status %in% c("Current", "Fully Paid", "In Grace Period")
df.loans$status <- as.factor(ifelse(good, "good", "bad"))

# drop columns inappropriate for classification
to.drop <- c(1:2, 4:5, 16:19, 37:48, 52:53)
df.loans <- df.loans[-to.drop]

# drop borrower-generated columns
to.drop <- c(7, 12, 14)
df.loans <- df.loans[-to.drop]

# convert interest rates from factors to numerics
df.loans$int_rate <- as.numeric(gsub("%", "", df.loans[, 3]))

# convert first credit history date from factor to date
list.splitdate <- strsplit(as.character(df.loans[, 16]), "-")
months <- as.character(lapply(list.splitdate, function(x) x[1]))
months <- as.character(match(months, month.abb))
months <- ifelse(nchar(months) == 1, paste("0", months, sep=""), months)
years <- as.character(lapply(list.splitdate, function(x) x[2]))
df.loans$earliest_cr_line <- as.Date(paste(years, months, "01", sep="-"))

# convert revolving utilization from factor to numeric
df.loans$revol_util <- as.numeric(gsub("%", "", df.loans[, 23]))

# convert last credit pull date from factor to date
list.splitdate <- strsplit(as.character(df.loans[, 26]), "-")
months <- as.character(lapply(list.splitdate, function(x) x[1]))
months <- as.character(match(months, month.abb))
months <- ifelse(nchar(months) == 1, paste("0", months, sep=""), months)
months <- ifelse(is.na(months), "01", months)
years <- as.character(lapply(list.splitdate, function(x) x[2]))
years <- ifelse(is.na(years), "2000", years)
df.loans$last_credit_pull_d <- as.Date(paste(years, months, "01", sep="-"))

# clean up NAs since the random forest model doesn't like them
# all NAs are truly not applicable, eg in "mths_since_last_delinq" NA means
#   never had any delinquencies
# therefore we will code them as separate classes for factors and as -1 for
#   numerics
idx.na <- is.na(df.loans$mths_since_last_delinq)
df.loans$mths_since_last_delinq <- ifelse(idx.na, -1,
                                          df.loans$mths_since_last_delinq)
idx.na <- is.na(df.loans$mths_since_last_record)
df.loans$mths_since_last_record <- ifelse(idx.na, -1,
                                          df.loans$mths_since_last_record)
idx.na <- is.na(df.loans$revol_util)
df.loans$revol_util <- ifelse(idx.na, -1, df.loans$revol_util)
idx.na <- is.na(df.loans$mths_since_last_major_derog)
df.loans$mths_since_last_major_derog <- ifelse(idx.na, -1,
                                               df.loans$mths_since_last_major_derog)
idx.na <- is.na(df.loans$avg_cur_bal)
df.loans$avg_cur_bal <- ifelse(idx.na, -1, df.loans$avg_cur_bal)
idx.na <- is.na(df.loans$bc_open_to_buy)
df.loans$bc_open_to_buy <- ifelse(idx.na, -1, df.loans$bc_open_to_buy)
idx.na <- is.na(df.loans$bc_util)
df.loans$bc_util <- ifelse(idx.na, -1, df.loans$bc_util)
idx.na <- is.na(df.loans$mo_sin_old_il_acct)
df.loans$mo_sin_old_il_acct <- ifelse(idx.na, -1, df.loans$mo_sin_old_il_acct)
idx.na <- is.na(df.loans$mths_since_recent_bc)
df.loans$mths_since_recent_bc <- ifelse(idx.na, -1,
                                        df.loans$mths_since_recent_bc)
idx.na <- is.na(df.loans$mths_since_recent_bc_dlq)
df.loans$mths_since_recent_bc_dlq <- ifelse(idx.na, -1,
                                            df.loans$mths_since_recent_bc_dlq)
idx.na <- is.na(df.loans$mths_since_recent_inq)
df.loans$mths_since_recent_inq <- ifelse(idx.na, -1,
                                         df.loans$mths_since_recent_inq)
idx.na <- is.na(df.loans$mths_since_recent_revol_delinq)
df.loans$mths_since_recent_revol_delinq <- ifelse(idx.na, -1,
                                                  df.loans$mths_since_recent_revol_delinq)
idx.na <- is.na(df.loans$num_tl_120dpd_2m)
df.loans$num_tl_120dpd_2m <- ifelse(idx.na, -1, df.loans$num_tl_120dpd_2m)
idx.na <- is.na(df.loans$percent_bc_gt_75)
df.loans$percent_bc_gt_75 <- ifelse(idx.na, -1,
                                    df.loans$percent_bc_gt_75)

# separate out a small sample because training on the full set takes hours
library(caret)
library(randomForest)
library(e1071)
set.seed(6620)
in.train <- createDataPartition(y=df.loans$status, p=.1, list=FALSE)
train.loans <- df.loans[in.train, ]
test.loans <- df.loans[-in.train, ]
print("...done!")

# start multi-core cluster
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# train boosted logistic regression
print("Training boosted logit...")
print(Sys.time())
start <- Sys.time()
blr.model <- train(status ~ ., data=train.loans, method="LogitBoost",
                   trControl=trainControl(method="cv", number=10))
print("...done!")
print(Sys.time() - start)

# evaluate model performance
library(gmodels)
blr.pred <- predict(blr.model, newdata=test.loans)
CrossTable(blr.pred, test.loans$status, prop.r=FALSE, prop.t=FALSE,
           prop.chisq=FALSE)
print(mean(blr.pred == test.loans$status))

# train random forest with 2-fold cross validation
set.seed(6620)
in.train <- createDataPartition(y=df.loans$status, p=.01, list=FALSE)
train.loans <- df.loans[in.train, ]
test.loans <- df.loans[-in.train, ]
print("Training random forest...")
start <- Sys.time()
print(start)
rf.model <- train(status ~ ., data=train.loans, method="rf",
                  trControl=trainControl(method="cv", number=2),
                  proximity=TRUE, allowParallel=TRUE)
print("...done!")
print(Sys.time() - start)

# evaluate model performance
rf.pred <- predict(rf.model, newdata=test.loans)
CrossTable(rf.pred, test.loans$status, prop.r=FALSE, prop.t=FALSE,
           prop.chisq=FALSE)
print(mean(rf.pred == test.loans$status))

# stop the multi-core cluster
stopCluster(cl)
```
