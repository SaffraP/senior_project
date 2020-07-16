# Load libraries
library(tidyverse)


# RF on small_train
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
# Somewhere along the way I lost the HasDetections column. I need to add that back in before I can run anything. 
# rf <- randomForest(
#   num ~ .,
#   data=small_train
# )




# 7-15-20
# Running a RF in R
# https://www.youtube.com/watch?v=dJclNIN-TPo

# Read in the data
data <- small_train
# set seed
set.seed(123)
# Split the data
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3)) # I could also do a validation set
train <- data[ind == 1, ]
test <- data[ind == 2, ]


# First try a logistic regression
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# 325 notebook
model <- glm(HasDetections ~., data = train, family = binomial(link='logit'))
summary(model)
anova(model, test="Chisq")
# pchisq(848464, 620489, lower.tail = FALSE) # This came from the notebook but doesn't give an output... concerning
# See the link above for methods on how to rate the fit. (like R2 or ROC)


# Second, run a RF
library(data.table)
library(mlr)
library(h2o)
# "set the data class to data.table. data.table is the most powerful R package made for faster data manipulation."
setDT(train)
setDT(test)
# Impute missing values. See article for how to do that.
# Insure the data is balanced (already did). Aka 50% infected and 50% not
setDT(train)[,.N/nrow(train), HasDetections]
setDT(test)[,.N/nrow(test), HasDetections]
## BAGGING
#create a task
traintask <- makeClassifTask(data = train, target = "HasDetections")
testtask <- makeClassifTask(data = test, target = "HasDetections")
#create learner: grow 100 trees on randomized samples of data with replacement.
bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)
# set 5 fold cross validation
rdesc <- makeResampleDesc("CV",iters=5L)
#set parallel backend (Windows)
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
## RANDOM FOREST
#make randomForest learner
library(randomForest)
library(xml2)
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)
# ^ that code should output several meetrics. acc.test.mean is probs what to look at
# Because the false positives were too high, we're retraining with adjusted cutoffs
#set cutoff
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE, cutoff = c(0.75,0.25))
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)
# Now it's time to tune the model
getParamSet(rf.lrn)
#set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),makeIntegerParam("nodesize",lower = 10,upper = 50))
#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)
#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)
#start tuning
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(acc), par.set = params, control = ctrl, show.info = T)

















