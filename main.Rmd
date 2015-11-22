### PROGRAMMING ASSIGNMENT ###
library(dplyr)
library(caret)
library(ggplot2)

set.seed(1000)

## FUNCTIONS ##
GetSampleError <- function(fold) {
  ftest <- use.training[-fold, ]
  ftrain <- use.training[fold]
  fmodel <- train(classe ~ ., data = ftrain, method = "rpart")
  fprediction <- predict(fmodel, newdata = ftest)
  
  # output accuracy
  #sprintf("Accuracy: %f%%", sum(prediction == test$classe)/length(test$classe) * 100)
  return(sum(fprediction == ftest$classe)/length(ftest$classe) * 100)
}

PmlWriteFiles <- function(x) {
  n = length(x)
  for(i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

## READ IN DATA ##

testing <- read.csv("pml-testing.csv", stringsAsFactors = TRUE)
testing <- testing[,-1]     # remove variable x
testing <- testing[,colSums(is.na(testing))<nrow(testing)]  # remove columns with all NA values

training <- read.csv("pml-training.csv")
training <- training[, -1]  # remove variable x

use.cols <- c("classe", names(testing))
use.col.nums <- match(use.cols[1:59], names(training))
use.training <- training %>% select(use.col.nums)


## MODEL ## 

# Classification tree
model.tree <- train(classe ~ ., data = use.training, method = "rpart")
print(model.tree$finalModel)

# Random forest
model.forest <- train(classe ~ ., data = use.training, method = "rf")
model.forest


## OUT OF SAMPLE ERROR ##

# K-fold Cross validation #
folds <- createFolds(y = use.training, k = 10, list = TRUE, returnTrain = TRUE)
fold.accuracy <- sapply(folds, GetSampleError)

# for single fold, e.g. Fold03
ftest <- use.training[-folds$Fold03, ]
ftrain <- use.training[folds$Fold03]
fmodel <- train(classe ~ ., data = ftrain, method = "rpart")
fprediction <- predict(fmodel, newdata = ftest)
sum(fprediction == ftest$classe)/length(ftest$classe)


## SUBMISSION ##

predictions <- predict(model.tree, newdata = testing)
answers <- as.character(predictions)
PmlWriteFiles(answers)

