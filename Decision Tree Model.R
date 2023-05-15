install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(caret)


columns <- c("age","marital","education","housing", "y")

bank_clean<-banktest1[columns]

str(bank_clean)
dim(bank_clean)
summary(bank_clean)
bank_clean$y <- factor(bank_clean$y)

set.seed(777)#Why do you need to set seed?

train.index <- sample(1:nrow(bank_clean), 0.7*nrow(bank_clean))
bank_clean.train <- bank_clean[train.index,]
dim(bank_clean.train)
## select the 30% left as the testing data
bank_clean.test <- bank_clean[-train.index,]
dim(bank_clean.test)

# Default decision tree model
# Builds a decision tree from the banktest1 dataset to predict
# species given all other columns as predictors
bank_clean.tree <- rpart(y~.,data=bank_clean.train, method="class",control =rpart.control(minsplit =1,minbucket=6, cp=0))

# Reports the model
print(bank_clean.tree)

## VISUALIZE THE MODEL
## plot the tree structure
library(rpart.plot)
rpart.plot(bank_clean.tree)
## print the tree structure
summary(bank_clean.tree)

## MODEL EVALUATION
## make prediction using decision model
bank_clean.predictions <- predict(bank_clean.tree, bank_clean.test, type = "class")
head(bank_clean.predictions)

## Comparison table
bank_clean.comparison <- bank_clean.test
bank_clean.comparison$Predictions <- bank_clean.predictions
bank_clean.comparison[ , c("y", "Predictions")]

## View misclassified rows
disagreement.index <- bank_clean.comparison$y != bank_clean.comparison$Predictions
bank_clean.comparison[disagreement.index,]
##Confusion Matrix
confusion_matrix <- confusionMatrix(bank_clean.predictions, bank_clean.test$y)
print(confusion_matrix)


# Calculate accuracy, precision, and recall
cm <- confusionMatrix(bank_clean.predictions, bank_clean.test$y)
accuracy <- cm$overall["Accuracy"]
print(accuracy)
precision <- cm$byClass["Pos Pred Value"]
print(precision)
recall <- cm$byClass["Sensitivity"]
print(recall)
