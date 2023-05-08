install.packages("rpart")

library(rpart)


str(bank)
dim(bank)
summary(bank)

columns <- c("age","marital","education","housing","y")

bank_clean<-bank[columns]

set.seed(777)

train.index <- sample(1:nrow(bank_clean), 0.7*nrow(bank_clean))
bank_clean.train <- bank_clean[train.index,]
dim(bank_clean.train)

bank_clean.test <- bank_clean[-train.index,]
dim(bank_clean.test)



bank_clean.tree <- rpart(y~.,data=bank_clean.train)

# Reports the model
print(bank_clean.tree)

## VISUALIZE THE MODEL
## plot the tree structure
plot(bank_clean.tree, margin=c(.1))
title(main = "Decision Tree Model of bank Data")
text(bank_clean.tree, use.n = TRUE)
## print the tree structure
summary(bank_clean.tree)

## make prediction using decision model
bank.predictions <- predict(bank.tree, iris.test, type = "class")
head(iris.predictions)

## Comparison table
iris.comparison <- iris.test
iris.comparison$Predictions <- iris.predictions
iris.comparison[ , c("Species", "Predictions")]

## View misclassified rows
disagreement.index <- iris.comparison$Species != iris.comparison$Predictions
iris.comparison[disagreement.index,]





