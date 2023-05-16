library(e1071)
library(caret)

bank_clean1<-bank_clean
bank_clean1$y <- factor(bank_clean1$y)
bank_clean1$y <- ifelse(bank_clean1$y == "yes", 1, 0)

set.seed(777)

train.index <- sample(1:nrow(bank_clean1), 0.7*nrow(bank_clean1))
bank_clean1.train <- bank_clean1[train.index,]
dim(bank_clean1.train)
## select the 30% left as the testing data
bank_clean1.test <- bank_clean1[-train.index,]
dim(bank_clean1.test)

# Build the SVM model
svm_model <- svm(y ~ ., data = bank_clean1.train, kernel = "radial")
formula<-y~age+marital+education+housing

# Visualize the model using a scatterplot
plot(svm_model, bank_clean1.train, formula)


# Make predictions on the test data
bank_clean1.predictions <- predict(svm_model, bank_clean1.test, type = "class")
head(bank_clean1.predictions)


# Create a confusion matrix
confusion_matrix <- confusionMatrix(bank_clean1.predictions, bank_clean1.test$y)
print(confusion_matrix)

# Calculate accuracy, precision, and recall
cm <- confusionMatrix(bank_clean1.predictions, bank_clean1.test$y)
accuracy <- cm$overall["Accuracy"]
print(accuracy)
precision <- cm$byClass["Pos Pred Value"]
print(precision)
recall <- cm$byClass["Sensitivity"]
print(recall)
