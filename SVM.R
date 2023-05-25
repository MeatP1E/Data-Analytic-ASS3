library(e1071)
library(caret)

bank_clean1<-bank_clean
bank_clean$y <- factor(bank_clean$y)

set.seed(777)

# Build the SVM model
svm_model <- svm(y ~ ., data = bank_clean.train, kernel = "radial")
formula<-y~age+marital+education+housing

# Visualize the model using a scatterplot
plot(svm_model, bank_clean.train, formula)
model <- svm(y ~ ., data = bank_clean.train)
plot(cmdscale(dist(bank_clean.train[,-5])),
     col = as.integer(bank_clean.train[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])


# Make predictions on the test data
bank_clean.predictions <- predict(svm_model, bank_clean.test, type = "class")
head(bank_clean.predictions)


# Create a confusion matrix
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

