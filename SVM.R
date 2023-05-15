library(e1071)


# Make predictions on the test data
bank_clean.predictions <- predict(bank_clean.tree, bank_clean.test, type = "class")
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
