library(rpart)
library(rpart.plot)
library(caret)
library(neuralnet)
library(gplots)

banktest1 <- read.csv("~/banktest1.csv")

# Select columns of interest
columns <- c("age", "new_marital", "new_education", "new_housing", "y")
bank_clean <- banktest1[columns]

# Remove rows with missing values
bank_clean <- bank_clean[complete.cases(bank_clean), ]

# Convert target variable to numeric, handling non-numeric values
bank_clean$y <- ifelse(bank_clean$y == "no", 0, 1)

# Set seed for reproducibility
set.seed(777)

# Split data into training and testing sets
train.index <- sample(1:nrow(bank_clean), 0.7 * nrow(bank_clean))
bank_clean.train <- bank_clean[train.index, ]
bank_clean.test <- bank_clean[-train.index, ]

# Fit neural network
nn <- neuralnet(new_education ~ ., data = bank_clean.train, hidden = 2, act.fct = "logistic", linear.output = FALSE)

# Plot neural network
plot(nn)

# Make predictions on the test data
nn.predict <- predict(nn, bank_clean.test[, -ncol(bank_clean.test)])

# Convert predictions to binary values
predicted_classes <- ifelse(nn.predict > 0.5, 1, 0)

# Evaluate model performance
confusion_matrix <- table(Actual = bank_clean.test$new_education, Predicted = predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Check if positive class exists in the predictions
if ("1" %in% colnames(confusion_matrix) && "1" %in% rownames(confusion_matrix)) {
  precision <- confusion_matrix["1", "1"] / sum(confusion_matrix[, "1"])
  recall <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
  f1_score <- 2 * precision * recall / (precision + recall)
} else {
  precision <- 0
  recall <- 0
  f1_score <- 0
}

# Create a new confusion matrix with additional rows and columns
new_confusion_matrix <- matrix(0, nrow = nrow(confusion_matrix) + 2, ncol = ncol(confusion_matrix) + 2)
rownames(new_confusion_matrix) <- c(rownames(confusion_matrix), "TN", "FP")
colnames(new_confusion_matrix) <- c(colnames(confusion_matrix), "FN", "TP")

# Copy values from the original confusion matrix to the new matrix
new_confusion_matrix[1:nrow(confusion_matrix), 1:ncol(confusion_matrix)] <- confusion_matrix

# Plot the confusion matrix using heatmap.2
heatmap.2(new_confusion_matrix, trace = "none", col = c("lightgray", "lightblue"),
          main = "Confusion Matrix", xlab = "Predicted", ylab = "Actual",
          margins = c(8, 8))

# Print the evaluation metrics
cat("\nAccuracy:", accuracy)
cat("\nPrecision:", precision)
cat("\nRecall:", recall)
cat("\nF1-Score:", f1_score)
