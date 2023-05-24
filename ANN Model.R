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
nn <- neuralnet(y~ ., data = bank_clean.train, hidden = 2, act.fct = "logistic", linear.output = FALSE)

# Plot neural network
plot(nn)
# Generate predictions on the test dataset
predictions <- predict(nn, bank_clean.test)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(Actual = bank_clean.test$y, Predicted = predicted_classes)

# Create a caret confusion matrix object
cm <- confusionMatrix(confusion_matrix)

# Plot the confusion matrix
plot(cm$table, col = cm$byClass, 
     main = paste("Confusion Matrix\nAccuracy:", cm$overall["Accuracy"]))

# Add labels to the plot
legend("bottomright", legend = c("0", "1"), 
       fill = cm$byClass, title = "Class")

# Print the evaluation metrics
print(cm)
