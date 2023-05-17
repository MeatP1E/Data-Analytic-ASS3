library(rpart)
library(rpart.plot)
library(caret)
library(neuralnet)

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
