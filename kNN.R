
library(caret)  

df <- bank ##load data
  columns <- c("age", "duration", "balance")
  bank_two <- df[columns]
  head(bank_two) ## see the studcture


##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(bank_two), 0.9 * nrow(bank_two)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first three coulumns of dataset because they are the predictor
bank_norm <- as.data.frame(lapply(bank_two[,c(1,2,3)], nor))

summary(bank_norm)

##extract training set
bank_train <- bank_norm[ran,] 
##extract testing set
bank_test <- bank_norm[-ran,] 
##extract 1 column of train dataset because it will be used as 'cl' argument in knn function.
bank_target_category <- bank_two[ran,1]
##extract 1 column if test dataset to measure the accuracy
bank_test_category <- bank_two[-ran,1]
##load the package class
library(class)
##run knn function
pr <- knn(bank_train,bank_test,cl=bank_target_category,k=13)


 ##create confusion matrix
 tab <- table(pr,bank_test_category)
 
 ##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
 
 accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
 
accuracy(tab)

precision <- 

