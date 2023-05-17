getwd()
install.packages ("corrplot")
rd <- read.csv("banktest1.csv") 
dfclean <- rd [,-c(9:11)]
dfclean2  <- dfclean [,-c(13:17)]
dfclean3 <- dfclean2 [,-c(2:5, 7:8)]
cor(dfclean3)

#Plot Correlation Map
library("corrplot")
corrplot(cor(dfclean3), method="number")