library(dplyr)
library(tidyr)

banktest1 <- bank


banktest1 <- banktest1[, -which(names(banktest1) == "poutcome")]

# find rows with "unknown" values and remove them
banktest1 <- banktest1 %>% 
  filter_all(any_vars(!is.na(.) & . != "unknown"))

#change unknown to other value in job column
mode_val <- as.character(names(which.max(table(banktest1$job))))

banktest1$job <- ifelse(banktest1$job == "unknown", mode_val, banktest1$job)
print(banktest1)


#change unknown to other value in education column
mode_val <- as.character(names(which.max(table(banktest1$education))))

banktest1$education <- ifelse(banktest1$education == "unknown", mode_val, banktest1$education)
print(banktest1)


#change unknown to other value in contact column 
mode_val <- as.character(names(which.max(table(banktest1$contact))))

banktest1$contact <- ifelse(banktest1$contact == "unknown", mode_val, banktest1$contact)
print(banktest1)


#Adding marital variable (single/divorced) with a Dummy variable 0/1
banktest1$new_marital <- ifelse(banktest1$marital == "single", 1, 
                                ifelse(banktest1$marital == "divorced", 0, 
                                       ifelse(banktest1$marital == "married", 2, NA)))


#Adding education variable with a Dummy variable 0/1
banktest1$new_education <- ifelse(banktest1$education == "primary", 0,
                                  ifelse(banktest1$education == "secondary", 1,
                                         ifelse(banktest1$education == "tertiary", 2, NA)))


#adding housing variable (yes/no) with dummy variable 0/1 
banktest1$new_housing <- ifelse(banktest1$housing == "no", 0, 1)


# check how many "unknown" values are left
banktest1 %>%
  summarise_all(list(~sum(. == "unknown"))) %>%
  gather(key = "variable", value = "nr_unknown") %>%
  arrange(-nr_unknown)

