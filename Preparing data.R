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
View(banktest1)

#change unknown to other value in education column
mode_val <- as.character(names(which.max(table(banktest1$education))))

banktest1$education <- ifelse(banktest1$education == "unknown", mode_val, banktest1$education)
print(banktest1)
View(banktest1)

#change unknown to other value in contact column 
mode_val <- as.character(names(which.max(table(banktest1$contact))))

banktest1$contact <- ifelse(banktest1$contact == "unknown", mode_val, banktest1$contact)
print(banktest1)
View(banktest1)



# display the resulting data
View(banktest1)

# check how many "unknown" values are left
banktest1 %>%
  summarise_all(list(~sum(. == "unknown"))) %>%
  gather(key = "variable", value = "nr_unknown") %>%
  arrange(-nr_unknown)

