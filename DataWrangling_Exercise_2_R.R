
# 0- Load and put the first row of file as a column header
library(readr)
library(dplyr)
library(tidyr)
titanic_original<- read.csv("titanic_original.csv", header = TRUE, sep = ",", fill=TRUE)
# Understand the data
str(titanic_original)
summary((titanic_original))
titanic_original <- tbl_df(titanic_original)
# 1: Port of embarkation - Find the missing values and replace them with S
titanic_replaced <- titanic_original %>% 
  mutate(embarked = replace(embarked, embarked == '', 'S'))
summary(titanic_replaced)

# 2- Age - Calculate the mean of the Age column and use that value to populate the missing values
titanic_replaced %>%
  age_mean <- mean(titanic_replaced$age, na.rm=TRUE)
missing <- which(is.na(titanic_replaced$age))
titanic_replaced$age[missing] <- age_mean
summary(titanic_replaced$age)

# 3- Lifeboat - Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'
titanic_replaced <- titanic_replaced %>% 
  mutate(boat = replace(boat, boat == '', NA))
summary(titanic_replaced$boat)

# 4- Cabin -  Create a new column has cabin_number which has 1 if there is a cabin number, and 0 otherwise.
titanic_replaced <- titanic_replaced %>%
  mutate(cabin, cabin_number = ifelse(cabin == "", "0","1"))

# 5- Submit
titatnic_clean <- titanic_replaced
View(titatnic_clean)
write.csv(titatnic_clean,file="titatnic_clean.csv")
