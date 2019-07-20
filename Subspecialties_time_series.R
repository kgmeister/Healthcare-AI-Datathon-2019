setwd("C:/Programs and stuff/R Projects/Datathon 2019")
install.packages ('tidyverse')
library ('tidyverse')
library (lubridate)
library(stringr)


#Loading the Subspecialties dataset
subs <- read.csv("Subspecialties.csv")


head(cleaned_sub)
class(subs$adm__datetime)


#Changing admission date to character
subs$adm__datetime <- as.character(subs$adm__datetime)


#Cleaned subspecialties
cleaned_sub <- subs %>%
  mutate(adm__datetime = gsub("UTC", "", adm__datetime)) %>%
  mutate(adm__datetime = gsub("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", "", adm__datetime)) %>%
  mutate(adm__datetime = as.Date(adm__datetime, "%Y-%m-%d"))


#Taking out patient IDs
cleaned_sub <- cleaned_sub[,c("adm__datetime", "no_of_subspecialities")]


#Finding out mean number of subspecialties for every day
cleaned_sub <- cleaned_sub %>%
  group_by(adm__datetime) %>%
  summarise(avg_num_disease=mean(no_of_subspecialities))


#Exporting into csv
write.csv(cleaned_sub, 'cleaned_sub.csv')
