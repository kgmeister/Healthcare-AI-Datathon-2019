setwd("C:/Programs and stuff/R Projects/Datathon 2019")
install.packages ('tidyverse')
library ('tidyverse')
library (lubridate)
library(stringr)
library(dplyr)

#Loading the inpatient dataset
inpatient<- read.csv("inpatientcase.csv")


#Creating the patient ID, admission date/time, discharge date time dataframe
Adm_Disc_Stay <- inpatient[, c('case_no', 'adm__datetime', 'dis_datetime')]


#Removing the "UTC" in admission and discharge date/times
Adm_Disc_Stay <- Adm_Disc_Stay %>% 
  mutate(adm__datetime = gsub("UTC", "", adm__datetime)) %>%
  mutate(dis_datetime = gsub("UTC", "", dis_datetime))


#Removing rows containing NA
Adm_Disc_Stay <- Adm_Disc_Stay %>% drop_na()


#Finding duration of stay by subtracting admission date from discharge date
duration <- as.duration (Adm_Disc_Stay$'dis_datetime' %--% Adm_Disc_Stay$'adm__datetime')

#Attaching duration column to Adm_Disc_Stay dataframe
Adm_Disc_Stay[,"stay"] <- duration


Adm_Disc_Stay$'dis_datetime' <- strptime(Adm_Disc_Stay$'dis_datetime', "%Y-%m-%d %H:%M:%S")
Adm_Disc_Stay$'adm__datetime' <- strptime(Adm_Disc_Stay$'adm__datetime', "%Y-%m-%d %H:%M:%S")


#Removing time from the date
Adm_Disc_Stay$'dis_datetime' <- format(as.POSIXct(Adm_Disc_Stay$'dis_datetime',format="%Y-%m-%d %H:%M:%S"),format='%Y-%m-%d')
Adm_Disc_Stay$'adm__datetime' <- format(as.POSIXct(Adm_Disc_Stay$'adm__datetime',format="%Y-%m-%d %H:%M:%S"),format='%Y-%m-%d')



