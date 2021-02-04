###############################################################

# R script to aggregate daily covid data to week level in 2020

################################################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

## Covid data (at country/day level)

# Read data
covid_data <- read_csv("/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data/covid data/owid-covid-data.csv")

# create variables, clean data

covid_data<-rename(covid_data, c("country" = "location"))

covid_data$week <- strftime(covid_data$date, format = "%V")

covid_data <- covid_data %>% separate(date, c("year", "month", "day"), "-",convert = TRUE)


# filter for 2020
covid_data <- covid_data %>% filter(year == 2020)


# aggregate daily covid data to week level

covid_by_week <- covid_data %>% group_by(country,week) %>% summarise(
  weekly_new_cases_permillion = sum(new_cases_per_million),
  weekly_new_deaths_permillion = sum(new_deaths_per_million),
  reproduction_rate= mean(reproduction_rate, na.rm = F),
  iso3=unique(iso_code),
  continent = unique(continent)
)

# fill NA-s with 0, because NA means no covid cases and deaths

covid_by_week <- covid_by_week %>% 
  replace_na(list(weekly_new_cases_permillion = 0, weekly_new_deaths_permillion = 0,reproduction_rate = 0))



# Save file

data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data/analysis_covid/data"

write.csv(covid_by_week,paste(data_dir,"covid_data_week.csv",sep="/"))







