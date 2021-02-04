##########################################################################################

# R script to create database of hungarian and english names of countres with iso3 codes

##########################################################################################

rm(list=ls())

library(readr)
library(dplyr)
library(readxl)

### Read data

data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data"

countries <- read_excel(paste(data_dir,"covid data/ksh_orszagkodlista.xlsx",sep="/"), na = "NaN") %>% 
                      rename(code = Országkód) %>% rename(country_hun = "Ország megnevezése") %>% 
                      rename(country_en = "Ország megnevezése angolul")

#drop organizations, the iso's first letter is an integer

countries <- countries %>% filter(is.na(as.numeric(substring(countries$code, 1, 1))))

# merge with iso3

country_codes_csv <- read_csv(paste(data_dir,"covid data/country-codes_csv.csv",sep="/"), na = "NaN")
isos<-country_codes_csv[,c(3,10)]
names(isos)<-c("iso3", "code")

countries<-merge(countries, isos, by="code")
# Droped during the merge: Koszovói Köztársaság, "Nem besorolható"

#lower uppercase letters

countries$country_hun <- tolower(countries$country_hun)

countries$country_en <- tolower(countries$country_en)

     
write.csv(countries, paste(data_dir,"analysis_covid/data/country_names_en_hun.csv",sep="/"))


