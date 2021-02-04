##########################################################################################

# R script to create database of country mentions in TEXT of online newpaper articles

##########################################################################################


rm(list=ls())

library(quanteda)
library(readr)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(xlsx)
require(forcats)

# Read data

data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data/analysis_covid/data"

covid_text <- read_csv(paste(data_dir,"covid_text.csv",sep="/")) %>% select(-1)

countries <-  read_csv(paste(data_dir,"country_names_en_hun.csv",sep="/")) %>% select(-1)

# create week and month variable
covid_text$week <- strftime(covid_text$date, format = "%V")

covid_text$month <-strftime(covid_text$date, format = "%m")

# Unify categories

covid_text$category <- tolower(covid_text$category)

covid_text <- covid_text %>% 
  mutate(category = gsub("á", "a", category)) %>% 
  mutate(category = gsub("ö", "o", category)) %>%
  mutate(category = gsub("ü", "u", category))  %>%
  mutate(category = gsub("é", "e", category))  %>%
  mutate(category = gsub("fn", "gazdasag", category)) %>% # fn is gazdasag in 24.hu
  mutate(category = gsub("egeszsegugy", "egeszseg", category)) %>%
  mutate(category = gsub("itthon", "belfold", category)) %>% #origo
  mutate(category = gsub("nagyvilag", "kulfold", category)) %>% #origo
  mutate(category = gsub("politika", "belfold", category)) %>% # politika is local politics at 444
  mutate(category = gsub("ketharmad", "belfold", category)) %>% # 888
  mutate(category = gsub("amerika-london-parizs", "kulfold", category))  # 888

covid_text %>% count(category) %>% arrange(desc(n))

# further refining needed after filtering for foreign countries

# now filter for articles only in kulfold and gazdasag

covid_text <- covid_text %>% filter((category == "kulfold") | (category == "gazdasag"))



# create list of different versions of USA and UK 
usa_tags <- c("'usa'","'us","egyesült államok","amerikai egyesült államok","'amerika'")
uk_tags <- c("britannia","anglia","egyesült királyság","'uk'")

covid_text$text <- tolower(covid_text$text)

us <- countries %>% filter(code == "US")
for (name in usa_tags){
  covid_text <- covid_text %>% 
    mutate(text = gsub(name, us[1,"country_hun"], text))
}

table(as.numeric(grepl(us[1,"country_hun"], covid_text$tags)))

uk <- countries %>% filter(code == "GB")

for (name in uk_tags){
  covid_text <- covid_text %>% 
    mutate(text = gsub(name, uk[1,"country_hun"], text))
}

table(as.numeric(grepl(uk[1,"country_hun"], covid_text$tags)))



# create variable to check distribution of country tags among rovats
# first check whether a country is among tags

countries$country_hun <-  tolower(countries$country_hun)

# Exclude Hungary
foreign_countries <- countries %>% filter(iso3 != "HUN")

country_tags <- foreign_countries$country_hun


covid_text$foreign_dummy <- as.numeric(grepl( paste(country_tags, collapse="|"), covid_text$text))


covid_text %>% filter(foreign_dummy == 1) %>% count(category) %>% arrange(desc(n))



covid_text %>% filter(foreign_dummy == 1) %>%
  count(category) %>% mutate(category = fct_reorder(category, n, .desc = TRUE)) %>%
  ggplot(aes(x = category, y = n)) + geom_bar(stat = 'identity') + ggtitle("Country related covid articles in categories")


#filter if country is mentioned
covid_text <- covid_text %>% filter(foreign_dummy == 1)


### extract country mentions from dfm

# use TEXT to identify country related articles
covidcorpus <- corpus(covid_text$text)

dict_countries <- dictionary(list(country = countries$country_hun))

dfm_covid <- dfm(covidcorpus)

# create week variable

docvars(dfm_covid, "week")<-covid_text$week
docvars(dfm_covid, "page")<-covid_text$page
dfm_country_mention<-dfm_select(dfm_covid, pattern = dict_countries, selection = "keep")


#covid_text_country_mentions <- cbind(covid_text, convert(dfm_country_mention, to = "data.frame"))

### Aggregate to week, page


country_by_week_dfm<-dfm_group(dfm_country_mention,     groups =c("week", "page"))

country_by_week<-convert(country_by_week_dfm, to = "data.frame")

country_by_week_m<-melt(country_by_week)

names(country_by_week_m)<-c("week_page", "country_hun", "mentions_text")
country_by_week_m$week_page<-gsub("\\.", "_", country_by_week_m$week_page)

country_by_week_m<-country_by_week_m %>%
  separate(week_page, c("week", "page"), "_")


country_by_week_m <- merge(country_by_week_m,countries %>% select(country_hun,iso3),by = "country_hun")


# include month not just week

month_week <- covid_text %>% select(month,week) %>% distinct() %>% arrange(week,month)


country_by_week_m <- merge(x = country_by_week_m, y = month_week, by = "week", all.x = TRUE) %>% 
  arrange(iso3,page,week)


write.csv(country_by_week_m,paste(data_dir,"country_mentions_in_covid_text.csv",sep="/"))



## Read weekly covid data

covid_by_week <- read_csv(paste(data_dir,"covid_data_week.csv",sep="/")) %>% select(-1)







