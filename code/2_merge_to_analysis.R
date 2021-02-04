#########################################################################################################

# R script to merge week level new covid cases, deaths, repr. rate to country mentions in covid articles

#########################################################################################################


rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
require(forcats)

# set data directory

data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data/analysis_covid/data"

# read country mentions in TAGS

country_mentions_in_tags <- read_csv(paste(data_dir,"country_mentions_in_covid_tags.csv",sep="/")) %>% select(-1)

# read country mentions in TEXT

country_mentions_in_text <- read_csv(paste(data_dir,"country_mentions_in_covid_text.csv",sep="/")) %>% select(-1)
# read weekly covid data

covid_by_week <- read_csv(paste(data_dir,"covid_data_week.csv",sep="/")) %>% select(-1)



# outer merge tags and tex mentions
outer_merge <- merge(x = country_mentions_in_text, y = country_mentions_in_tags, 
               by = c("week","page","iso3","month","country_hun"), all=TRUE) %>% 
               arrange(iso3,page,week)

# fill NA-s with 0

outer_merge <- outer_merge %>% replace_na(list(mentions_tags = 0, mentions_text = 0))

# merge  with covid week data

outer_merge <- merge(x = outer_merge, y = covid_by_week %>% 
               select(weekly_new_cases_permillion,weekly_new_deaths_permillion,reproduction_rate,week,iso3), 
               by = c("week","iso3"), all.x = TRUE) %>% 
               replace_na(list(weekly_new_cases_permillion = 0, weekly_new_deaths_permillion = 0,reproduction_rate = 0))

outer_merge <- merge(x = outer_merge, y = covid_by_week %>% select(iso3,continent) %>% distinct(), by = "iso3", all.x = TRUE)

# create dummy variable if country was mentioned
outer_merge <- outer_merge %>% rename(mention_count_in_tags = mentions_tags, mention_count_in_text = mentions_text) %>%
               mutate(mentioned_in_tags = ifelse(mention_count_in_tags>0,1,0),
               mentioned_in_text = ifelse(mention_count_in_text>0,1,0))

summary(outer_merge)

# Save file
write.csv(outer_merge,paste(data_dir,"data_for_regression.csv",sep="/"))


