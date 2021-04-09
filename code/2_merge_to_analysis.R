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

country_mentions_in_tags <- read_csv(paste(data_dir,"country_mentions_in_covid_death_tags.csv",sep="/")) %>% select(-1)

# read country mentions in TEXT

country_mentions_in_text <- read_csv(paste(data_dir,"country_mentions_in_covid_death_text.csv",sep="/")) %>% select(-1)
# read weekly covid data

covid_by_week <- read_csv(paste(data_dir,"covid_data_week.csv",sep="/")) %>% select(-1)

covid_policy_by_week <- read_csv(paste(data_dir,"covid_policy_week.csv",sep="/"))



# outer merge tags and tex mentions
outer_merge <- merge(x = country_mentions_in_text, y = country_mentions_in_tags, 
               by = c("week","page","iso3","month","country_hun"), all=TRUE) %>% 
               arrange(iso3,page,week)

# fill NA-s with 0

outer_merge <- outer_merge %>% replace_na(list(mentions_tags = 0, mentions_text = 0))

# merge  with covid week data

outer_merge <- merge(x = outer_merge, y = covid_by_week %>% 
               select(weekly_new_cases_permillion,weekly_new_deaths_permillion,reproduction_rate,week,iso3), 
               by = c("week","iso3"), all.x = TRUE) %>% replace_na(list(weekly_new_cases_permillion = 0,
                                                                      weekly_new_deaths_permillion = 0,
                                                                      reproduction_rate = 0
               ))
# add continent
outer_merge <- merge(x = outer_merge, y = covid_by_week %>% select(iso3,continent) %>% distinct(),
                     by = "iso3", all.x = TRUE) %>% replace_na(list(continent = "Other"))


# merge with covid policy

covid_policy_by_week[is.na(covid_policy_by_week)]<-0


outer_merge <- merge(x = outer_merge, y = covid_policy_by_week %>% filter(year == 2020) %>% select(-month),
                     by = c("week","iso3"), all.x = TRUE) %>% replace_na(list(year = 2020))



outer_merge <- outer_merge %>% arrange(iso3,week)


# create dummy variable if country was mentioned
outer_merge <- outer_merge %>% dplyr::rename(mention_count_in_tags = mentions_tags, mention_count_in_text = mentions_text) %>%
               mutate(mentioned_in_tags = ifelse(mention_count_in_tags>0,1,0),
               mentioned_in_text = ifelse(mention_count_in_text>0,1,0))

summary(outer_merge)

# Save files
write.csv(outer_merge,paste(data_dir,"data_for_regression_with_policys.csv",sep="/"))


outer_merge_filt <- outer_merge %>% select(week,iso3,page,month,country_hun,mention_count_in_text,mention_count_in_tags,
                       weekly_new_cases_permillion,weekly_new_deaths_permillion,reproduction_rate,continent)


write.csv(outer_merge_filt,paste(data_dir,"data_for_regression_only_death_mention.csv",sep="/"))






