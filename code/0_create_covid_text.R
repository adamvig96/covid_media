########################################################################################################

# R script to filter covid related articles in selected categories (columns) of online newpaper articles

########################################################################################################



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
### Read data

data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data"

text_data <- read_csv(paste(data_dir,"clean_text/2020/all_site_2020.csv",sep="/"))

# prepare for filtering

text_data$text = tolower(text_data$text)
text_data$tags = tolower(text_data$tags)

# define covid tags
covid_tags <- c("koronavírus","covid-2019","sars-cov","covid-19","covid")

# define death tags
death_tags <- c("halál","elhuny","meghal")

# select articles based in covid tag apperance in text or tags

text_data$covid_in_tags <-as.numeric(grepl( paste(covid_tags, collapse="|"), text_data$tags))
text_data$covid_in_text <-as.numeric(grepl( paste(covid_tags, collapse="|"), text_data$text))

text_data$death_in_text <-as.numeric(grepl( paste(death_tags, collapse="|"), text_data$text, perl = TRUE))


table(text_data %>% select(covid_in_tags,covid_in_text,death_in_text))


# Filter out covid stories

covid_tags_1_text_0 <- text_data%>% 
  filter(covid_in_tags==1 & covid_in_text ==0) %>% select(date,page,text,tags,category)
  
View(covid_tags_1_text_0[sample(nrow(covid_tags_1_text_0), 50), ])

covid_tags_0_text_1 <- text_data%>% 
  filter(covid_in_tags==0 & covid_in_text ==1) %>% select(date,page,text,tags,category)
  
View(covid_tags_0_text_1[sample(nrow(covid_tags_0_text_1), 50), ])

covid_tags_1_text_1 <-text_data%>% 
  filter(covid_in_tags==1 & covid_in_text ==1) %>% select(date,page,text,tags,category)

View(covid_tags_1_text_1[sample(nrow(covid_tags_1_text_1), 50), ])



# check distribution of covid related articles among categories (rovat)

covid_in <- text_data%>% 
  filter(covid_in_tags==1 & covid_in_text ==1) %>% select(date,page,text,tags,category)


covid_in%>% filter(page == "24.hu") %>%
  count(category) %>%
  mutate(category = fct_reorder(category, n, .desc = TRUE)) %>%
  ggplot(aes(x = category, y = n)) + geom_bar(stat = 'identity') +
  ggtitle("Distribution of covid related articles \n in categories - 24")

grouped_category <- covid_in %>% 
  group_by(page,category) %>% count("text") %>% arrange(desc(n)) %>% arrange(page)


#write.xlsx(as.data.frame(grouped_category), paste(data_dir,"analysis_covid/data/covid_articles_in_categories.xlsx",sep="/"))

# read database filtered on categories

filter_categories <- read_excel(paste(data_dir,"analysis_covid/data/covid_articles_in_categories_filtered.xlsx",sep="/"))


text_cat_filt <- text_data %>% filter(category %in% filter_categories$category)

#select covid related articles

covid_in <- text_cat_filt%>% 
  filter(covid_in_tags==1 & covid_in_text ==1) %>% select(date,page,text,tags,category,death_in_text)



#Save file
write.csv(covid_in,paste(data_dir,"analysis_covid/data/covid_text.csv",sep="/"))
