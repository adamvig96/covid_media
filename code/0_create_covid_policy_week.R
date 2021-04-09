rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)


data_dir = "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/github/media_network/media_data/analysis_covid/data"

data_on_covid_policy <- read.csv(paste0(data_dir,"/covid-policy-tracker-master/data/OxCGRT_latest.csv",sep=""))%>% select(-1)

data_on_covid_policy$week <- strftime(data_on_covid_policy$Date, format = "%V")
data_on_covid_policy$year <- strftime(data_on_covid_policy$Date, format = "%Y")
data_on_covid_policy$month <- strftime(data_on_covid_policy$Date, format = "%m")


data_on_covid_policy <- data_on_covid_policy %>% rename(C1_school_closing = C1_School.closing,
                                C2_workplace_closing = C2_Workplace.closing,
                                C3_close_public_events = C3_Cancel.public.events,
                                C4_restrictions_on_gatherings = C4_Restrictions.on.gatherings,
                                C5_close_public_transport = C5_Close.public.transport,
                                C6_stay_at_home_req = C6_Stay.at.home.requirements,
                                C7_restrictions_on_internal_movement = C7_Restrictions.on.internal.movement,
                                C8_international_travel_controls = C8_International.travel.controls,
                                E1_income_support = E1_Income.support,
                                E3_fiscal_measures = E3_Fiscal.measures,
                                E2_debt_contract_relief = E2_Debt.contract.relief,
                                E4_international_support = E4_International.support,
                                H1_public_information_campaigns = H1_Public.information.campaigns,
                                H2_testing_policy = H2_Testing.policy,
                                H3_contact_tracing = H3_Contact.tracing,
                                H4_emergency_investment_in_healthcare = H4_Emergency.investment.in.healthcare,
                                H5_investment_in_vaccines = H5_Investment.in.vaccines,
                                H6_facial_coverings = H6_Facial.Coverings,
                                H7_vaccination_policy = H7_Vaccination.policy,
                                )


data_on_covid_policy <- data_on_covid_policy %>% group_by(CountryCode,week) %>% 
  summarise(year = first(year),
         month = first(month),
         C1_school_closing = first(C1_school_closing),
         C2_workplace_closing = first(C2_workplace_closing),
         C3_close_public_events = first(C3_close_public_events),
         C4_restrictions_on_gatherings = first(C4_restrictions_on_gatherings),
         C5_close_public_transport = first(C5_close_public_transport),
         C6_stay_at_home_req = first(C6_stay_at_home_req),
         C7_restrictions_on_internal_movement = first(C7_restrictions_on_internal_movement),
         C8_international_travel_controls = first(C8_international_travel_controls),
         E1_income_support = first(E1_income_support),
         E2_debt_contract_relief = first(E2_debt_contract_relief),
         E3_fiscal_measures = sum(E3_fiscal_measures),
         E4_international_support = sum(E4_international_support),
         H1_public_information_campaigns = first(H1_public_information_campaigns),
         H2_testing_policy = first(H2_testing_policy),
         H3_contact_tracing = first(H3_contact_tracing),
         H4_emergency_investment_in_healthcare = sum(H4_emergency_investment_in_healthcare),
         H5_investment_in_vaccines = sum(H5_investment_in_vaccines),
         H6_facial_coverings = first(H6_facial_coverings),
         H7_vaccination_policy = first(H7_vaccination_policy),
         )

data_on_covid_policy <- data_on_covid_policy %>% rename(iso3 = CountryCode)

write_csv(data_on_covid_policy,paste(data_dir,"covid_policy_week.csv",sep="/"))


