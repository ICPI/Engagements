##  PMTCT Analysis
##  8/22/18
##  J Davis
  #   Purpose:This request is for an analyst at ICPI to prepare a modernized collection 
  # of PMTCT data visuals that can easily be updated with new quarterly data by SGAC 
  # Program Quality. This will allow S/GAC staff to easily and compellingly demonstrate 
  # our program progress and impact through graphs and charts when so requested

##  libraries
library(tidyverse)
library(ICPIutilities)

##  File paths

data_path <- "C:/Users/GHFP/Documents/data/8.15_release"

results_path <- "C:/Users/GHFP/Documents/ICPI/PMTCT/results"

old_data <- "C:/Users/GHFP/Documents/data/All Datasets"

##  some objects for the things we want to keep

indc <- c("PMTCT_STAT", "PMTCT_ART", "PMTCT_HEI_POS", "TX_NEW", "TX_RET", "PMTCT_EID", 
          "PMTCT_STAT_POS", "PMTCT_STAT_NewlyIdentified_Negative", "PMTCT_EID_Less_Equal_Two_Months",
          "PMTCT_EID", "PMTCT_EID_Two_Twelve_Months", "PMTCT_STAT_KnownatEntry_POSITIVE",
          "PMTCT_STAT_NewlyIdentified_POSITIVE")

# Mapping for historic to current

map <- tibble::tribble(
  ~Indicator,                  ~indicator_short,
  "PMTCT_ART",           "PMTCT Patients on ART",
  "PMTCT_EID",  "At-Risk Infants Tested for HIV",
  "PMTCT_STAT", "ANC Clients who know HIV Status"
)

## import and filtering MSD to relelvent indicators and reshaping

mer <- read_rds(file.path(data_path, "MER_Structured_Dataset_OU_FY17-18_20180815_v1_1.rds"))%>%
  filter(indicator %in% indc) %>% 
  filter(disaggregate == "Total Numerator") %>% 
  ICPIutilities::add_cumulative() %>% 
  group_by(operatingunit, indicator) %>%
  summarise_at(vars(fy2017apr, fy2018cum, fy2017_targets, fy2018_targets), ~sum(.,na.rm = TRUE))%>%
  ungroup() %>% 
  rename("2017_results" = fy2017apr, "2018_results" = fy2018cum, "2017_targets" = fy2017_targets, "2018_targets" = fy2018_targets) %>% 
  gather(period, value, -operatingunit, -indicator) %>% 
  separate(period, c("year", "type"), sep = "_") %>% 
  mutate_at(vars(year),~as.double(.)) %>% 
  spread(key = type, value = value)


##  pull in old data and drop DSD and TA standalone obs, we only want "dsd + TA"
old <- read_csv(file.path(old_data, "Country and Regional Targets_Results 2004-2016.csv"),
                col_types = cols(.default = "c"),
                na = "null")%>%
  mutate_at(vars(Year, `Measure Value`),~as.double(.)) %>% 
  filter(`DSD/TA` == "DSD+TA", !(`Indicator Short Name` %in% c("TB Patients on ART", "OVC Prevention Services"))) %>% 
  select(-`DSD/TA`) %>%
  rename(operatingunit = `Country/Region`, type = `Measure Name`, value = `Measure Value`, indicator_short = `Indicator Short Name` )%>%
  filter(!(operatingunit == "Global")) %>%
  spread(key = type, value = value)


## join Old and MER data to new dataframe

pmtct <- right_join(old, map, by = "indicator_short") %>% 
  select(-indicator_short, -Bundle) %>% 
  rename_all(~tolower(.)) %>%
  bind_rows(mer) %>% 
  write_csv(file.path(results_path, "PMTCT_data.csv", na=""))















  
  