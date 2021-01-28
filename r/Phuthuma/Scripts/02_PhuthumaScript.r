##########################################################################################################
###################### PHUTHUMA R Script for Dashboard Population ########################################
###################### Developed by: Gina Safarty, Yaa Obeng ###########################################
paste(Sys.Date()) # Last Updated: Jan. 15, 2021


#load packages
library(tidyverse)
library(here)
library(readxl)
library(tibble)
library(lubridate)
library(splitstackshape)



# GLOBALS -----------------------------------------------------------------------------
raw<-here("Data")
old<-here("Data/Historical")
prv<-here("Data/Last Month")
'%ni%'<- Negate('%in%')
filename<-paste("historic_siyenza", "_", Sys.Date(), ".txt", sep="")
hist_files<-list.files(old,pattern="siyenza")
raw_files<-list.files(raw,pattern="CDC|USAID")
old_files<-list.files(old, pattern="historic")
prv_files<-list.files(prv, pattern="prev")
numden_ind<-c("HTS_TST_fac", "HTS_TST_POS_fac", "TX_NEW", "TPT_NEW","Index_contacts_tested",
              "Index_contacts_pos", "HTS_TST_com","HTS_TST_POS_com","Headcount")
snapshot_ind<-c("TX_CURR_28_proxy", "TX_CURR_90", "Latemissed")
filename1<-paste("new_InteragencyDash", "_", Sys.Date(), ".txt", sep="")
filename2<-paste("prev_InteragencyDash", "_", Sys.Date(), ".txt", sep="")

curr_mon <- "2020-11" #Change each time you use this script to the month and yr of your current raw dataset

##################### New Data Processing ##########################

# MONTHLY RAW DATA & HISTORICAL IN ---------------------------------------------------------

raw_df<-here("Data",raw_files) %>% 
  map(~ read_xlsx(.x, sheet = which(str_detect(excel_sheets(.x), "_RAW|Siyenza"))))%>% 
  reduce(bind_rows)
  

old_df<-read_tsv("Data/Historical/historic_siyenza_2021-01-27.txt", col_names = T, #Change the file name each time you run to last month's data file
                 col_types = c(
                   HTS_TST_POS_fac = "d",
                   TPT_NEW = "d",
                   TX_NEW = "d")) %>%
  mutate(MechanismID=as.character(MechanismID)) %>%
  rename(Latemissed = LateMissed) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  filter(mon_yr < "2020-10") 

# PROCESS NEW MONTHLY DATA-----------------------------------------------------------------

raw_base<-raw_df %>% 
  rename(Week_End = End_Date,
         Week_Start = Start_Date) %>% 
  mutate(mon_yr= format(Week_End, "%Y-%m"),
         MechanismID=as.character(MechanismID),
         FundingAgency= case_when(FundingAgency== "CDC/HHS" ~ "HHS/CDC",
                                  TRUE ~ FundingAgency)) %>%
  arrange(Facility, mon_yr) %>% 
  mutate(TX_CURR_28_proxy = case_when(is.na(Latemissed) ~ TX_CURR_90,
                                      TRUE ~ (TX_CURR_90 - Latemissed))) %>% 
  #filter(mon_yr == curr_mon) %>%  #CHANGE TO 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  select(c(1:9, 12,10,11,13,14)) 
  
# MERGE Historical and NEW MONTHLY DATA-----------------------------------------------------

base_df<-bind_rows(raw_base,old_df)


# YIELD, PROXY LINKAGE, TEST COVERAGE & TPT INITIATION DATA CALC ----------------------------

numden_df<-base_df %>% 
  filter(indicator %in% numden_ind) %>% 
  group_by(Facility, mon_yr) %>% 
  spread(indicator, val) %>% 
  mutate(yield_fac = case_when(HTS_TST_fac > 0 ~ (HTS_TST_POS_fac/HTS_TST_fac),TRUE ~ 0),
         tested_fac = case_when(HTS_TST_fac > 0 ~ (HTS_TST_fac/Headcount), TRUE ~ 0),
         yield_com = case_when (HTS_TST_com >0 ~ (HTS_TST_POS_com/ HTS_TST_com), TRUE ~ 0),
         proxy_linkage = case_when(HTS_TST_POS_fac > 0 ~ TX_NEW/HTS_TST_POS_fac,TRUE ~ 0),
         percentTPTinit = case_when(TX_NEW > 0 ~TPT_NEW/TX_NEW, TRUE ~ 0),
         yield_index = case_when(Index_contacts_tested > 0 ~ Index_contacts_pos/Index_contacts_tested, TRUE ~ 0)) %>% 
  ungroup() %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  mutate(new_indicator = indicator) %>% 
  select(-indicator) %>% 
  spread(new_indicator, val) %>% 
  select(-c(FundingAgency:Community)) %>% 
  arrange(Facility, mon_yr)

# NON-CALCULATION INDICATORS----------------------------------------------------------

rem_ind<-base_df %>% 
  filter(indicator %ni% c(numden_ind, snapshot_ind)) %>% 
  spread(indicator, val) %>% 
  #select(-c(FundingAgency:Community)) %>% 
  arrange(Facility, mon_yr)

rem_ind1<-rem_ind %>% 
  select(-NET_NEW_proxy)

# SNAPSHOT DATA CALC -------------------------------------------------------------------

snapshot_df<-base_df %>% 
  filter(indicator %in% snapshot_ind) %>% 
  spread(indicator, val) %>% 
  arrange(Facility, mon_yr)


##### SUBSET HISTORICAL TX_CURR_28 'TX_CURR_28_proxy'

 
 ###### CALCULATE NET_NEW_proxy FOR NEW MONTHS
nnproxy<-snapshot_df %>%
  select(-c("Latemissed", "TX_CURR_90")) %>% 
  filter(!is.na(TX_CURR_28_proxy)) %>% 
  arrange(Facility, Week_End, mon_yr)%>% 
  filter(mon_yr > "2020-04") %>% 
  mutate(NET_NEW_proxy = TX_CURR_28_proxy - lag(TX_CURR_28_proxy, default=0, order_by = Facility)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  spread(indicator, val) %>%
  arrange(Facility, mon_yr) %>%   
  select(-c("TX_CURR_28_proxy")) %>% 
  filter(mon_yr > "2020-06")

# CREATE SNAPSHOT DATASET -------------------------------------------------------------------  
  tx<- rem_ind %>% 
    select(1:12, 21) %>% 
    filter(mon_yr < "2020-10")

tx<-bind_rows(tx,nnproxy) %>% 
  arrange(Facility, mon_yr)

# CREATE FINAL DATASET -------------------------------------------------------------------
merge_df<-left_join(rem_ind1,numden_df) %>% 
  arrange(Facility, mon_yr)

merge_tx<-left_join(snapshot_df, tx) %>% 
  arrange(Facility, mon_yr)

merge_df1<-left_join(merge_tx, merge_df) %>% 
  arrange(Facility, mon_yr)


merge_df2<-merge_df1 %>% 
  mutate(Week_Start = as.POSIXct(Week_Start),
         Week_End = as.POSIXct(Week_End),
         Siyenza_StartDate = as.POSIXct(Siyenza_StartDate),
         Siyenza_EndDate = as.POSIXct(Siyenza_EndDate)) %>% 
  arrange(Facility, mon_yr) %>% 
  mutate(Year = format(Week_End, "%Y"),
         mon = format(Week_End, "%m")) 
         
merge_df3<- merge_df2 %>% 
   mutate(Quarter = case_when(mon %in% c("10", "11", "12") ~ "Q1",
                              mon %in% c("01", "02", "03") ~ "Q2",
                              mon %in% c("04", "05", "06") ~ "Q3",
                              TRUE ~ "Q4")) 
 
merge_df3$nyear<- as.Date(merge_df3$Year, "%Y")


merge_df3<- merge_df3 %>%
mutate(nyear = case_when(mon %in% c("10", "11", "12") ~ nyear %m+% years(1), 
                            TRUE ~ nyear)) %>%  # YOU WILL NEED TO RERUN LINES 140 ONWARDS IF You MAKE A CHANGE HERE
  mutate(nyear = format(nyear, "%Y"),
         nyear = paste("FY", nyear, sep=" ")) %>% 
  select (-c(Year, mon)) %>% 
  rename(Year = nyear) %>% 
  mutate(DistrictType = case_when(PSNU %in% c("gp City of Johannesburg Metropolitan Municipality",
                                               "gp City of Tshwane Metropolitan Municipality",
                                               "kz eThekwini Metropolitan Municipality",
                                               "gp Ekurhuleni Metropolitan Municipality",
                                               "wc City of Cape Town Metropolitan Municipality") ~ "Metro",
                                   TRUE ~ "Non-Metro"),
         week_count = as.double(difftime(lubridate::ymd(Week_End),
                                        lubridate::ymd(Week_Start),
                                        units = "weeks")),
         PrimePartner = case_when(str_detect(PrimePartner, "Wits|WITS") ~ paste(PrimePartner, FundingAgency, sep="_"),
                                  TRUE ~ PrimePartner)) %>% 
  mutate(week_count = round(week_count),
         tested_fac = case_when(tested_fac=="Inf" ~0,
                                TRUE ~ tested_fac)) 
# EXPORT FINAL DATASET TO OUTPUTS FOLDER ------------------------------------------------------------------

write_tsv(merge_df3, file.path(here("outputs"),filename1), na = "")

# EXPORT FINAL DATASET TO HISTORICAL FOLDER ------------------------------------------------------------------

hist_df<-merge_df3 

write_tsv(hist_df, file.path(here("Data/Last Month"),filename2),na="")

# CLEAR GLOBAL ENVIRONMENT --------------------------------------------------------------
rm(list=ls())



