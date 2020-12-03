##########################################################################################################
###################### PHUTHUMA R Script for Dashboard Population ########################################
###################### Developed by: Gina Safarty, Yaa Obeng ###########################################
paste(Sys.Date()) # Last Updated: Dec. 2, 2020


#load packages
library(tidyverse)
library(here)
library(readxl)
library(tibble)
library(lubridate)
library(splitstackshape)



#GLOBALS -----------------------------------------------------------------------------
raw<-here("Data")
old<-here("Data/Historical")
'%ni%'<- Negate('%in%')
filename<-paste("historic_siyenza", "_", Sys.Date(), ".txt", sep="")
hist_files<-list.files(raw,pattern="Siyenza")
raw_files<-list.files(raw,pattern="raw")
old_files<-list.files(old, pattern="historic")
numden_ind<-c("HTS_TST_fac", "HTS_TST_POS_fac", "TX_NEW", "TPT_NEW","Index_contacts_tested",
              "Index_contacts_pos", "HTS_TST_com","HTS_TST_POS_com","Headcount")
snapshot_ind<-c("TX_CURR_28", "TX_CURR_90", "Latemissed")
filename1<-paste("new_InteragencyDash", "_", Sys.Date(), ".txt", sep="")

##################### New Data Processing ##########################

# MONTHLY RAW DATA & HISTORICAL IN ---------------------------------------------------------

raw_df<-here("Data",raw_files) %>% 
  map(~ read_xlsx(.x, sheet = which(str_detect(excel_sheets(.x), "RAW"))))%>%
  reduce(bind_rows)

old_df<-read_tsv("Data/Historical/historic_siyenza_2020-12-02.txt", col_names = T,
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
         MechanismID=as.character(MechanismID)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  select(c(1:9, 12,10,11,13,14)) %>% 
  filter(FundingAgency == "HHS/CDC") # Temporary mod b/c dummy data has only CDC DATA

# MERGE Historical and NEW MONTHLY DATA-----------------------------------------------------

base_df<-bind_rows(raw_base,old_df)

# CREATE IM Transition Data  -----------------------------------------------------
mech_ref<- base_df %>% 
  select(1:7,11:12,8:10) %>% 
  arrange(Facility, Week_End) %>% 
  distinct()
 

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
  select(-c(FundingAgency:Community))

# NON-CALCULATION INDICATORS----------------------------------------------------------

rem_ind<-base_df %>% 
  filter(indicator %ni% c(numden_ind, snapshot_ind)) %>% 
  spread(indicator, val) %>% 
  select(-c(FundingAgency:Community)) %>% 
  arrange(Facility, mon_yr)

# SNAPSHOT DATA CALC -------------------------------------------------------------------

snapshot_df<-base_df %>% 
  filter(indicator %in% snapshot_ind)

##### RENAME HISTORICAL TX_CURR_28 'TX_CURR_28_proxy'
txcurr28<- snapshot_df %>% 
  filter(indicator %in% c("TX_CURR_28", "Latemissed")) %>%
  spread(indicator, val) %>% 
  filter(mon_yr < "2020-07") %>% 
  rename(TX_CURR_28_proxy = TX_CURR_28) %>% 
  arrange(Facility, mon_yr) %>% 
  select(-c(FundingAgency:Community))

##### CALCULATE NEW TX_CURR_28_proxy   
txcurr28proxy<-snapshot_df %>% 
  filter(indicator %in% c("TX_CURR_90", "Latemissed")) %>% 
  spread(indicator, val) %>% 
  arrange(Facility, mon_yr) %>% 
  mutate(TX_CURR_28_proxy = TX_CURR_90 - Latemissed)

txcurr28proxy<-txcurr28proxy %>% 
  mutate(NET_NEW_proxy = TX_CURR_28_proxy - lag(TX_CURR_28_proxy, default=0, order_by = Facility)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  filter(mon_yr > "2020-06") %>% 
  spread(indicator, val) %>%
  select(-c(FundingAgency:Community)) %>% 
  arrange(Facility, mon_yr)  

# CREATE FINAL DATASET & Fix Data variables to UTC timezone -------------------------------------------------------------------
merge_df<-left_join(txcurr28,rem_ind) %>% 
  arrange(Facility, mon_yr)
merge_df<-full_join(merge_df, txcurr28proxy) %>% 
  arrange(Facility, mon_yr)
merge_df<-full_join(merge_df, numden_df) %>% 
  arrange(Facility, mon_yr)
merge_df<-left_join(merge_df,mech_ref) %>% 
  select(37:42, 1:6, 22:36, 7, 17,8,21,9:16,18:20)

merge_df<-merge_df %>% 
  mutate(Week_Start = as.POSIXct(Week_Start, tz="UTC"),
         Week_End = as.POSIXct(Week_End, tz="UTC"),
         Siyenza_StartDate = as.POSIXct(Siyenza_StartDate, tz="UTC"),
         Siyenza_EndDate = as.POSIXct(Siyenza_EndDate, tz="UTC"))

# EXPORT FINAL DATASET ------------------------------------------------------------------

write_tsv(merge_df, file.path(here("outputs"),filename1), na = "")

# INDICATOR Rename KEY ------------------------------------------------------------------- 
indref<-merge_df %>% 
   gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
   mutate(org_indicator = case_when(mon_yr < "2020-06" & indicator == "TX_CURR_28_proxy" ~ "TX_CURR_28",
                                     mon_yr < "2020-06" & indicator == "NET_NEW_proxy" ~ "NET_NEW",
                                    mon_yr < "2020-10" & indicator == "HTS_TST_POS_fac" ~ "HTS_TST_POS", TRUE~ indicator)) %>% 
  select(c(Facility, MechanismID, mon_yr, indicator, org_indicator)) %>% 
  distinct(indicator, org_indicator)
 
# CLEAR GLOBAL ENVIRONMENT --------------------------------------------------------------
rm(list=ls())



