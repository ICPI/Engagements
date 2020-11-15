# PHUTHUMA R Script for Dashboard Population 
# Developed by: Gina Safarty, Yaa Obeng 
paste(Sys.Date()) # Last Updated


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

raw_files<-list.files(raw,pattern=".xlsx")
old_file<-list.files(old, pattern="historic")

numden_ind<-c("HTS_TST_fac", "HTS_TST_POS_fac", "TX_NEW", "TPT_NEW","Index_contacts_tested",
              "Index_contacts_pos", "HTS_TST_com","HTS_TST_POS_com","Headcount")
snapshot_ind<-c("TX_CURR_28", "TX_CURR_90", "Latemissed")

filename1<-paste("new_InteragencyDash", "_", Sys.Date(), ".txt", sep="")



##################### New Data Processing ##########################

# MONTHLY RAW DATA & HISTORICAL IN ---------------------------------------------------------

raw_df<-here("Data",raw_files) %>% 
  map(~ read_xlsx(.x, sheet = which(str_detect(excel_sheets(.x), "Dummy Data"))))%>% #update sheet name when real is received
  reduce(bind_rows)

old_df<-read_tsv(here("Data/Historical", old_file), col_names = T,
                 col_types = c(
                   HTS_TST_POS_fac = "d",
                   TPT_NEW = "d",
                   TX_NEW = "d")) %>%
  mutate(MechanismID=as.character(MechanismID)) %>%
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

# YIELD, PROXY LINKAGE, TEST COVERAGE & TPT INITIATION DATA CALC ----------------------------

numden_df<-base_df %>% 
  filter(indicator %in% numden_ind) %>% 
    group_by(Facility, MechanismID, mon_yr) %>% 
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
    spread(new_indicator, val)
  
# NON-CALCULATION INDICATORS----------------------------------------------------------
  
rem_ind<-base_df %>% 
    filter(indicator %ni% c(numden_ind, snapshot_ind)) %>% 
    spread(indicator, val)

# SNAPSHOT DATA CALC -------------------------------------------------------------------
  
 snapshot_df<-base_df %>% 
    filter(indicator %in% snapshot_ind)

   ##### CALCULATE HISTORICAL NET_NEW  
 netnew28<- snapshot_df %>% 
     filter(indicator %in% c("TX_CURR_28")) %>%
    filter(!is.na(val)) %>%  
    arrange(Facility, mon_yr) %>% 
    mutate(net_new= (val - lag(val, default = 0, order_by = Facility))) %>% 
    gather(indicator, val, "net_new") %>% 
    filter(mon_yr < "2020-07") %>% 
    mutate(new_indicator= "NET_NEW_proxy") %>%
   select(-indicator) %>% 
    spread(new_indicator, val)
 
 ##### RENAME HISTORICAL TX_CURR_28 'TX_CURR_28_proxy'
 txcurr28<- snapshot_df %>% 
   filter(indicator %in% c("TX_CURR_28")) %>%
   mutate(new_indicator= "TX_CURR_28_proxy") %>% 
   select(-indicator) %>% 
   spread(new_indicator, val) %>% 
   mutate(Latemissed = 0,
          TX_CURR_90 = 0) 
   
 
 ##### COMBINE Historical TX_CURR_28 & NET_ NEW
 htx<-left_join(txcurr28, netnew28) %>% 
   select(1:12,14,16,13,15)
    
##### CALCULATE NEW TX_CURR_28_proxy   
txcurr28proxy<-snapshot_df %>% 
    filter(indicator %in% c("TX_CURR_90", "Latemissed")) %>% 
    spread(indicator, val) %>% 
    arrange(Facility, mon_yr) %>% 
    filter(FundingAgency == "HHS/CDC") %>% 
    mutate(TX_CURR_28_proxy = TX_CURR_90 - Latemissed)
   
 txcurr28proxy<-txcurr28proxy %>% 
    mutate(NET_NEW_proxy = TX_CURR_28_proxy - lag(TX_CURR_28_proxy, default=0, order_by = Facility)) %>% 
    gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
    mutate(new_indicator= indicator) %>% 
    filter(mon_yr > "2020-06") %>% 
   select(-indicator) %>% 
   spread(new_indicator, val)
   
# CREATE FINAL DATASET -------------------------------------------------------------------
 tx_df<-bind_rows(htx, txcurr28proxy)
 merge_df<-left_join(numden_df, tx_df) 
 merge_df<-left_join(merge_df, rem_ind) %>% 
   arrange(Facility, mon_yr) 
 
# EXPORT FINAL DATASET ------------------------------------------------------------------
 
write_tsv(merge_df, file.path(here("Dataout"),filename1), na = "")
 
# CLEAR GLOBAL ENVIRONMENT --------------------------------------------------------------
rm(list=ls())

 # INDICATOR Rename KEY ------------------------------------------------------------------- 
indref<-merged_df %>% #don't see merged_df, esp after global is cleared?
  select(c(Facility, MechanismID, mon_yr, indicator, new_indicator)) %>% 
  distinct(indicator, new_indicator)

 write_tsv(indref, file.path(here("Dataout"),"indicatorKey.txt",na=""))


