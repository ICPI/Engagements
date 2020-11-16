# TOPIC: PHUTHUMA R Script - Roll Up Historic Weekly Data to Monthly
# Developed by: Yaa Obeng & Gina Sarfaty


#load packages ----------------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(here)



#GLOBALS -----------------------------------------------------------------------------
raw<-here("Data/Historical")


# RAW DATA IN ------------------------------------------------------------------------

raw_files<-list.files(raw,pattern=".xlsx")


raw_df<-here("Data/Historical",raw_files) %>% 
  map(~ read_excel(.x, sheet = which(str_detect(excel_sheets(.x), "RAW|Siyenza"))))%>%
  reduce(bind_rows)


# ROLL UP ----------------------------------------------------------------------------

df_base<-raw_df %>% 
  mutate(mon_yr= format(Week_End, "%Y-%m"),
         MechanismID=as.character(MechanismID)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric))))
                                

df_snapshot<-df_base%>%
  filter(indicator %in% c("TX_CURR_28", "EARLYMISSED", "LATEMISSED"), !is.na(val)) %>% 
  group_by(mon_yr,Facility,MechanismID) %>% 
  filter(Week_End==max(Week_End)) %>% 
  ungroup() %>% 
  select(-c(Week_Start,Week_End))


df_cumulative<-df_base %>% 
  filter(!is.na(val),
         !indicator %in% c("TX_CURR_28", "EARLYMISSED", "LATEMISSED")) %>% 
  select(-c(Week_Start,Week_End)) %>% 
  group_by(FundingAgency,PrimePartner,MechanismID,SNU1,PSNU,Community,Facility,
           Siyenza_StartDate,Siyenza_EndDate,mon_yr,indicator) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE)
  

final<-bind_rows(df_snapshot,df_cumulative) %>% 
  spread(indicator,val) %>% 
  select(-c(HTS_TST_POS_IP,TARG_WKLY_NETNEW,TX_CURR_TLD,TX_NEW_IP,TX_NEW_TLD)) %>% 
  rename(HTS_TST_POS_fac=HTS_TST_POS,
         TPT_NEW=`TPT initiated`,
         Earlymissed=EARLYMISSED,
         LateMissed=LATEMISSED)


# CREATE MONTHLY WEEK START & END VARIABLES FOR HISTORICAL DATA --------------------------
dateref<- df_base %>%
  group_by(mon_yr, Facility, MechanismID) %>% 
  mutate(Week_Start = min(Week_Start),
         Week_End = max(Week_End)) %>% 
  ungroup() %>% 
  select(c(Facility, MechanismID, Week_Start, Week_End, mon_yr)) %>% 
  distinct(Facility, MechanismID, Week_Start, Week_End, mon_yr)


# MERGE HISTORICAL DATA WITH WEEK START & END VARIABLES -----------------------------------
final<-left_join(final, dateref)  


# EXPORT -----------------------------------------------------------------------------

filename<-paste("historic_siyenza", "_", Sys.Date(), ".txt", sep="")

write_tsv(final, file.path(here("Data/Historical"),filename,na=""))

