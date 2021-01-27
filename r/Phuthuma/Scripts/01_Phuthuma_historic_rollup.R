# TOPIC: PHUTHUMA R Script - Roll Up Historic Weekly Data to Monthly
# Developed by: Yaa Obeng & Gina Sarfaty


#load packages ----------------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(here)
library(tibble)
library(lubridate)
library(splitstackshape)



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
         MechanismID=as.character(MechanismID)) 

df_base<-df_base %>% 
  mutate(MechanismID = case_when(PSNU == "ec Buffalo City Metropolitan Municipality" & mon_yr >"2019-12"~ "81902", TRUE ~ MechanismID),
         PrimePartner= case_when(PSNU == "ec Buffalo City Metropolitan Municipality" & mon_yr > "2019-12"~ "Maternal, Adolscent and Child Health (MatCH)",
                            TRUE ~ PrimePartner)) %>% 
  gather(indicator,val,colnames(select_if(., (is.numeric)))) %>% 
  arrange(Facility, mon_yr, indicator)


df_snapshot<-df_base%>%
  filter(indicator %in% c("EARLYMISSED","LATEMISSED"), !is.na(val)) %>% 
  spread(indicator,val) %>% 
  gather(indicator,val,EARLYMISSED:LATEMISSED) %>% 
  group_by(mon_yr,Facility,MechanismID) %>% 
  filter(Week_End==max(Week_End)) %>% 
  ungroup() %>% 
  select(-c(Week_Start,Week_End)) %>% 
  arrange(Facility, mon_yr)

df_snapshot1<-df_base%>%
  filter(indicator %in% c("TX_CURR_28"), !is.na(val)) %>% 
  spread(indicator,val) %>% 
  filter(!is.na(TX_CURR_28)) %>%
  gather(indicator,val,TX_CURR_28) %>% 
  group_by(mon_yr,Facility,MechanismID) %>% 
  filter(Week_End==max(Week_End)) %>% 
  ungroup() %>% 
  select(-c(Week_Start,Week_End)) %>% 
  arrange(Facility, mon_yr) %>% 
  filter(mon_yr<"2020-07")

df_snapshot <- bind_rows(df_snapshot, df_snapshot1)

df_netnew<- df_base %>% 
  filter(indicator %in% c("TX_CURR_28")) %>%
  filter(!is.na(val)) %>%  
  arrange(Facility, Week_End, mon_yr) %>%
  group_by(Facility)%>% 
  # mutate(net_new = (val - lag(val, default = 0, order_by = Facility)))
  mutate(net_new = case_when(Siyenza_StartDate == date("2019-08-01") & Week_End < date("2019-08-02") ~ 0,
                             Siyenza_StartDate == date("2019-08-01") & Week_End == date("2019-08-02") ~ 0,
                            Siyenza_StartDate == date("2019-03-01") & Week_End < date("2019-03-02") ~ 0,
                            Siyenza_StartDate == Week_End ~ 0,
                            TRUE ~ (val - lag(val, default = 0, order_by = Facility)))) %>% 
  gather(indicator, val, "net_new") %>% 
  select(-c(Week_Start,Week_End)) %>% 
  group_by(FundingAgency,PrimePartner,MechanismID,SNU1,PSNU,Community,Facility,
           Siyenza_StartDate,Siyenza_EndDate,mon_yr,indicator) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE) %>% 
  arrange(Facility, mon_yr) %>% 
  filter(mon_yr < "2020-07") 



df_cumulative<-df_base %>% 
  filter(!is.na(val),
         !indicator %in% c("TX_CURR_28", "EARLYMISSED", "LATEMISSED")) %>% 
  select(-c(Week_Start,Week_End)) %>% 
  group_by(FundingAgency,PrimePartner,MechanismID,SNU1,PSNU,Community,Facility,
           Siyenza_StartDate,Siyenza_EndDate,mon_yr,indicator) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE)


final<-bind_rows(df_snapshot,df_netnew, df_cumulative) %>% 
  spread(indicator,val) %>% 
  select(-c(HTS_TST_POS_IP,TX_CURR_TLD,TX_NEW_IP,TX_NEW_TLD)) %>% 
  rename(HTS_TST_POS_fac=HTS_TST_POS,
         TPT_NEW=`TPT initiated`,
         Earlymissed=EARLYMISSED,
         LateMissed=LATEMISSED,
         NET_NEW_proxy=net_new,
         TX_CURR_28_proxy=TX_CURR_28) %>%
  arrange(Facility, mon_yr) %>% 
  filter(mon_yr< "2020-10")


# CREATE MONTHLY WEEK START & END VARIABLES FOR HISTORICAL DATA --------------------------
date_df<- df_base %>%
  spread(indicator, val) %>% 
  select(c(Facility, MechanismID, Week_Start, Week_End, mon_yr, Siyenza_StartDate, Siyenza_EndDate)) %>% 
  arrange(Facility, mon_yr) %>%
  filter(Week_Start > Siyenza_StartDate) %>% 
  arrange(Facility, Week_Start, mon_yr) %>% 
  group_by(Facility, mon_yr) %>% 
  mutate(nWeek_Start = min(Week_Start),
         nWeek_End = max(Week_End)) %>%
  select(-c(Week_Start, Week_End)) %>% 
  distinct(Facility, MechanismID, nWeek_Start, nWeek_End, mon_yr, Siyenza_StartDate, Siyenza_EndDate)

date_ref1<-date_df %>%
  mutate(nWeek_End = date(nWeek_End)) %>% 
  mutate(nWeek_End = case_when( mon_yr == "2020-10" ~ date("2020-10-30"),
                                mon_yr == "2020-11" ~ date("2020-11-27"),
                                mon_yr == "2019-04" ~ date("2019-04-26"),
                                mon_yr == "2019-03" ~ date("2019-03-29"),
                                TRUE ~ nWeek_End))

  

# MERGE HISTORICAL DATA WITH WEEK START & END VARIABLES -----------------------------------
final1<-left_join(final, date_ref1) %>% 
filter(mon_yr < "2020-10", !is.na(nWeek_End)) %>%
  rename(Week_End = nWeek_End,
         Week_Start = nWeek_Start)
  
  


# EXPORT -----------------------------------------------------------------------------

filename<-paste("historic_siyenza", "_", Sys.Date(), ".txt", sep="")

write_tsv(final1, file.path(here("Data/Historical"),filename),na="")

write_tsv(date_ref1, file.path(here("Data/Historical"),"dateref_20210111.txt"),na="")

