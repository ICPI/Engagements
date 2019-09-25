memory.limit(size=1000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program for creating site metrics dataset
#   Programmer: Imran Mujawar
#   Date started: 06/26/2019
#   Date updated: 
#   Program: Creates dataset for site triage
#           
# ---------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pulling in the load package function R file
# Load function to install list of packages
ldpkg <- dget("ldpkg.R")

# Running the ldpkg function which checks if package is installed 
# and then loads it if it is, otherwise, installs and then loads
ldpkg(c("readr",
        "eply",
        "stringr",
        "tidyverse",
        "leaflet" , 
        "rgdal" , 
        "rgeos", 
        "sp", 
        "RColorBrewer",
        "readxl",
        "geojsonio"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%X"))
tm <- str_replace_all(t, "[: ]", "_")
dt <- str_replace_all(dx, "[ ]", "")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}


# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in required datasets ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Identifying Genie or MSD files in the RawData folder
filelist <- list.files(pattern="Genie|genie|MER")

gfile_loc <- filelist[3] 

# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file=gfile_loc, 
                col_names = TRUE, n_max = 0)   

foonames <- tolower(names(foo))


# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("qtr|targets|cumulative", foonames), "d", "c"))

# colvecx <- as.vector(ifelse(foonames %in% c("FILL IN STUFF HERE"  ), "d", "c"))

colvec <- paste(colvecx, collapse = '')


# Pulling the dataset with the correct column types
datim <- read_tsv(file=gfile_loc, 
                  col_names = TRUE,
                  col_types = colvec)

names(datim) <- tolower(names(datim))  


# Getting site lat long dat
latlng <- read.csv("CoteDIvoire_Facilities_11142018.csv", header = T) %>% 
  select(uid, Longitude, Latitude) %>% 
  filter(!is.na(Longitude)) %>% 
  mutate(orgunituid = as.character(uid))

shppath <- "C:/Temp_work/CDI_clustering/CIV_clustering_proj_20180511/"

civ_district <- readOGR(dsn=paste(shppath, 'CIV_DATIM', sep=""), 
                        layer = 'CotedIvoireHealthDistrictsLsib2016Dec')
civ_region <- readOGR(dsn=paste(shppath, 'SANTE_2012', sep=""), layer = 'REGION_SANITAIRE')
# civ.district <- read_sf('CIV_DATIM/CotedIvoireHealthDistrictsLsib2016Dec.shp')

# check CRS projection for both 
proj4string(civ_region)
proj4string(civ_district)

# change CRS to 
civ_region <- spTransform(civ_region,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  


# creating variables requested to triage sites
df <- datim %>% 
  # Get only total numerator and indicators of interest
  filter(indicator %in% c("HTS_TST", 
                          "HTS_TST_POS", 
                          "TX_CURR", 
                          "TX_NEW",
                          "TX_PVLS") &
           standardizeddisaggregate %in% c("Total Numerator",
                                           "Total Denominator") &
           fiscal_year %in% c("2019","2018") &
           fundingagency %in% c("HHS/CDC")) %>% 
  gather(period, valu, 
         targets,   
         qtr1,          
         qtr2,          
         qtr3,          
         qtr4,          
         cumulative) %>%
  mutate(periodx = str_replace_all(period, "tr", ""),
         periodx = str_replace_all(periodx, "ulative", "")) %>% 
  mutate(colvars = paste(indicator, "_", numeratordenom, "_", fiscal_year,
                         if_else(periodx %in% c("targets"), "_", ""),
                         periodx, sep = "")) %>% 
  select(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         colvars, valu) %>% 
  filter(colvars %in% c("HTS_TST_N_2018cum", 
                        "HTS_TST_N_2019_targets", 
                        "HTS_TST_N_2019q1", 
                        "HTS_TST_N_2019q2", 
                        "HTS_TST_N_2019q3", 
                        "HTS_TST_N_2019cum", 
                        "HTS_TST_POS_N_2018cum", 
                        "HTS_TST_POS_N_2019_targets", 
                        "HTS_TST_POS_N_2019q1", 
                        "HTS_TST_POS_N_2019q2", 
                        "HTS_TST_POS_N_2019q3", 
                        "HTS_TST_POS_N_2019cum", 
                        "TX_CURR_N_2018cum", 
                        "TX_CURR_N_2019_targets", 
                        "TX_CURR_N_2019q1", 
                        "TX_CURR_N_2019q2",
                        "TX_CURR_N_2019q3",
                        "TX_CURR_N_2019cum",
                        "TX_NEW_N_2018cum", 
                        "TX_NEW_N_2019_targets", 
                        "TX_NEW_N_2019q1", 
                        "TX_NEW_N_2019q2", 
                        "TX_NEW_N_2019q3",
                        "TX_NEW_N_2019cum", 
                        "TX_PVLS_D_2018cum", 
                        "TX_PVLS_D_2019_targets", 
                        "TX_PVLS_D_2019q1", 
                        "TX_PVLS_D_2019q2", 
                        "TX_PVLS_D_2019q3",
                        "TX_PVLS_D_2019cum",
                        "TX_PVLS_N_2018cum", 
                        "TX_PVLS_N_2019_targets", 
                        "TX_PVLS_N_2019q1", 
                        "TX_PVLS_N_2019q2",
                        "TX_PVLS_N_2019q3",
                        "TX_PVLS_N_2019cum"
  )) %>% 
  group_by(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         colvars) %>% 
  summarize(val = sum(valu, na.rm=T)) %>%
  ungroup() %>% 
  filter(!is.na(val)) %>% 
  spread(colvars, val)


xlpath <- "Site_List_revised_targets_from_Nicoue.xlsx"

xlcols <- c(rep("text",12), rep("numeric",16))

n_max = 2000

# Pulling in updated targets and priority site lists
cop18 <- read_xlsx(path=xlpath, sheet = 1, col_names = TRUE, 
                   col_types = xlcols,
                   na = "", trim_ws = TRUE, skip = 1,
          n_max = 2000, guess_max = min(1000, n_max),
          progress = readxl_progress(), .name_repair = "unique")

# Pulling in updated targets and priority site lists
cop19 <- read_xlsx(path=xlpath, sheet = 2, col_names = TRUE, 
                   col_types = xlcols,
                   na = "", trim_ws = TRUE, skip = 1,
                   n_max = 2000, guess_max = min(1000, n_max),
                   progress = readxl_progress(), .name_repair = "unique")

# Pulling in updated targets and priority site lists
prsites <- read_xlsx(path=xlpath, sheet = 3, col_names = TRUE, 
                   col_types = xlcols,
                   na = "", trim_ws = TRUE, skip = 1,
                   n_max = 2000, guess_max = min(1000, n_max),
                   progress = readxl_progress(), .name_repair = "unique")


# Creating the dataset to merge updated tx_curr targets onto the dataset
revtx <- cop18 %>% 
  select(orgUnitUID, "Revised COP18 Target") %>% 
  rename(orgunituid=orgUnitUID,
         rev_tx_curr_target="Revised COP18 Target") %>% 
  # select_if(is.character)
  group_by(orgunituid) %>% 
  summarise(rev_tx_curr_target=sum(rev_tx_curr_target,na.rm=T)) %>% 
  ungroup()

# Creating dataset with priority sites
sitelist <- prsites %>% 
  select(orgUnitUID) %>% 
  rename(orgunituid=orgUnitUID) %>% 
  mutate(priority_site = "Y") %>% 
  unique()
  
# Mering updated targets
df_rev <- left_join(df, revtx)
# merging the priority sites variable onto dataset
df_revx <- left_join(df_rev, sitelist)


df1 <- df_revx %>% 
  rowwise() %>% 
  mutate(NET_NEW_gap_2019cum = if_else(!is.na(rev_tx_curr_target) & rev_tx_curr_target>0, 
                                  sum(rev_tx_curr_target, -TX_CURR_N_2019cum, na.rm=T), NA_real_)) %>%
  mutate(NET_NEW_2019q1 = sum(TX_CURR_N_2019q1, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(NET_NEW_2019q2 = sum(TX_CURR_N_2019q2, -TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(NET_NEW_2019q3 = sum(TX_CURR_N_2019q3, -TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(NET_NEW_2019cum = sum(TX_CURR_N_2019cum, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(gain_loss_2019q1 = sum(NET_NEW_2019q1, -TX_NEW_N_2019q1)) %>% 
  mutate(gain_loss_2019q2 = sum(NET_NEW_2019q2, -TX_NEW_N_2019q2)) %>% 
  mutate(gain_loss_2019q3 = sum(NET_NEW_2019q3, -TX_NEW_N_2019q3)) %>% 
  mutate(gain_loss_2019cum = sum(NET_NEW_2019cum, -TX_NEW_N_2019cum)) %>% 
  mutate(percent_gainloss_2019q1 = (TX_CURR_N_2019q1/
                                       sum(TX_CURR_N_2018cum,TX_NEW_N_2019q1, 
                                           na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q2 = (TX_CURR_N_2019q2/
                                      sum(TX_CURR_N_2019q1,TX_NEW_N_2019q2, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q3 = (TX_CURR_N_2019q3/
                                      sum(TX_CURR_N_2019q2,TX_NEW_N_2019q3, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019cum = (TX_CURR_N_2019cum/
                                   sum(TX_CURR_N_2018cum,TX_NEW_N_2019cum, 
                                       na.rm = T))-1) %>% 
  ungroup() %>% 
  mutate(HTS_POS_target_achv_2019cum = if_else(!is.na(HTS_TST_POS_N_2019_targets) & 
                                         HTS_TST_POS_N_2019_targets>0, 
                                       HTS_TST_POS_N_2019cum/
                                         HTS_TST_POS_N_2019_targets, NA_real_)) %>%
  mutate(TX_CURR_percent_2019cum = TX_CURR_N_2019cum/sum(TX_CURR_N_2019cum, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q1 = TX_CURR_N_2019q1/sum(TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q2 = TX_CURR_N_2019q2/sum(TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q3 = TX_CURR_N_2019q3/sum(TX_CURR_N_2019q3, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019cum = TX_NEW_N_2019cum/sum(TX_NEW_N_2019cum, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q1 =  TX_NEW_N_2019q1/ sum(TX_NEW_N_2019q1, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q2 =  TX_NEW_N_2019q2/ sum(TX_NEW_N_2019q2, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q3 =  TX_NEW_N_2019q3/ sum(TX_NEW_N_2019q3, na.rm=T)) %>% 
  mutate(NET_NEW_gap_percent_2019cum = NET_NEW_gap_2019cum/sum(NET_NEW_gap_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019cum = HTS_TST_POS_N_2019cum/sum(HTS_TST_POS_N_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q1  = HTS_TST_POS_N_2019q1 /sum(HTS_TST_POS_N_2019q1 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q2  = HTS_TST_POS_N_2019q2 /sum(HTS_TST_POS_N_2019q2 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q3  = HTS_TST_POS_N_2019q3 /sum(HTS_TST_POS_N_2019q3 , na.rm=T)) %>% 
  mutate(TX_PVLS_suppr_2019cum = if_else(!is.na(TX_PVLS_D_2019cum) & 
                                        TX_PVLS_D_2019cum>0, 
                                 TX_PVLS_N_2019cum/TX_PVLS_D_2019cum, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q1 = if_else(!is.na(TX_PVLS_D_2019q1) & 
                                           TX_PVLS_D_2019q1>0, 
                                         TX_PVLS_N_2019q1/TX_PVLS_D_2019q1, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q2 = if_else(!is.na(TX_PVLS_D_2019q2) & 
                                           TX_PVLS_D_2019q2>0, 
                                         TX_PVLS_N_2019q2/TX_PVLS_D_2019q2, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q3 = if_else(!is.na(TX_PVLS_D_2019q3) & 
                                          TX_PVLS_D_2019q3>0, 
                                        TX_PVLS_N_2019q3/TX_PVLS_D_2019q3, NA_real_)) %>% 
  mutate(yield_2019q1 = if_else(!is.na(HTS_TST_N_2019q1) & 
                                   HTS_TST_N_2019q1>0, 
                                 HTS_TST_POS_N_2019q1/HTS_TST_N_2019q1, NA_real_)) %>% 
  mutate(yield_2019q2 = if_else(!is.na(HTS_TST_N_2019q2) & 
                                   HTS_TST_N_2019q2>0, 
                                 HTS_TST_POS_N_2019q2/HTS_TST_N_2019q2, NA_real_)) %>% 
  mutate(yield_2019q3 = if_else(!is.na(HTS_TST_N_2019q3) & 
                                  HTS_TST_N_2019q3>0, 
                                HTS_TST_POS_N_2019q3/HTS_TST_N_2019q3, NA_real_)) %>% 
  mutate(yield_2019cum = if_else(!is.na(HTS_TST_N_2019cum) & 
                                   HTS_TST_N_2019cum>0, 
                                 HTS_TST_POS_N_2019cum/HTS_TST_N_2019cum, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q1 = if_else(!is.na(TX_NEW_N_2019q1) & 
                                          TX_NEW_N_2019q1>0, 
                                        NET_NEW_2019q1/TX_NEW_N_2019q1, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q2 = if_else(!is.na(TX_NEW_N_2019q2) & 
                                          TX_NEW_N_2019q2>0, 
                                        NET_NEW_2019q2/TX_NEW_N_2019q2, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q3 = if_else(!is.na(TX_NEW_N_2019q3) & 
                                          TX_NEW_N_2019q3>0, 
                                        NET_NEW_2019q3/TX_NEW_N_2019q3, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019cum = if_else(!is.na(TX_NEW_N_2019cum) & 
                                          TX_NEW_N_2019cum>0, 
                                        NET_NEW_2019cum/TX_NEW_N_2019cum, NA_real_)) 


  
namevec <- sort(names(df1)[c(7:length(df1))])
  

df_finalx <- df1 %>% 
  select(
    orgunituid:snuprioritization,
    priority_site,

    HTS_TST_N_2018cum,          
    HTS_TST_N_2019_targets,     
    HTS_TST_N_2019cum,          
    HTS_TST_N_2019q1,           
    HTS_TST_N_2019q2,           
    HTS_TST_N_2019q3,           
    
    HTS_TST_POS_N_2018cum,      
    HTS_TST_POS_N_2019_targets, 
    HTS_TST_POS_N_2019cum,      
    HTS_TST_POS_N_2019q1,       
    HTS_TST_POS_N_2019q2, 
    HTS_TST_POS_N_2019q3, 
    
    HTS_POS_percent_2019cum,    
    HTS_POS_percent_2019q1,     
    HTS_POS_percent_2019q2,     
    HTS_POS_percent_2019q3,     
    HTS_POS_target_achv_2019cum,

    yield_2019cum,              
    yield_2019q1,               
    yield_2019q2,         
    yield_2019q3,         
    
    TX_CURR_N_2018cum,          
    TX_CURR_N_2019_targets,  
    rev_tx_curr_target,         
   
    TX_CURR_N_2019cum,          
    TX_CURR_N_2019q1,           
    TX_CURR_N_2019q2,           
    TX_CURR_N_2019q3,           
    TX_CURR_percent_2019cum,    
    TX_CURR_percent_2019q1,     
    TX_CURR_percent_2019q2,     
    TX_CURR_percent_2019q3,     
    NET_NEW_2019cum,            
    NET_NEW_2019q1,             
    NET_NEW_2019q2,             
    NET_NEW_2019q3,             
    NET_NEW_gap_2019cum,        
    NET_NEW_gap_percent_2019cum,
    gain_loss_2019cum,          
    gain_loss_2019q1,           
    gain_loss_2019q2,           
    gain_loss_2019q3,           
    percent_gainloss_2019cum,   
    percent_gainloss_2019q1,    
    percent_gainloss_2019q2,   
    percent_gainloss_2019q3,   
    
    OGAC_prox_ret_2019q1 ,
    OGAC_prox_ret_2019q2 ,
    OGAC_prox_ret_2019q3 ,
    OGAC_prox_ret_2019cum,

    TX_NEW_N_2018cum,           
    TX_NEW_N_2019_targets,      
    TX_NEW_N_2019cum,           
    TX_NEW_N_2019q1,            
    TX_NEW_N_2019q2,            
    TX_NEW_N_2019q3,            
    TX_NEW_percent_2019cum,     
    TX_NEW_percent_2019q1,      
    TX_NEW_percent_2019q2,      
    TX_NEW_percent_2019q3,      
    TX_PVLS_D_2018cum,          
    TX_PVLS_D_2019_targets,     
    TX_PVLS_D_2019cum,          
    TX_PVLS_D_2019q1,           
    TX_PVLS_D_2019q2,           
    TX_PVLS_D_2019q3,           
    TX_PVLS_N_2018cum,          
    TX_PVLS_N_2019_targets,     
    TX_PVLS_N_2019cum,          
    TX_PVLS_N_2019q1,           
    TX_PVLS_N_2019q2,           
    TX_PVLS_N_2019q3,           
    TX_PVLS_suppr_2019cum,      
    TX_PVLS_suppr_2019q1,       
    TX_PVLS_suppr_2019q2,       
    TX_PVLS_suppr_2019q3       
  )
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~ Coarse Age bands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# creating variables requested to triage sites
cdf <- datim %>% 
  # Get only total numerator and indicators of interest
  filter(indicator %in% c("HTS_TST", 
                          "HTS_TST_POS", 
                          "TX_CURR", 
                          "TX_NEW",
                          "TX_PVLS") &
           trendscoarse %in% c("<15","15+", "Unknown Age") &
           fiscal_year %in% c("2019","2018") &
           fundingagency %in% c("HHS/CDC")) %>% 
  gather(period, valu, 
         targets,   
         qtr1,          
         qtr2,          
         qtr3,          
         qtr4,          
         cumulative) %>%
  mutate(periodx = str_replace_all(period, "tr", ""),
         periodx = str_replace_all(periodx, "ulative", "")) %>% 
  mutate(colvars = paste(indicator, "_", numeratordenom, "_", fiscal_year,
                         if_else(periodx %in% c("targets"), "_", ""),
                         periodx, sep = "")) %>% 
  select(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         trendscoarse, sex,
         colvars, valu) %>% 
  filter(colvars %in% c("HTS_TST_N_2018cum", 
                        "HTS_TST_N_2019_targets", 
                        "HTS_TST_N_2019q1", 
                        "HTS_TST_N_2019q2", 
                        "HTS_TST_N_2019q3", 
                        "HTS_TST_N_2019cum", 
                        "HTS_TST_POS_N_2018cum", 
                        "HTS_TST_POS_N_2019_targets", 
                        "HTS_TST_POS_N_2019q1", 
                        "HTS_TST_POS_N_2019q2", 
                        "HTS_TST_POS_N_2019q3", 
                        "HTS_TST_POS_N_2019cum", 
                        "TX_CURR_N_2018cum", 
                        "TX_CURR_N_2019_targets", 
                        "TX_CURR_N_2019q1", 
                        "TX_CURR_N_2019q2",
                        "TX_CURR_N_2019q3",
                        "TX_CURR_N_2019cum",
                        "TX_NEW_N_2018cum", 
                        "TX_NEW_N_2019_targets", 
                        "TX_NEW_N_2019q1", 
                        "TX_NEW_N_2019q2", 
                        "TX_NEW_N_2019q3",
                        "TX_NEW_N_2019cum", 
                        "TX_PVLS_D_2018cum", 
                        "TX_PVLS_D_2019_targets", 
                        "TX_PVLS_D_2019q1", 
                        "TX_PVLS_D_2019q2", 
                        "TX_PVLS_D_2019q3",
                        "TX_PVLS_D_2019cum",
                        "TX_PVLS_N_2018cum", 
                        "TX_PVLS_N_2019_targets", 
                        "TX_PVLS_N_2019q1", 
                        "TX_PVLS_N_2019q2",
                        "TX_PVLS_N_2019q3",
                        "TX_PVLS_N_2019cum"
  )) %>% 
  group_by(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         trendscoarse, sex,
         colvars) %>% 
  summarize(val = sum(valu, na.rm=T)) %>%
  ungroup() %>% 
  filter(!is.na(val)) %>% 
  spread(colvars, val)



# Creating dataset with priority sites
sitelist <- prsites %>% 
  select(orgUnitUID) %>% 
  rename(orgunituid=orgUnitUID) %>% 
  mutate(priority_site = "Y") %>% 
  unique()
  
# Merging updated targets
# merging the priority sites variable onto dataset
cdf_revx <- left_join(cdf, sitelist)


cdf1 <- cdf_revx %>% 
  rowwise() %>% 
  mutate(NET_NEW_gap_2019cum = if_else(!is.na(TX_CURR_N_2019_targets) & TX_CURR_N_2019_targets>0, 
                                  sum(TX_CURR_N_2019_targets, -TX_CURR_N_2019cum, na.rm=T), NA_real_)) %>%
  mutate(NET_NEW_2019q1 = sum(TX_CURR_N_2019q1, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(NET_NEW_2019q2 = sum(TX_CURR_N_2019q2, -TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(NET_NEW_2019q3 = sum(TX_CURR_N_2019q3, -TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(NET_NEW_2019cum = sum(TX_CURR_N_2019cum, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(gain_loss_2019q1 = sum(NET_NEW_2019q1, -TX_NEW_N_2019q1)) %>% 
  mutate(gain_loss_2019q2 = sum(NET_NEW_2019q2, -TX_NEW_N_2019q2)) %>% 
  mutate(gain_loss_2019q3 = sum(NET_NEW_2019q3, -TX_NEW_N_2019q3)) %>% 
  mutate(gain_loss_2019cum = sum(NET_NEW_2019cum, -TX_NEW_N_2019cum)) %>% 
  mutate(percent_gainloss_2019q1 = (TX_CURR_N_2019q1/
                                       sum(TX_CURR_N_2018cum,TX_NEW_N_2019q1, 
                                           na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q2 = (TX_CURR_N_2019q2/
                                      sum(TX_CURR_N_2019q1,TX_NEW_N_2019q2, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q3 = (TX_CURR_N_2019q3/
                                      sum(TX_CURR_N_2019q2,TX_NEW_N_2019q3, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019cum = (TX_CURR_N_2019cum/
                                   sum(TX_CURR_N_2018cum,TX_NEW_N_2019cum, 
                                       na.rm = T))-1) %>% 
  ungroup() %>% 
  mutate(HTS_POS_target_achv_2019cum = if_else(!is.na(HTS_TST_POS_N_2019_targets) & 
                                         HTS_TST_POS_N_2019_targets>0, 
                                       HTS_TST_POS_N_2019cum/
                                         HTS_TST_POS_N_2019_targets, NA_real_)) %>%
  mutate(TX_CURR_percent_2019cum = TX_CURR_N_2019cum/sum(TX_CURR_N_2019cum, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q1 = TX_CURR_N_2019q1/sum(TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q2 = TX_CURR_N_2019q2/sum(TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q3 = TX_CURR_N_2019q3/sum(TX_CURR_N_2019q3, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019cum = TX_NEW_N_2019cum/sum(TX_NEW_N_2019cum, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q1 =  TX_NEW_N_2019q1/ sum(TX_NEW_N_2019q1, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q2 =  TX_NEW_N_2019q2/ sum(TX_NEW_N_2019q2, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q3 =  TX_NEW_N_2019q3/ sum(TX_NEW_N_2019q3, na.rm=T)) %>% 
  mutate(NET_NEW_gap_percent_2019cum = NET_NEW_gap_2019cum/sum(NET_NEW_gap_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019cum = HTS_TST_POS_N_2019cum/sum(HTS_TST_POS_N_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q1  = HTS_TST_POS_N_2019q1 /sum(HTS_TST_POS_N_2019q1 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q2  = HTS_TST_POS_N_2019q2 /sum(HTS_TST_POS_N_2019q2 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q3  = HTS_TST_POS_N_2019q3 /sum(HTS_TST_POS_N_2019q3 , na.rm=T)) %>% 
  mutate(TX_PVLS_suppr_2019cum = if_else(!is.na(TX_PVLS_D_2019cum) & 
                                        TX_PVLS_D_2019cum>0, 
                                 TX_PVLS_N_2019cum/TX_PVLS_D_2019cum, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q1 = if_else(!is.na(TX_PVLS_D_2019q1) & 
                                           TX_PVLS_D_2019q1>0, 
                                         TX_PVLS_N_2019q1/TX_PVLS_D_2019q1, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q2 = if_else(!is.na(TX_PVLS_D_2019q2) & 
                                           TX_PVLS_D_2019q2>0, 
                                         TX_PVLS_N_2019q2/TX_PVLS_D_2019q2, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q3 = if_else(!is.na(TX_PVLS_D_2019q3) & 
                                          TX_PVLS_D_2019q3>0, 
                                        TX_PVLS_N_2019q3/TX_PVLS_D_2019q3, NA_real_)) %>% 
  mutate(yield_2019q1 = if_else(!is.na(HTS_TST_N_2019q1) & 
                                   HTS_TST_N_2019q1>0, 
                                 HTS_TST_POS_N_2019q1/HTS_TST_N_2019q1, NA_real_)) %>% 
  mutate(yield_2019q2 = if_else(!is.na(HTS_TST_N_2019q2) & 
                                   HTS_TST_N_2019q2>0, 
                                 HTS_TST_POS_N_2019q2/HTS_TST_N_2019q2, NA_real_)) %>% 
  mutate(yield_2019q3 = if_else(!is.na(HTS_TST_N_2019q3) & 
                                  HTS_TST_N_2019q3>0, 
                                HTS_TST_POS_N_2019q3/HTS_TST_N_2019q3, NA_real_)) %>% 
  mutate(yield_2019cum = if_else(!is.na(HTS_TST_N_2019cum) & 
                                   HTS_TST_N_2019cum>0, 
                                 HTS_TST_POS_N_2019cum/HTS_TST_N_2019cum, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q1 = if_else(!is.na(TX_NEW_N_2019q1) & 
                                          TX_NEW_N_2019q1>0, 
                                        NET_NEW_2019q1/TX_NEW_N_2019q1, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q2 = if_else(!is.na(TX_NEW_N_2019q2) & 
                                          TX_NEW_N_2019q2>0, 
                                        NET_NEW_2019q2/TX_NEW_N_2019q2, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q3 = if_else(!is.na(TX_NEW_N_2019q3) & 
                                          TX_NEW_N_2019q3>0, 
                                        NET_NEW_2019q3/TX_NEW_N_2019q3, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019cum = if_else(!is.na(TX_NEW_N_2019cum) & 
                                          TX_NEW_N_2019cum>0, 
                                        NET_NEW_2019cum/TX_NEW_N_2019cum, NA_real_)) 


  
namevec <- sort(names(df1)[c(7:length(df1))])
  

cdf_finalx <- cdf1 %>% 
  select(
    orgunituid:snuprioritization,
    trendscoarse, sex,
    priority_site,

    HTS_TST_N_2018cum,          
    HTS_TST_N_2019_targets,     
    HTS_TST_N_2019cum,          
    HTS_TST_N_2019q1,           
    HTS_TST_N_2019q2,           
    HTS_TST_N_2019q3,           
    
    HTS_TST_POS_N_2018cum,      
    HTS_TST_POS_N_2019_targets, 
    HTS_TST_POS_N_2019cum,      
    HTS_TST_POS_N_2019q1,       
    HTS_TST_POS_N_2019q2, 
    HTS_TST_POS_N_2019q3, 
    
    HTS_POS_percent_2019cum,    
    HTS_POS_percent_2019q1,     
    HTS_POS_percent_2019q2,     
    HTS_POS_percent_2019q3,     
    HTS_POS_target_achv_2019cum,

    yield_2019cum,              
    yield_2019q1,               
    yield_2019q2,         
    yield_2019q3,         
    
    TX_CURR_N_2018cum,          
    TX_CURR_N_2019_targets,  

    TX_CURR_N_2019cum,          
    TX_CURR_N_2019q1,           
    TX_CURR_N_2019q2,           
    TX_CURR_N_2019q3,           
    TX_CURR_percent_2019cum,    
    TX_CURR_percent_2019q1,     
    TX_CURR_percent_2019q2,     
    TX_CURR_percent_2019q3,     
    NET_NEW_2019cum,            
    NET_NEW_2019q1,             
    NET_NEW_2019q2,             
    NET_NEW_2019q3,             
    NET_NEW_gap_2019cum,        
    NET_NEW_gap_percent_2019cum,
    gain_loss_2019cum,          
    gain_loss_2019q1,           
    gain_loss_2019q2,           
    gain_loss_2019q3,           
    percent_gainloss_2019cum,   
    percent_gainloss_2019q1,    
    percent_gainloss_2019q2,   
    percent_gainloss_2019q3,   
    
    OGAC_prox_ret_2019q1 ,
    OGAC_prox_ret_2019q2 ,
    OGAC_prox_ret_2019q3 ,
    OGAC_prox_ret_2019cum,

    TX_NEW_N_2018cum,           
    TX_NEW_N_2019_targets,      
    TX_NEW_N_2019cum,           
    TX_NEW_N_2019q1,            
    TX_NEW_N_2019q2,            
    TX_NEW_N_2019q3,            
    TX_NEW_percent_2019cum,     
    TX_NEW_percent_2019q1,      
    TX_NEW_percent_2019q2,      
    TX_NEW_percent_2019q3,      
    TX_PVLS_D_2018cum,          
    TX_PVLS_D_2019_targets,     
    TX_PVLS_D_2019cum,          
    TX_PVLS_D_2019q1,           
    TX_PVLS_D_2019q2,           
    TX_PVLS_D_2019q3,           
    TX_PVLS_N_2018cum,          
    TX_PVLS_N_2019_targets,     
    TX_PVLS_N_2019cum,          
    TX_PVLS_N_2019q1,           
    TX_PVLS_N_2019q2,           
    TX_PVLS_N_2019q3,           
    TX_PVLS_suppr_2019cum,      
    TX_PVLS_suppr_2019q1,       
    TX_PVLS_suppr_2019q2,       
    TX_PVLS_suppr_2019q3       
  )
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~ Fine Age bands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# creating variables requested to triage sites
fineage <- datim %>% 
  filter(indicator %in% c("HTS_TST", 
                          "HTS_TST_POS", 
                          "TX_CURR", 
                          "TX_NEW",
                          "TX_PVLS") &
           fiscal_year %in% c("2019","2018") &
           fundingagency %in% c("HHS/CDC")) 
  

fdf <- fineage %>% 
  # Get only total numerator and indicators of interest
  filter(trendssemifine %in% c("<01",            
                             "<05",          
                             "01-09",
                             "10-14",
                             "15-19"
                             ) ) %>% 
  mutate(semifine = if_else(
    trendssemifine %in% c("<01",            
                          "<05",          
                          "01-09"), "<09", trendssemifine)) %>% 
  gather(period, valu, 
         targets,   
         qtr1,          
         qtr2,          
         qtr3,          
         qtr4,          
         cumulative) %>%
  mutate(periodx = str_replace_all(period, "tr", ""),
         periodx = str_replace_all(periodx, "ulative", "")) %>% 
  mutate(colvars = paste(indicator, "_", numeratordenom, "_", fiscal_year,
                         if_else(periodx %in% c("targets"), "_", ""),
                         periodx, sep = "")) %>% 
  select(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         semifine, sex,
         colvars, valu) %>% 
  filter(colvars %in% c("HTS_TST_N_2018cum", 
                        "HTS_TST_N_2019_targets", 
                        "HTS_TST_N_2019q1", 
                        "HTS_TST_N_2019q2", 
                        "HTS_TST_N_2019q3", 
                        "HTS_TST_N_2019cum", 
                        "HTS_TST_POS_N_2018cum", 
                        "HTS_TST_POS_N_2019_targets", 
                        "HTS_TST_POS_N_2019q1", 
                        "HTS_TST_POS_N_2019q2", 
                        "HTS_TST_POS_N_2019q3", 
                        "HTS_TST_POS_N_2019cum", 
                        "TX_CURR_N_2018cum", 
                        "TX_CURR_N_2019_targets", 
                        "TX_CURR_N_2019q1", 
                        "TX_CURR_N_2019q2",
                        "TX_CURR_N_2019q3",
                        "TX_CURR_N_2019cum",
                        "TX_NEW_N_2018cum", 
                        "TX_NEW_N_2019_targets", 
                        "TX_NEW_N_2019q1", 
                        "TX_NEW_N_2019q2", 
                        "TX_NEW_N_2019q3",
                        "TX_NEW_N_2019cum", 
                        "TX_PVLS_D_2018cum", 
                        "TX_PVLS_D_2019_targets", 
                        "TX_PVLS_D_2019q1", 
                        "TX_PVLS_D_2019q2", 
                        "TX_PVLS_D_2019q3",
                        "TX_PVLS_D_2019cum",
                        "TX_PVLS_N_2018cum", 
                        "TX_PVLS_N_2019_targets", 
                        "TX_PVLS_N_2019q1", 
                        "TX_PVLS_N_2019q2",
                        "TX_PVLS_N_2019q3",
                        "TX_PVLS_N_2019cum"
  )) %>% 
  group_by(orgunituid,               
         sitename,  
         sitetype,
         psnu,                     
         psnuuid,                  
         snuprioritization,
         semifine, sex,
         colvars) %>% 
  summarize(val = sum(valu, na.rm=T)) %>%
  ungroup() %>% 
  filter(!is.na(val)) %>% 
  spread(colvars, val)

  
# Merging updated targets
# merging the priority sites variable onto dataset
fdf_revx <- left_join(fdf, sitelist)


fdf1 <- fdf_revx %>% 
  rowwise() %>% 
  mutate(NET_NEW_gap_2019cum = if_else(!is.na(TX_CURR_N_2019_targets) & TX_CURR_N_2019_targets>0, 
                                  sum(TX_CURR_N_2019_targets, -TX_CURR_N_2019cum, na.rm=T), NA_real_)) %>%
  mutate(NET_NEW_2019q1 = sum(TX_CURR_N_2019q1, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(NET_NEW_2019q2 = sum(TX_CURR_N_2019q2, -TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(NET_NEW_2019q3 = sum(TX_CURR_N_2019q3, -TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(NET_NEW_2019cum = sum(TX_CURR_N_2019cum, -TX_CURR_N_2018cum, na.rm=T)) %>% 
  mutate(gain_loss_2019q1 = sum(NET_NEW_2019q1, -TX_NEW_N_2019q1)) %>% 
  mutate(gain_loss_2019q2 = sum(NET_NEW_2019q2, -TX_NEW_N_2019q2)) %>% 
  mutate(gain_loss_2019q3 = sum(NET_NEW_2019q3, -TX_NEW_N_2019q3)) %>% 
  mutate(gain_loss_2019cum = sum(NET_NEW_2019cum, -TX_NEW_N_2019cum)) %>% 
  mutate(percent_gainloss_2019q1 = (TX_CURR_N_2019q1/
                                       sum(TX_CURR_N_2018cum,TX_NEW_N_2019q1, 
                                           na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q2 = (TX_CURR_N_2019q2/
                                      sum(TX_CURR_N_2019q1,TX_NEW_N_2019q2, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019q3 = (TX_CURR_N_2019q3/
                                      sum(TX_CURR_N_2019q2,TX_NEW_N_2019q3, 
                                          na.rm = T))-1) %>% 
  mutate(percent_gainloss_2019cum = (TX_CURR_N_2019cum/
                                   sum(TX_CURR_N_2018cum,TX_NEW_N_2019cum, 
                                       na.rm = T))-1) %>% 
  ungroup() %>% 
  mutate(HTS_POS_target_achv_2019cum = if_else(!is.na(HTS_TST_POS_N_2019_targets) & 
                                         HTS_TST_POS_N_2019_targets>0, 
                                       HTS_TST_POS_N_2019cum/
                                         HTS_TST_POS_N_2019_targets, NA_real_)) %>%
  mutate(TX_CURR_percent_2019cum = TX_CURR_N_2019cum/sum(TX_CURR_N_2019cum, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q1 = TX_CURR_N_2019q1/sum(TX_CURR_N_2019q1, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q2 = TX_CURR_N_2019q2/sum(TX_CURR_N_2019q2, na.rm=T)) %>% 
  mutate(TX_CURR_percent_2019q3 = TX_CURR_N_2019q3/sum(TX_CURR_N_2019q3, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019cum = TX_NEW_N_2019cum/sum(TX_NEW_N_2019cum, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q1 =  TX_NEW_N_2019q1/ sum(TX_NEW_N_2019q1, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q2 =  TX_NEW_N_2019q2/ sum(TX_NEW_N_2019q2, na.rm=T)) %>% 
  mutate(TX_NEW_percent_2019q3 =  TX_NEW_N_2019q3/ sum(TX_NEW_N_2019q3, na.rm=T)) %>% 
  mutate(NET_NEW_gap_percent_2019cum = NET_NEW_gap_2019cum/sum(NET_NEW_gap_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019cum = HTS_TST_POS_N_2019cum/sum(HTS_TST_POS_N_2019cum, na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q1  = HTS_TST_POS_N_2019q1 /sum(HTS_TST_POS_N_2019q1 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q2  = HTS_TST_POS_N_2019q2 /sum(HTS_TST_POS_N_2019q2 , na.rm=T)) %>% 
  mutate(HTS_POS_percent_2019q3  = HTS_TST_POS_N_2019q3 /sum(HTS_TST_POS_N_2019q3 , na.rm=T)) %>% 
  mutate(TX_PVLS_suppr_2019cum = if_else(!is.na(TX_PVLS_D_2019cum) & 
                                        TX_PVLS_D_2019cum>0, 
                                 TX_PVLS_N_2019cum/TX_PVLS_D_2019cum, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q1 = if_else(!is.na(TX_PVLS_D_2019q1) & 
                                           TX_PVLS_D_2019q1>0, 
                                         TX_PVLS_N_2019q1/TX_PVLS_D_2019q1, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q2 = if_else(!is.na(TX_PVLS_D_2019q2) & 
                                           TX_PVLS_D_2019q2>0, 
                                         TX_PVLS_N_2019q2/TX_PVLS_D_2019q2, NA_real_)) %>% 
  mutate(TX_PVLS_suppr_2019q3 = if_else(!is.na(TX_PVLS_D_2019q3) & 
                                          TX_PVLS_D_2019q3>0, 
                                        TX_PVLS_N_2019q3/TX_PVLS_D_2019q3, NA_real_)) %>% 
  mutate(yield_2019q1 = if_else(!is.na(HTS_TST_N_2019q1) & 
                                   HTS_TST_N_2019q1>0, 
                                 HTS_TST_POS_N_2019q1/HTS_TST_N_2019q1, NA_real_)) %>% 
  mutate(yield_2019q2 = if_else(!is.na(HTS_TST_N_2019q2) & 
                                   HTS_TST_N_2019q2>0, 
                                 HTS_TST_POS_N_2019q2/HTS_TST_N_2019q2, NA_real_)) %>% 
  mutate(yield_2019q3 = if_else(!is.na(HTS_TST_N_2019q3) & 
                                  HTS_TST_N_2019q3>0, 
                                HTS_TST_POS_N_2019q3/HTS_TST_N_2019q3, NA_real_)) %>% 
  mutate(yield_2019cum = if_else(!is.na(HTS_TST_N_2019cum) & 
                                   HTS_TST_N_2019cum>0, 
                                 HTS_TST_POS_N_2019cum/HTS_TST_N_2019cum, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q1 = if_else(!is.na(TX_NEW_N_2019q1) & 
                                          TX_NEW_N_2019q1>0, 
                                        NET_NEW_2019q1/TX_NEW_N_2019q1, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q2 = if_else(!is.na(TX_NEW_N_2019q2) & 
                                          TX_NEW_N_2019q2>0, 
                                        NET_NEW_2019q2/TX_NEW_N_2019q2, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019q3 = if_else(!is.na(TX_NEW_N_2019q3) & 
                                          TX_NEW_N_2019q3>0, 
                                        NET_NEW_2019q3/TX_NEW_N_2019q3, NA_real_)) %>% 
  mutate(OGAC_prox_ret_2019cum = if_else(!is.na(TX_NEW_N_2019cum) & 
                                          TX_NEW_N_2019cum>0, 
                                        NET_NEW_2019cum/TX_NEW_N_2019cum, NA_real_)) 


  
namevec <- sort(names(df1)[c(7:length(df1))])
  

fdf_finalx <- fdf1 %>% 
  select(
    orgunituid:snuprioritization,
    semifine, sex,
    priority_site,

    HTS_TST_N_2018cum,          
    HTS_TST_N_2019_targets,     
    HTS_TST_N_2019cum,          
    HTS_TST_N_2019q1,           
    HTS_TST_N_2019q2,           
    HTS_TST_N_2019q3,           
    
    HTS_TST_POS_N_2018cum,      
    HTS_TST_POS_N_2019_targets, 
    HTS_TST_POS_N_2019cum,      
    HTS_TST_POS_N_2019q1,       
    HTS_TST_POS_N_2019q2, 
    HTS_TST_POS_N_2019q3, 
    
    HTS_POS_percent_2019cum,    
    HTS_POS_percent_2019q1,     
    HTS_POS_percent_2019q2,     
    HTS_POS_percent_2019q3,     
    HTS_POS_target_achv_2019cum,

    yield_2019cum,              
    yield_2019q1,               
    yield_2019q2,         
    yield_2019q3,         
    
    TX_CURR_N_2018cum,          
    TX_CURR_N_2019_targets,  

    TX_CURR_N_2019cum,          
    TX_CURR_N_2019q1,           
    TX_CURR_N_2019q2,           
    TX_CURR_N_2019q3,           
    TX_CURR_percent_2019cum,    
    TX_CURR_percent_2019q1,     
    TX_CURR_percent_2019q2,     
    TX_CURR_percent_2019q3,     
    NET_NEW_2019cum,            
    NET_NEW_2019q1,             
    NET_NEW_2019q2,             
    NET_NEW_2019q3,             
    NET_NEW_gap_2019cum,        
    NET_NEW_gap_percent_2019cum,
    gain_loss_2019cum,          
    gain_loss_2019q1,           
    gain_loss_2019q2,           
    gain_loss_2019q3,           
    percent_gainloss_2019cum,   
    percent_gainloss_2019q1,    
    percent_gainloss_2019q2,   
    percent_gainloss_2019q3,   
    
    OGAC_prox_ret_2019q1 ,
    OGAC_prox_ret_2019q2 ,
    OGAC_prox_ret_2019q3 ,
    OGAC_prox_ret_2019cum,

    TX_NEW_N_2018cum,           
    TX_NEW_N_2019_targets,      
    TX_NEW_N_2019cum,           
    TX_NEW_N_2019q1,            
    TX_NEW_N_2019q2,            
    TX_NEW_N_2019q3,            
    TX_NEW_percent_2019cum,     
    TX_NEW_percent_2019q1,      
    TX_NEW_percent_2019q2,      
    TX_NEW_percent_2019q3,      
    TX_PVLS_D_2018cum,          
    TX_PVLS_D_2019_targets,     
    TX_PVLS_D_2019cum,          
    TX_PVLS_D_2019q1,           
    TX_PVLS_D_2019q2,           
    TX_PVLS_D_2019q3,           
    TX_PVLS_N_2018cum,          
    TX_PVLS_N_2019_targets,     
    TX_PVLS_N_2019cum,          
    TX_PVLS_N_2019q1,           
    TX_PVLS_N_2019q2,           
    TX_PVLS_N_2019q3,           
    TX_PVLS_suppr_2019cum,      
    TX_PVLS_suppr_2019q1,       
    TX_PVLS_suppr_2019q2,       
    TX_PVLS_suppr_2019q3       
  )
  




## Merging datasets together

df_final1 <- df_finalx %>% 
  mutate(level_disagg = "Total",
         age = "Total",
         sex = "Total")  
  
cdf_final1 <- cdf_finalx %>% 
  mutate(level_disagg = "Coarse age") %>% 
  mutate(age = paste(" ", trendscoarse, sep="")) %>% 
  select(-trendscoarse)

fdf_final1 <- fdf_finalx %>% 
  mutate(level_disagg = "0 to 19 only") %>% 
  mutate(age = paste(" ", semifine, sep="")) %>% 
  select(-semifine)


# absolutely final dataset

FINAL <- bind_rows(df_final1, 
                   cdf_final1,
                   fdf_final1) %>% 
  select(orgunituid:priority_site, level_disagg:sex,
         HTS_TST_N_2018cum:TX_PVLS_suppr_2019q3)








write.csv(FINAL, "2019_09_13_age_sex_site_metrics_CDI.csv", na="", row.names = F)

# Export latitude and longitude data
write.csv(latlng, "CDI_latlong.csv", na="", row.names = F)
  
  

# grouping metric dataset to PSNU level
mutate(HTS_POS_target_achv = if_else(!is.na(HTS_TST_POS_targets) & HTS_TST_POS_targets>0, 
                                     HTS_TST_POS_cum/HTS_TST_POS_targets, NA_real_)) %>%
  mutate(TX_CURR_percent = TX_CURR_cum/sum(TX_CURR_cum, na.rm=T)) %>% 
  mutate(NET_NEW_gap_percent = NET_NEW_gap/sum(NET_NEW_gap, na.rm=T)) 
  
  

# Creating topojson file
topojson_write(civ_district, file = "test.json")






