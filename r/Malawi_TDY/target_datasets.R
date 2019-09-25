memory.limit(size=1000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program for created Peds dataset
#   Programmer: Imran Mujawar
#   Date started: 10/25/2018
#   Date updated: 06/19/2019
#   Program: Creates dataset for Peds tool
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
        "ISOweek",
        "xlsx",
        "rJava",
        "lubridate"))

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

gfile_loc <- filelist[1] 

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


spath <- "Site_Org_List_2019_06_24.csv"
# Pulling in the master facility list
sfoo <- read.csv(spath)

scolvecx <- c(rep("c", ncol(sfoo)))
scolvec <- paste(scolvecx, collapse = '')


siteorglist <- read_csv(spath, 
                        col_names = TRUE,
                        col_types = scolvec)

# vector for DATIM IDs for filtering MSD
sitevec <- unique(siteorglist$orgUnit)

# getting dataset with latitude and longitude
sitecoords <- siteorglist %>% 
  select(orgUnit, Latitude, Longitude) %>% 
  rename(orgunituid = orgUnit) %>% 
  unique()

# getting dataset with PrimePartner names
ims <- siteorglist %>% 
  #creating mech ID and PrimePartner combo name
  mutate(partner = gsub("[[:space:]]", "_", PrimePartner)) %>% 
  mutate(id_partner = paste(MechanismNumber, partner, sep="_")) %>% 
  select(id_partner, MechanismNumber, PrimePartner) %>% 
  rename(mechanismid = MechanismNumber) %>% 
  unique()

im_vec <- unique(ims$mechanismid)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Creating Metadata dataframe ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metadf <- datim %>%   
  select(orgunituid,               
         sitename,                 
         sitetype,                 
         communityprioritization,  
         facilityprioritization,  
         operatingunit,            
         snu1,                     
         snu1uid,                  
         psnu,                     
         psnuuid,                  
         snuprioritization,        
         fundingagency,            
         primepartner,             
         implementingmechanismname,
         mechanismuid,             
         mechanismid) %>% 
  unique() %>% 
  # Filter for only CDC 
  filter(fundingagency %in% c("HHS/CDC")) %>% 
  filter(orgunituid %in% sitevec) %>% 
  filter(mechanismid %in% im_vec)



# Merging latitude and longitude data onto metadata
metadf1 <- left_join(metadf, sitecoords)

# Merging Partner metadata to the file
metadf2 <- left_join(metadf1, ims)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Adding indicators ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= HTS_TST ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# moddf <- datim %>% 
#   filter(indicator %in% c("HTS_TST")) %>% 
#   filter(fiscal_year %in% c("2019")) %>%
#   filter(sitetype %ni% c("Military")) %>% 
#   select(modality, sitetype) %>% unique() %>% 
#   filter(!is.na(modality))


# Pulling in the Excel file with indicator overview
ifoo <- read.csv("indicator_list.csv")
icolvecx <- c(rep("c", ncol(ifoo)))
icolvec <- paste(icolvecx, collapse = '')
ilist <- read_csv("indicator_list.csv", 
                  col_names = TRUE,
                  col_types = icolvec)


# pulling in the hts modality dataset
mfile <- "modality_data_2019_06_24.csv"
mfoo <- read.csv(mfile)
mcolvecx <- c(rep("c", ncol(mfoo)))
mcolvec <- paste(mcolvecx, collapse = '')

modlist <- read_csv(mfile, 
                    col_names = TRUE,
                    col_types = mcolvec)

modlist_fac <- modlist %>%
  filter(sitetype %in% c("Facility")) %>% 
  select(-sitetype)

modlist_com <- modlist %>%
  filter(sitetype %in% c("Community")) %>% 
  select(-sitetype)

# Adding modality data for each facility and community
# depending on modality type
# ~~~ For Facility ~~~

metadf_fac <- metadf2 %>% 
  filter(sitetype %in% c("Facility"))

metadf_com <- metadf2 %>% 
  filter(sitetype %in% c("Community"))


# cartesian product of metadata and modality
# facility with facility modalities 
df_fac <- merge(x = metadf_fac, y = modlist_fac, by = NULL)

# community with community modalities 
df_com <- merge(x = metadf_com, y = modlist_com, by = NULL)

df_mod <- bind_rows(df_fac, df_com) %>% 
  mutate(indicator = "HTS_TST")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= HTS_INDEX ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # Adding HTS_INDEX to facility data
metadf_faci <- metadf_fac %>% 
  mutate(indicator = "HTS_INDEX",
         modality_datim = "Index")

metadf_comi <- metadf_com %>% 
  mutate(indicator = "HTS_INDEX",
         modality_datim = "IndexMod")

df_index <- bind_rows(metadf_faci, metadf_comi)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= HTS data with statushiv ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
statushiv_vec <- as.vector(unique(datim$statushiv)[c(2,3,4)])


htsx <- bind_rows(df_mod, df_index) 

#cartesian merge with status
hts_stat <- merge(x = htsx, y = statushiv_vec, by = NULL) %>% 
  rename(statushiv = y)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= TX_NEW data  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding TX_NEW data only to facilities
tx_new <- metadf_fac %>% 
  mutate(indicator = "TX_NEW")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Custom Indicators  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indvec <- unique(ilist$indicator)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Patient tracking variables  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating the denominator variable for 
trk_d <- metadf_fac %>% 
  mutate(indicator = indvec[4])


# Creating the numerator variable
trk_n <- metadf_fac %>% 
  mutate(indicator = indvec[5])


# Adding trk_n with disaggs only to facilities
trk <- ilist %>% 
  filter(indicator %in% c("pt_trk_n"))

stdd_vec <- bind_cols(str_split(unique(trk$disaggs),pattern = "~")[1])%>% 
  rename(trk_outcome=V1)


#cartesian merge with tracking status
trk_n1 <- merge(x = trk_n, y = stdd_vec, by = NULL) 

trk_all <- bind_rows(trk_n1, trk_d)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Viral Load variables  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# variable vl_samples
# Creating the viral load sample variable for 
vl <- metadf_fac %>% 
  mutate(indicator = indvec[6])

vlsi <- ilist %>% 
  filter(indicator %in% indvec[6])

vl_vec <- bind_cols(str_split(unique(vlsi$disaggs),pattern = "~")[1])%>% 
  rename(reason_for_test=V1)


#cartesian merge with vl dataset
vl_all <- merge(x = vl, y = vl_vec, by = NULL) 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Compiling the metadataset  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
final_df <- bind_rows(hts_stat,
                      tx_new,
                      trk_all,
                      vl_all) %>% 
  mutate(otherdisaggregate =            
           case_when(
           indicator %in% c("pt_trk_n")         ~ trk_outcome,
           indicator %in% c("vl_samples")       ~ reason_for_test,
           TRUE                                 ~ "")) %>% 
  select(-c(trk_outcome, reason_for_test))
  
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Adding weeks  ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setting starting date for Monday 
x <- as.Date("2019-07-01")

num_weeks <- 12

# vector for the start dates
vecweek <- seq(x, by=7, length.out = num_weeks)
# vector with the end dates
vecweekend <- vecweek+6

monthvec <- c()

for (i in 1:length(vecweek)){
mweek <- lubridate::month(seq(vecweek[i], vecweekend[i], by="days"))
monthvec[i] <- month.abb[median(mweek)]
}

yearvec <- c()

for (j in 1:length(vecweek)){
  yweek <- lubridate::year(seq(vecweek[j], vecweekend[j], by="days"))
  yearvec[j] <- median(yweek)
}


num_months = round(num_weeks/4, 0)

monthcount <- paste("month", sort(c(rep(seq(1:num_months), 4))), sep="_")



# creating dataset with week numbers and dates. 
weekdata <- as.data.frame(cbind(as.character(vecweek), as.character(vecweekend),
                                as.character(monthvec), as.character(monthcount), 
                                as.character(yearvec))) %>% 
  rename(weekstart  = V1,
         weekends   = V2,
         monthname  = V3,
         monthcount = V4,
         yr         = V5) %>% 
  # getting year 
  mutate(weeknum = paste("week", 
                         formatC(row_number(), width=2, flag="0"), 
                         sep="_")) %>% 
  mutate(weekname = paste(weeknum, "_from_",
                          str_sub(weekstart, start= -5), "_to_", 
                          str_sub(weekends, start= -5), "-",str_sub(weekends, start=1, end=4),
                          sep=""))


# creating cartesian product with weeks for all indicators and metadata
all_weeks <- merge(x = final_df, y = weekdata, by = NULL) 



# creating columns for age band entry
final_weeks <- all_weeks %>% 
  mutate(default = 0,
         f_lt_01 = 0,
         f_lt_05 = 0,
         f_01_04 = 0,
         f_05_09 = 0,
         f_10_14 = 0,
         f_15_19 = 0,
         f_20_24 = 0,
         f_25_29 = 0,
         f_30_34 = 0,
         f_35_39 = 0,
         f_40_44 = 0,
         f_45_49 = 0,
         f_50_plus = 0,
         f_unknown = 0,
         m_lt_01 = 0,
         m_lt_05 = 0,
         m_01_04 = 0,
         m_05_09 = 0,
         m_10_14 = 0,
         m_15_19 = 0,
         m_20_24 = 0,
         m_25_29 = 0,
         m_30_34 = 0,
         m_35_39 = 0,
         m_40_44 = 0,
         m_45_49 = 0,
         m_50_plus = 0,
         m_unknown = 0,
         f_lt_15 = 0,
         f_15_plus = 0,
         m_lt_15 = 0,
         m_15_plus = 0,
         female = 0,
         male = 0  ) %>% 
  # arranging variables as preferred
  select(
    id_partner,  
    indicator,
    snu1uid,                  
    psnuuid,                  
    snu1,                     
    psnu,                     
    snuprioritization,  
    sitetype,                 
    communityprioritization,  
    facilityprioritization, 
    modality_ou1,             
    modality_ou2, 
    modality_datim, 
    statushiv,
    otherdisaggregate,      
    Latitude,                 
    Longitude,
    orgunituid,               
    sitename,
    weekstart:male
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Simulating data for each indicator ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sim_hts_neg <- final_weeks %>% 
  filter(indicator %in% c("HTS_TST") & 
           statushiv %in% c("Negative")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_hts_neg$valx <- round(rnorm(nrow(sim_hts_neg), mean=500000, sd=100000),0)

sim_hts_pos <- final_weeks %>% 
  filter(indicator %in% c("HTS_TST") & 
           statushiv %in% c("Positive")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_hts_pos$valx <- round(rnorm(nrow(sim_hts_pos), mean=50000, sd=10000),0)

sim_hts_uk <- final_weeks %>% 
  filter(indicator %in% c("HTS_TST") & 
           statushiv %in% c("Unknown")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_hts_uk$valx <- round(runif(nrow(sim_hts_uk), min=1000, max=5000),0)


sim_ind_neg <- final_weeks %>% 
  filter(indicator %in% c("HTS_INDEX") & 
           statushiv %in% c("Negative")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_ind_neg$valx <- round(runif(nrow(sim_ind_neg), min=100000, max=500000),0)

sim_ind_pos <- final_weeks %>% 
  filter(indicator %in% c("HTS_INDEX") & 
           statushiv %in% c("Positive")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_ind_pos$valx <- round(runif(nrow(sim_ind_pos), min=10000, max=50000),0)

sim_ind_uk <- final_weeks %>% 
  filter(indicator %in% c("HTS_INDEX") & 
           statushiv %in% c("Unknown")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_ind_uk$valx <- round(runif(nrow(sim_ind_uk), min=1000, max=5000),0)

sim_tx <- final_weeks %>% 
  filter(indicator %in% c("TX_NEW")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_tx$valx <- round(runif(nrow(sim_tx), min=8000, max=40000),0)


sim_trk_d <- final_weeks %>% 
  filter(indicator %in% c("pt_trk_d")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_trk_d$valx <- round(runif(nrow(sim_trk_d), min=80, max=400),0)

sim_trk_n <- final_weeks %>% 
  filter(indicator %in% c("pt_trk_n")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_trk_n$valx <- round(runif(nrow(sim_trk_n), min=50, max=100),0)

sim_vl <- final_weeks %>% 
  filter(indicator %in% c("vl_samples")) %>% 
  select(-c(f_lt_15,
            f_15_plus,
            m_lt_15,
            m_15_plus,
            default)) %>% 
  gather(age, val, f_lt_01:m_unknown) 

sim_vl$valx <- round(runif(nrow(sim_vl), min=5000, max=10000),0)


sims_final <- bind_rows(sim_hts_neg,
                        sim_hts_pos,
                        sim_hts_uk,
                        sim_ind_neg,
                        sim_ind_pos,
                        sim_ind_uk,
                        sim_tx,
                        sim_trk_d,
                        sim_trk_n,
                        sim_vl) 


sims_final1 <- sims_final %>% 
  mutate_if(is.factor, list(~as.character(.))) %>% 
  select(-val) %>% 
  group_by(id_partner,             
           indicator,              
           snu1uid,                
           psnuuid,                
           snu1,                   
           psnu,                   
           snuprioritization,      
           sitetype,               
           communityprioritization,
           facilityprioritization, 
           modality_ou1,           
           modality_ou2,           
           modality_datim,         
           statushiv,              
           otherdisaggregate,      
           Latitude,               
           Longitude,              
           orgunituid,             
           sitename,
           yr, monthname, monthcount,
           weekstart,              
           weekends,               
           weeknum,                
           weekname,               
           age) %>% 
  summarise_all(list(~sum), na.rm=T) %>% 
  ungroup() 

sim_final1x <- sims_final1 %>% 
  spread(age, valx)



sim_finalxx <- sim_final1x %>% 
  mutate(f_lt_15 = f_lt_01 +	
                   f_lt_05 +
                   f_01_04 +	
                   f_05_09 +	
                   f_10_14,
         f_15_plus = f_15_19	+
                     f_20_24	+
                     f_25_29	+
                     f_30_34	+
                     f_35_39	+
                     f_40_44	+
                     f_45_49	+
                     f_50_plus,
         m_lt_15 = m_lt_01 +	
                   m_lt_05 +
                   m_01_04 +	
                   m_05_09 +	
                   m_10_14,
         m_15_plus = m_15_19	+
                     m_20_24	+
                     m_25_29	+
                     m_30_34	+
                     m_35_39	+
                     m_40_44	+
                     m_45_49	+
                     m_50_plus,
         female  = f_lt_15 + f_15_plus + f_unknown,
         male    = m_lt_15 + m_15_plus + m_unknown,
         default = f_lt_15 + f_15_plus + f_unknown +
                   m_lt_15 + m_15_plus + m_unknown ) %>% 
  select(id_partner:weekname,default,
         f_lt_01, f_lt_05, f_01_04:f_50_plus,
         m_lt_01, m_lt_05, m_01_04:m_50_plus,
         female, male)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Excel templates ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# creating vector of partners to loop through
partner_v <- as.vector(unique(final_weeks$id_partner))


# options(java.parameters = "-Xmx80000m")
# 
# jgc <- function()
# {
#   gc()
#   .jcall("java/lang/System", method = "gc")
# }     
# 
# keepers <- c("partner_v",
#              "ilist",
#              "sim_final1xx",
#              "%ni%",
#              "folder_arch",
#              "indvec")
# 
# rm(list=setdiff(ls(), keepers))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function that populates csv templates ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating folder architecture
mainDir <- "C:/Temp_work/ShareFile_sync/Personal Folders/Test_folder/Malawi_Mockdata/Partner_Data"


for (i in 1:length(partner_v)) {
  subDir <- paste("_", partner_v[i], sep="")

if (file.exists(paste(mainDir,"/", subDir, sep = ""))) {
  cat("subDir exists in mainDir and is a directory")
} else {
  cat("subDir does not exist in mainDir - creating")
  dir.create(file.path(mainDir, subDir))
}

    filepath = paste(mainDir,"/", subDir, "/", "weekly_data_", partner_v[i], "_all_vars.csv", sep = "")  
    
    df <- sim_finalxx %>% 
      filter(id_partner %in% partner_v[i])
    
    write.csv(df, filepath, row.names = F, na="")
    
    rm(df)

}


