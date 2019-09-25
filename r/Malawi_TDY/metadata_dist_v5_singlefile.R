memory.limit(size=1000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program for creating metadata for all partners in one file
#   Programmer: Imran Mujawar
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
        "rJava"))

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

# vector for the start dates
vecweek <- seq(x, by=7, length.out = 12)
# vector with the end dates
vecweekend <- vecweek+6


# creating dataset with week numbers and dates. 
weekdata <- as.data.frame(cbind(as.character(vecweek), as.character(vecweekend))) %>% 
  rename(weekstart = V1,
         weekends  = V2) %>% 
  mutate(weeknum = paste("week", 
                         formatC(row_number(), width=2, flag="0"), 
                         sep="_")) %>% 
  mutate(weekname = paste(weeknum, "_from_",
                          str_sub(weekstart, start= -5), "_to_", 
                          str_sub(weekends, start= -5), sep=""))


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
         m_15_plus = 0) %>% 
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
    weekstart:m_15_plus
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Excel templates ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# creating vector of partners to loop through
partner_v <- as.vector(unique(final_weeks$id_partner))


options(java.parameters = "-Xmx80000m")

jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}     

keepers <- c("partner_v",
             "ilist",
             "final_weeks",
             "%ni%",
             "folder_arch",
             "indvec")

rm(list=setdiff(ls(), keepers))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function that populates Excel templates ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating folder architecture
mainDir <- "C:/Temp_work/TDY_Malawi/Testing_Rscript_single"


for (i in 1:length(partner_v)) {
  subDir <- paste("_", partner_v[i], sep="")

if (file.exists(paste(mainDir,"/", subDir, sep = ""))) {
  cat("subDir exists in mainDir and is a directory")
} else {
  cat("subDir does not exist in mainDir - creating")
  dir.create(file.path(mainDir, subDir))
}

    filepath = paste(mainDir,"/", subDir, "/", partner_v[i], "_all_vars.csv", sep = "")  
    
    df <- final_weeks %>% 
      filter(id_partner %in% partner_v[i])
    
    write.csv(df, filepath, row.names = F)
    
    rm(df)

}


