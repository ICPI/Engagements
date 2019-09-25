#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 


# This function lets you check whether a vector of packages is installed, and 
# if not installed installs them, before finally loading all the packages
ldpkg <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ldpkg(c(
  "tidyverse",
  "readxl",
  "plotly",
  "leaflet" , 
  "rgdal" , 
  "rgeos", 
  "sp", 
  "RColorBrewer", 
  "htmltools", 
  "leaflet.minicharts",
  "stringr"
))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in DATA ~~~~~~~==============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Finding the weekly data by key word 'Monthly'
glist <- list.files(pattern="Monthly")

# Creating the dataset headers
colnam <- c(
  "codemer",
  "periode",
  "partnaire",
  "region",
  "district",
  "site",
  "hts_tst_f_lt15",
  "hts_tst_f_lt15plus",
  "hts_tst_m_lt15",
  "hts_tst_m_lt15plus",
  "hts_tst",
  "blah1",
  "blah2",
  "hts_tst_pos_f_lt15",
  "hts_tst_pos_f_lt15plus",
  "hts_tst_pos_m_lt15",
  "hts_tst_pos_m_lt15plus",
  "hts_tst_pos",
  "blah3",
  "blah4",
  "tx_new_f_lt15",
  "tx_new_f_lt15plus",
  "tx_new_m_lt15",
  "tx_new_m_lt15plus",
  "tx_new",
  "tx_new_n_f_lt15",
  "tx_new_n_f_lt15plus",
  "tx_new_n_m_lt15",
  "tx_new_n_m_lt15plus",
  "tx_new_n",
  "tx_new_p_f_lt15",
  "tx_new_p_f_lt15plus",
  "tx_new_p_m_lt15",
  "tx_new_p_m_lt15plus",
  "tx_new_p",
  "pre_art_d_lt15",
  "pre_art_d_15plus",
  "pre_art_d",
  "pre_art_f_lt15",
  "pre_art_f_15plus",
  "pre_art_f",
  "tx_curr_f_lt15",
  "tx_curr_f_lt15plus",
  "tx_curr_m_lt15",
  "tx_curr_m_lt15plus",
  "tx_curr"
)


# Creating empty list to take in multiple datasets to stack
datalist = list()


# For loop, looping through all the Excel files
for (x in 1:length(glist)) {
  
# taking the xth Excel file  
xlpath <- glist[x]

# Breaking up the name of file to get partner name
nlist <- strsplit(xlpath, "_")[[1]]
# Partner name
partnerx <- nlist[1]

# nested for loop to read in each sheet in the xth excel file
datatab = list()

for (i in 1:4){

# reading in the i th sheet data  
  df  <- read_excel(xlpath, sheet = i, col_names = F,
           na = "", trim_ws = TRUE, skip = 3,
           n_max = 500, guess_max = 500,
           progress = readxl_progress())
  
  dfx <- df[,1:46]
  
  
# Changing column headers 
    names(dfx) <- colnam
  

# Modifying the data      
df1 <- dfx %>% 
  # Creating partner name variable and month number
  mutate(partner = partnerx,
         month = i) %>% 
  # Only keeping desired variables
  select(partner, month, region, district, site, 
         hts_tst, hts_tst_pos, tx_new, tx_curr,
         partnaire, periode)  
  
datatab[[i]] <- df1

}

# Stacking all the 4 months' data from the Excel file
pdata <- dplyr::bind_rows(datatab)

datalist[[x]] <- pdata
}


# Stacking the datasets from all Excel files
finaldf <- bind_rows(datalist) %>% 
  mutate(month_name = case_when(
    month==1 ~ "janvier",
    month==2 ~ "février",
    month==3 ~ "mars",
    month==4 ~ "avril"
  )) %>% 
  select(partner, month_name, month, site:tx_curr) %>% 
  filter(!is.na(site)) 


# Output the final dataset in csv
write.csv(finaldf, "2019_05_28_WeeklyData.csv", row.names = F, na="")


# Pulling in the site names
site93 <- read_excel("Sites/Copy of 93 sites for immediate site visits.xlsx", 
                     range = "Sheet1!G1:H94", 
                   guess_max = 90)


# Pulling in the site names
site140 <- read_excel("Sites/Copy of 140 Priority Sites List.xlsx", 
                     range = "Sheet3!A2:E142", 
                     guess_max = 100)


s93 <- unique(site93$Site)
s140 <- unique(site140$organisationunitname)

allsites <- sort(unique(c(s140)))

allsitesx <- str_replace_all(tolower(allsites), "[[:punct:]]", "")



sfinaldf <- finaldf %>% 
  filter(site %in% allsites) %>% 
  gather(indicator, value, hts_tst, hts_tst_pos, tx_new, tx_curr) 


# Adding dataset with previous tx_curr value to get net new
txp <- sfinaldf %>% 
  filter(indicator %in% c("tx_curr")) %>% 
  # Adds month to push tx_curr value to next month
  mutate(month = month+1) %>% 
  mutate(indicator = "tx_curr_p") %>% 
  mutate(month_name = case_when(
    month==1 ~ "janvier",
    month==2 ~ "février",
    month==3 ~ "mars",
    month==4 ~ "avril"
  )) %>% 
  filter(month %in% c(2, 3, 4))


# adding it on to the main dataset
sfdf <- bind_rows(sfinaldf, txp) %>% 
  spread(indicator, value) %>% 
  rowwise() %>% 
  mutate(net_new = if_else(month==1, 0, sum(tx_curr, -tx_curr_p, na.rm=T))) %>%
  ungroup() %>% 
  select(month_name:site, hts_tst:net_new) %>% 
  group_by(month_name, month, site) %>% 
  summarise_all(list(~sum), na.rm=T) %>% 
  ungroup() 

  


write.csv(sfdf, "Output/2019_05_28_MonthlyData_prsites.csv", row.names = F, na="")


# Pulling in data from the MSD
merlist <- list.files("RawData/",pattern="MER")

# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file=paste("RawData/", merlist, sep=""),
                col_names = TRUE, n_max = 0)   

foonames <- tolower(names(foo))

colvecx <- as.vector(ifelse(grepl("qtr|targets|cumulative", foonames), "d", "c"))

# colvecx <- as.vector(ifelse(foonames %in% c("FILL IN STUFF HERE"  ), "d", "c"))

colvec <- paste(colvecx, collapse = '')


# Pulling in the data with correct datatype for variables  
datim <- read_tsv(file=paste("RawData/", merlist, sep=""),
                  col_names = TRUE,
                  col_types = colvec)      # ending if-else for Genie check

names(datim) <- tolower(names(datim))  


# Restructuring the DATIM dataset
df <- datim %>% 
  filter(indicator %in% c("TX_CURR") & 
          standardizeddisaggregate %in% c("Total Numerator") &
           fundingagency %in% c("HHS/CDC") & 
           fiscal_year %in% c("2019", "2018")) %>%
  # converting site names to lowercase for better matching
  mutate(sitex = str_replace_all(tolower(sitename), "[[:punct:]]", "")) %>%
  filter(sitex %in% c(allsitesx, 
                      "centre integre de recherchesâ biocliniquesâ dâ€™abidjan cirba",
                      "dispensaire municipal de mâ€™pouto", 
                      "dispensaire rural de mâ€™badon"
  )) %>%
  select(orgunituid, sitename, fiscal_year, cumulative, targets) %>%
  gather(period, value, cumulative, targets) %>% 
  filter(!is.na(value)) %>% 
  mutate(pf = paste(fiscal_year, period, sep="")) %>% 
  filter(pf %in% c("2018cumulative","2019targets")) %>% 
  select(orgunituid, sitename, period, value) %>% 
  group_by_if(is.character) %>% 
  summarise_all(list(~sum), na.rm=T) %>% 
  ungroup() %>%
  spread(period, value) %>%
  rowwise() %>% 
  mutate(net_new_t = sum(targets, -cumulative, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(mnet_new_t = round(net_new_t/12,0)) %>% 
  select(orgunituid, sitename, mnet_new_t) %>% 
  rename(site = sitename)


expdf <- left_join(sfdf, df) %>% 
  select(month_name, month, orgunituid, site,
         hts_tst,
         hts_tst_pos,	
         tx_curr,	
         tx_curr_p,	
         tx_new,	
         net_new,	
         mnet_new_t) %>% 
  mutate(mnet_new_tx = if_else(month==1, tx_curr, mnet_new_t)) %>%
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>% 
  group_by(orgunituid, site) %>% 
  arrange(month) %>% 
  mutate(tx_curr_t = cumsum(mnet_new_tx)) %>% 
  ungroup() 



# Swapping out the TX_CURR for March with TX_CURR from FY2019Q2
tx <- datim %>% 
  filter(indicator %in% c("TX_CURR") & 
           standardizeddisaggregate %in% c("Total Numerator") &
           fundingagency %in% c("HHS/CDC") & 
           fiscal_year %in% c("2019")) %>%
  # converting site names to lowercase for better matching
  mutate(sitex = str_replace_all(tolower(sitename), "[[:punct:]]", "")) %>%
  filter(sitex %in% c(allsitesx)) %>%
  select(orgunituid, sitename, qtr2) %>% 
  group_by(orgunituid, sitename) %>% 
  summarise_all(list(~sum), na.rm=T) %>%
  ungroup()
  
write.csv(tx, "Output/2019_05_29_March_DATIM18.csv", row.names = F, na="")


write.csv(expdf, "Output/2019_05_29_MonthlyData_prsites_targets.csv", row.names = F, na="")









