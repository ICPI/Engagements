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

data_path <- "C:/Users/GHFP/Documents/data/6.22_refresh"

results_path <- "C:/Users/GHFP/Documents/ICPI/PMTCT/results"

##  some objects for the things we want to keep

indc <- c("PMTCT_STAT", "PMTCT_ART", "PMTCT_HEI_POS", "TX_NEW", "TX_RET")

##

# pmtct <- read_msd(file.path(data_path, "MER_Structured_Dataset_OU_IM_FY17-18_20180622_v2_1.txt")) 
pmtct <- read_rds(file.path(data_path, "MER_Structured_Dataset_OU_IM_FY17-18_20180622_v2_1.rds"))%>%
  filter(indicator %in% indc)
  