
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program for creating dataset for Agency self-review Dashboard
#   Programmer: Imran Mujawar
#   Date started: 11/05/2019
#   Date updated: 
# ---------------------------------------------------------------
memory.limit()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pulling in the load package function R file
# Load function to install list of packages
ldpkg <- dget("ldpkg.R")

# Running the ldpkg function which checks if package is installed 
# and then loads it if it is, otherwise, installs and then loads
ldpkg(c("readr",
        "tidyverse",
        "epitools",
        "ggpubr", 
        "lme4"))

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
# ============= Pulling in Site-IM ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffolder <- "C:/temp_folder/peds_index/IAS_peds_IndexTesting/RawData"

kfilelist <- list.files(ffolder, pattern="SITE_IM")


gloc <- paste(ffolder, "/", kfilelist[1], sep="")

if(grepl(".txt", gloc)){
  
foo <- read_tsv(file=gloc, 
                col_names = TRUE, n_max = 0)
  
} else {

  filex <- unzip(gloc, list = TRUE) %>% .$Name


foo <- read_tsv(file=unz(gloc, filex), 
                col_names = TRUE, n_max = 0)

close(unz(gloc, filex))
}

foonames <- tolower(names(foo))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in Site-IM ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("qtr|targets|cumulative", foonames), "d", "c"))

colvec <- paste(colvecx, collapse = '')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Modifying the dataset ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # char_var <- char_varx[!grepl("prioritization|approvallevel", char_varx)]
  num_var <- foonames[grepl("qtr|targets", foonames)]

char_varx <- foonames[!grepl("qtr|targets|cumulative", foonames)]
  char_var <- char_varx[!grepl("approvallevel", char_varx)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Loop-edy Loop ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  newlist <- kfilelist[c(29,26,23,21,24,37,32,12,13,31,8,16)]
  
peds_pop <- function(k){

# Pulling the dataset with the correct column types
xloc <- paste(ffolder, "/", k, sep="")

if(grepl(".txt", k)){
  
  datim <- read_tsv(file=xloc, 
                    col_names = TRUE, 
                    col_types = colvec)
  
} else {

xfile <- unzip(xloc, list = TRUE) %>% .$Name

datim <- read_tsv(file=unz(xloc, xfile), 
                col_names = TRUE, 
                col_types = colvec)

close(unz(xloc, xfile))}


names(datim) <- tolower(names(datim))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Getting Treatment Trends dataset ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#getting character variables for treatment dataset
#gather time periods and spread indicators
indx1 <- datim %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
  filter(standardizeddisaggregate %in% 
           c("Modality/Age Aggregated/Sex/Result",
             "Modality/Age/Sex/Result",
             "Modality/MostCompleteAgeDisagg")) %>% 
  filter(trendscoarse %in% c("<15")) %>% 
  select(operatingunit,
         orgunituid, snu1uid, psnuuid,
         indicator,
         modality, sex, 
         trendssemifine, trendsfine, trendscoarse,
         fiscal_year,
         cumulative) %>% 
  filter(fiscal_year %in% c("2019", "2018")) %>%   
  mutate(index_all = if_else(modality %in% c("Index Testing", "IndexMod", "Index"),
                             "indx", "non")) %>% 
  mutate(colvars = paste(index_all, indicator, fiscal_year, sep="_")) %>%
  select(-c(indicator, fiscal_year, modality)) %>%
  group_by(operatingunit, psnuuid, orgunituid, colvars) %>% 
  summarise(valu=sum(cumulative, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(valu)) %>% 
  spread(colvars, valu) %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>%
  mutate(tst_iprop_2018 = indx_HTS_TST_2018/(indx_HTS_TST_2018+non_HTS_TST_2018)) %>% 
  mutate(tst_iprop_2019 = indx_HTS_TST_2019/(indx_HTS_TST_2019+non_HTS_TST_2019)) %>% 
  mutate(pos_iprop_2018 = indx_HTS_TST_POS_2018/(indx_HTS_TST_POS_2018+non_HTS_TST_POS_2018)) %>% 
  mutate(pos_iprop_2019 = indx_HTS_TST_POS_2019/(indx_HTS_TST_POS_2019+non_HTS_TST_POS_2019)) %>% 
  # mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>%
  mutate(tst_iprop_diff = (tst_iprop_2019 - tst_iprop_2018)) %>% 
  mutate(pos_iprop_diff = (pos_iprop_2019 - pos_iprop_2018)) %>% 
  mutate(tst_diff = indx_HTS_TST_2019 - indx_HTS_TST_2018) %>% 
  mutate(pos_diff = indx_HTS_TST_POS_2019 - indx_HTS_TST_POS_2018) 

indx1

}
 



indlist <- map(.x=newlist, 
              .f = ~peds_pop(.x))


indxx <- dplyr::bind_rows(indlist)


# Creating the dataset for the linear mixed logistic regression model 
ltst <- indxx %>% 
  select(operatingunit, orgunituid, 
         indx_HTS_TST_2018,
         indx_HTS_TST_2019,
         non_HTS_TST_2018,
         non_HTS_TST_2019) %>% 
  gather(colvar, vcount, indx_HTS_TST_2018,
         indx_HTS_TST_2019,
         non_HTS_TST_2018,
         non_HTS_TST_2019) %>% 
  filter(vcount>0) %>% 
  mutate(modtype = substr(colvar, 0, 3)) %>% 
  mutate(modbin = if_else(modtype %in% c("ind"), 1, 0)) %>% 
  mutate(period = if_else(grepl("2019", colvar), "2019", "2018")) %>% 
  uncount(vcount)


ltst2 <- indxx %>% 
  select(operatingunit, psnuuid, orgunituid, 
         indx_HTS_TST_2018,
         indx_HTS_TST_2019,
         non_HTS_TST_2018,
         non_HTS_TST_2019) %>% 
  gather(colvar, vcount, indx_HTS_TST_2018,
         indx_HTS_TST_2019,
         non_HTS_TST_2018,
         non_HTS_TST_2019) %>% 
  filter(vcount>0) %>% 
  mutate(modtype = substr(colvar, 0, 3)) %>% 
  mutate(period = as.factor(if_else(grepl("2019", colvar), "2019", "2018"))) %>% 
  select(-colvar) %>% 
  group_by(operatingunit, psnuuid, orgunituid, period, modtype) %>% 
  summarise(valu = sum(vcount, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(valu)) %>% 
  spread(modtype, valu) %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>%
  mutate(denom = ind + non)




  
lpos2 <- indxx %>% 
  select(operatingunit, psnuuid, orgunituid, 
         indx_HTS_TST_POS_2018,
         indx_HTS_TST_POS_2019,
         non_HTS_TST_POS_2018,
         non_HTS_TST_POS_2019) %>% 
  gather(colvar, vcount, 
         indx_HTS_TST_POS_2018,
         indx_HTS_TST_POS_2019,
         non_HTS_TST_POS_2018,
         non_HTS_TST_POS_2019) %>% 
  filter(vcount>0) %>% 
  mutate(modtype = substr(colvar, 0, 3)) %>% 
  mutate(period = as.factor(if_else(grepl("2019", colvar), "2019", "2018"))) %>% 
  select(-colvar) %>% 
  group_by(operatingunit, psnuuid, orgunituid, period, modtype) %>% 
  summarise(valu = sum(vcount, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(valu)) %>% 
  spread(modtype, valu) %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>%
  mutate(denom = ind + non)


oulevel <- indxx %>% 
  select(operatingunit, 
         indx_HTS_TST_2018,
         indx_HTS_TST_2019,
         non_HTS_TST_2018,
         non_HTS_TST_2019,
         indx_HTS_TST_POS_2018,
         indx_HTS_TST_POS_2019,
         non_HTS_TST_POS_2018,
         non_HTS_TST_POS_2019) %>% 
  group_by(operatingunit) %>% 
  summarise_all(list(~sum(., na.rm=T))) %>% 
  ungroup() %>% 
  mutate(tst_iprop_2018 = indx_HTS_TST_2018/(indx_HTS_TST_2018+non_HTS_TST_2018)) %>% 
  mutate(tst_iprop_2019 = indx_HTS_TST_2019/(indx_HTS_TST_2019+non_HTS_TST_2019)) %>% 
  mutate(pos_iprop_2018 = indx_HTS_TST_POS_2018/(indx_HTS_TST_POS_2018+non_HTS_TST_POS_2018)) %>% 
  mutate(pos_iprop_2019 = indx_HTS_TST_POS_2019/(indx_HTS_TST_POS_2019+non_HTS_TST_POS_2019)) %>% 
  mutate(stat_tst_2018 = paste(round(tst_iprop_2018*100,1), 
                               "% \n(", prettyNum(indx_HTS_TST_2018,big.mark=","), 
                               "/", prettyNum((indx_HTS_TST_2018+non_HTS_TST_2018),big.mark=","),
                               ")", sep="")) %>% 
  mutate(stat_tst_2019 = paste(round(tst_iprop_2019*100,1), 
                               "% \n(", prettyNum(indx_HTS_TST_2019,big.mark=","), 
                               "/", prettyNum((indx_HTS_TST_2019+non_HTS_TST_2019),big.mark=","),
                               ")", sep="")) %>% 
  mutate(stat_tst_2018 = paste(round(tst_iprop_2018*100,1), 
                               "% \n(", prettyNum(indx_HTS_TST_2018,big.mark=","), 
                               "/", prettyNum((indx_HTS_TST_2018+non_HTS_TST_2018),big.mark=","),
                               ")", sep="")) %>% 
  mutate(stat_pos_2018 = paste(round(pos_iprop_2018*100,1), 
                               "% \n(", prettyNum(indx_HTS_TST_POS_2018,big.mark=","), 
                               "/", prettyNum((indx_HTS_TST_POS_2018+non_HTS_TST_POS_2018),big.mark=","),
                               ")", sep="")) %>% 
  mutate(stat_pos_2019 = paste(round(pos_iprop_2019*100,1), 
                               "% \n(", prettyNum(indx_HTS_TST_POS_2019,big.mark=","), 
                               "/", prettyNum((indx_HTS_TST_POS_2019+non_HTS_TST_POS_2019),big.mark=","),
                               ")", sep="")) %>% 
  select(operatingunit,
         stat_tst_2018, stat_tst_2019,
         stat_pos_2018, stat_pos_2019)
  


odd_ou <- function(x){  
  
ltst2x <- ltst2 %>% filter(operatingunit %in% x) 

m <- glmer(ind/denom~period+
             (1|psnuuid/orgunituid),
           weights=denom,
           data=ltst2x,family="binomial")

px <- coef(summary(m))[,4]
pval <- px[2]
se <- sqrt(diag(vcov(m)))

tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
               se)  

odf <- as.data.frame(exp(tab)) %>% mutate(operatingunit = x) %>% 
  slice(2:2) %>% 
  mutate(pvalu = pval)

odf }

ouvec <- unique(ltst2$operatingunit)

oddlist <- map(.x=ouvec, 
               .f = ~odd_ou(.x))


odd_df <- dplyr::bind_rows(oddlist)

write.csv(odd_df, "tst_odds.csv")


odd_oup <- function(x){  
  
  lpos2x <- lpos2 %>% filter(operatingunit %in% x) 
  
  m <- glmer(ind/denom~period+
               (1|psnuuid/orgunituid),
             weights=denom,
             data=lpos2x,family="binomial")
  
  px <- coef(summary(m))[,4]
  pval <- px[2]
  se <- sqrt(diag(vcov(m)))
  
  tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                 se)  
  
  odf <- as.data.frame(exp(tab)) %>% mutate(operatingunit = x) %>% 
    slice(2:2) %>% 
    mutate(pvalu = pval)
  
  odf }

ouvec <- unique(ltst2$operatingunit)

oddlistp <- map(.x=ouvec, 
               .f = ~odd_oup(.x))

options(scipen = 999)

odd_dfp <- dplyr::bind_rows(oddlistp)

write.csv(odd_dfp, "pos_odds.csv")



odd_dfx <- odd_df %>% 
  mutate(tst_odds = paste(format(round(Est, 2), nsmall=2), "\n[",
                          format(round(LL,  2), nsmall=2), ", ",
                          format(round(UL,  2), nsmall=2), "]", 
                          sep="")) %>% 
  mutate(tst_pval = if_else(pvalu<0.0001, "<0.0001", 
                           paste(format(round(pvalu,3), nsmall=3), sep=""))) %>% 
  select(operatingunit, tst_odds, tst_pval)


odd_dfpx <- odd_dfp %>% 
  mutate(pos_odds = paste(format(round(Est, 2), nsmall=2), "\n[",
                          format(round(LL,  2), nsmall=2), ", ",
                          format(round(UL,  2), nsmall=2), "]",
                          sep="")) %>% 
  mutate(pos_pval = if_else(pvalu<0.0001, "<0.0001", 
                            paste(format(round(pvalu,3), nsmall=3), sep=""))) %>% 
  select(operatingunit, pos_odds, pos_pval)


oufinal <- left_join(oulevel, odd_dfx)

oufinal1 <- left_join(oufinal, odd_dfpx) %>% 
  select(operatingunit,
         stat_tst_2018, stat_tst_2019,
         tst_odds, tst_pval,
         stat_pos_2018, stat_pos_2019,
         pos_odds, pos_pval)


write.csv(oufinal1, "table_results_v3.csv")



# ==================================================


start_time <- Sys.time()

mx <- glmer(ind/denom~period+
             (1|operatingunit/psnuuid/orgunituid),
           weights=denom,
           data=ltst2,family="binomial")

end_time <- Sys.time()

memory.limit(size=10000000)

timediff <- end_time - start_time
# Time difference of 1.000327 mins


px <- coef(summary(mx))[,4]
pval <- px[2]
se <- sqrt(diag(vcov(mx)))

tab <- cbind(Est = fixef(mx), LL = fixef(mx) - 1.96 * se, UL = fixef(mx) + 1.96 *
               se)  

odf <- as.data.frame(exp(tab)) %>%  
  slice(2:2) %>% 
  mutate(pvalue = pval)

binom.exact(520, 1000, conf.level = 0.95)

