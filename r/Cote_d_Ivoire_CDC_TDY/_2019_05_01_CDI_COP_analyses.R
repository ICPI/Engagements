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

xlpath <- "RawData/COMPILE LTFU_08-may2019_Analysis_v2.xlsx"
xlpathold <- "LTFU_analysis_17APRdata_Clean_AN_IM.xlsx"


# Pulling from the dataset provided by the team
# From Lost-to-follow-up tab
ltfu <- read_excel(xlpath, range = "FICHE DE RECHERCHE PDV!A10:AM8190", 
                   guess_max = 7000)
ltfuold <- read_excel(xlpathold, range = "FICHE DE RECHERCHE PDV!A11:AV6974")

names(ltfu) <- tolower(names(ltfu))
names(ltfuold) <- tolower(names(ltfuold))

lfsitesold <- ltfuold %>% 
  filter(prioritysite %in% c("YES")) %>%
  # converting site names to lowercase for better matching
  mutate(sitex = str_replace_all(tolower(site), "[[:punct:]]", "")) %>% 
  select(sitex) %>% 
  filter(sitex %ni% c("dispensaire urbain de gagnoa"))

# Getting old site names
lfkeyold <- unique(lfsitesold$sitex)


retcare <- ltfu %>%
  select(site,
         rech_result, patient_revenu, statut30sept, statut31dec18,
         prioritysite) %>%
  filter(prioritysite %in% c("YES")) %>%
  # Calculating unexplained loss
  mutate(unexploss = if_else(statut30sept %in% c("LTFU", "ACTIVE") , 1, 0)) %>%
  # Calculating misclassification
    mutate(missclass = if_else(statut30sept %in% c("ACTIVE"), 1, 0)) %>%
  # Calculating the LTFU status variable
  mutate(ltfu_30sept = if_else(statut30sept %in% c("LTFU"), 1, 0)) %>%
  # Calculating the 'return to care' variable
  mutate(ret_to_care = if_else(ltfu_30sept==1 & patient_revenu %in% c("1"), 1, 0)) %>%
  # Calculating the 'unreachable' variable
  # if patient LTFU, with Resultat de la recherche = "(2) Injoignable", and return to care NO.
  mutate(unreachable = if_else(ltfu_30sept==1 & 
                                 rech_result %in% c("2") &
                                 patient_revenu %ni% c("1"),
                                 1, 0)) %>%
  # Getting variable promise to return 
  # if patient LTFU, with Resultat de la recherche = "(1a) RDV programmé", and return to care NO.
  mutate(promise_to_return = if_else(ltfu_30sept==1 & 
                                 rech_result %in% c("1a") &
                                 patient_revenu %ni% c("1"),
                               1, 0)) %>%
  # Refused to return
  # if patient LTFU, with Resultat de la recherche = "(1b) Refus RDV", and return to care NO.
  mutate(refused_to_return = if_else(ltfu_30sept==1 & 
                                       rech_result %in% c("1b") &
                                       patient_revenu %ni% c("1"),
                                     1, 0)) %>%
  # Deceased
  # if patient LTFU, with Resultat de la recherche = "(1b) Refus RDV", and return to care NO.
  mutate(dead = if_else(ltfu_30sept==1 & 
                         rech_result %in% c("3") &
                         patient_revenu %ni% c("1"),
                         1, 0)) %>%
  # Self-transfer
  # if patient LTFU, with Resultat de la recherche = "(4) Auto-transferé", and return to care NO.
  mutate(selftransfer = if_else(ltfu_30sept==1 & 
                                       rech_result %in% c("4") &
                                       patient_revenu %ni% c("1"),
                                     1, 0)) %>%
  # Ongoing search
  # The remaining patients who are LTFU
  rowwise() %>% 
  mutate(ongoing = max(0, ltfu_30sept - sum(ret_to_care,
                                            unreachable,
                                            promise_to_return,
                                            refused_to_return,
                                            dead,
                                            selftransfer, na.rm= T
                                            ))) %>% 
  ungroup() %>% 
  select(site, unexploss, 
         missclass, ltfu_30sept, 
         ret_to_care,
         unreachable,
         promise_to_return,
         refused_to_return,
         dead,
         selftransfer,
         ongoing) %>%
  group_by_if(is.character) %>%
  summarise_all(list(~sum), na.rm=T) %>%
  ungroup() %>%
  # converting site names to lowercase for better matching
  mutate(sitex = str_replace_all(tolower(site), "[[:punct:]]", "")) %>% 
  filter(sitex %in% lfkeyold)

setdiff(lfkeyold, unique(retcare$sitex))

# Pulling in data from the MSD
glist <- list.files(pattern="Genie")

# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file=glist, 
                col_names = TRUE, n_max = 0)   

foonames <- tolower(names(foo))

colvecx <- as.vector(ifelse(grepl("qtr|targets|cumulative", foonames), "d", "c"))

# colvecx <- as.vector(ifelse(foonames %in% c("FILL IN STUFF HERE"  ), "d", "c"))

colvec <- paste(colvecx, collapse = '')


# Pulling in the data with correct datatype for variables  
datim <- read_tsv(file=glist, 
                  col_names = TRUE,
                  col_types = colvec)      # ending if-else for Genie check

names(datim) <- tolower(names(datim))  


# Restructuring the DATIM dataset
df <- datim %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW") & 
           standardizeddisaggregate %in% c("Total Numerator") &
           fundingagency %in% c("HHS/CDC")) %>%
  # converting site names to lowercase for better matching
  mutate(sitex = str_replace_all(tolower(sitename), "[[:punct:]]", "")) %>%
  filter(sitex %in% c(lfkeyold, 
                      "centre integre de recherchesâ biocliniquesâ dâ€™abidjan cirba",
                      "dispensaire municipal de mâ€™pouto", 
                      "dispensaire rural de mâ€™badon"
  )) %>%
  # Filter for fiscal year FY2018
  filter(fiscal_year %in% c("2018")) %>% 
  select(orgunituid, sitename, sitex, indicator, cumulative) %>% 
  group_by_if(is.character) %>% 
  summarise_all(list(~sum), na.rm=T) %>% 
  ungroup() %>%
  spread(indicator, cumulative) 
  
  
dfnames <- unique(df$sitex)
residx <- setdiff(lfkeyold, dfnames) 


# merging the two datasets together
finalx <- left_join(retcare, df) %>% 
  mutate(net_gainloss = TX_NET_NEW - TX_NEW) %>% 
  # Calculating the OGAC retention proxy
  mutate(retprox = round((TX_NET_NEW/TX_NEW)*100,0)) %>% 
  mutate(retproxs = paste(retprox, "%", sep=""))  


final1 <- finalx %>%
  select(
         site             ,
         TX_CURR          ,
         TX_NET_NEW       ,
         TX_NEW           ,
         net_gainloss     ,
         retprox          ,
         retproxs         ,
         unexploss        ,
         missclass        ,
         ltfu_30sept      ,
         promise_to_return,
         ongoing          ,
         refused_to_return,
         ret_to_care      ,
         unreachable      ,
         dead             ,
         selftransfer
  )

  
write.csv(final1, "2019_05_10_ptoutcomes_v0.csv", na="", row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Normalizing data for site signatures analyses ~~~~~~~==============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Normalized view of partner's data across indicators for Q2
normx <- function(x){
  (x-min(x))/(max(x)-min(x))
}


top16 <- finalx %>% 
  arrange(desc(ltfu_30sept)) %>% 
  mutate(rankx = row_number()) %>% 
  filter(rankx <= 16)


pnormx <- top16 %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>% 
  mutate_if(is.numeric, list(~replace(., is.nan(.), 0))) %>%
  mutate(lefttofind = promise_to_return + ongoing) %>%
  mutate(net_loss = -net_gainloss) %>% 
  select(site, TX_CURR, retprox, net_loss, ltfu_30sept, lefttofind) %>%
  mutate_at(vars(
    "TX_CURR" ,
    "retprox" ,
    "net_loss", 
    "ltfu_30sept",
    "lefttofind"
  ), .funs = list(normx = ~normx(.))) %>% 
  # Gathering all numeric variables
  gather(varname, value, TX_CURR:lefttofind_normx) %>% 
  mutate(normed = if_else(grepl("normx", varname), "scaled", "unscaled")) %>% 
  # Remove the 'normx' postscript from the variable name
  mutate(varname = str_remove(varname, "_normx")) %>% 
  spread(normed, value) %>% 
  mutate_if(is.numeric, list(~replace(., is.na(.), 0))) %>% 
  mutate_if(is.numeric, list(~replace(., is.nan(.), 0))) %>% 
  # format the unscaled numbers appropriately
  mutate(unscaledx = if_else(varname %in% c("retprox"), 
                             paste(unscaled, "%", sep=""),
                                     paste(format(round(unscaled,0), big.mark=",", scientific=F),"", sep=""))) %>% 
  mutate(varnamex = case_when(
    varname %in% c("lefttofind")     ~  "Left to Find",
    varname %in% c("ltfu_30sept")    ~  "LTFU",
    varname %in% c("net_loss")       ~  "Net Loss",
    varname %in% c("retprox")        ~  "Retention Proxy",
    varname %in% c("TX_CURR")        ~  "TX_CURR"
  ))

# site names sorted by LTFU volume
sortsite <- top16 %>% select(site) %>% unique()



p <- ggplot(pnormx, aes(x=site, y=scaled, group=varnamex, 
                       text=paste(unscaledx, "<br>", site, "<br>", varnamex,  sep=""))) +
  geom_line(aes(color=varnamex)) +
  geom_point(aes(color=varnamex)) +
  scale_x_discrete(limits= as.vector(sortsite$site)) +
  theme(axis.text.x=element_text(size=1, angle=90)) +
  labs(x = "Sites") +
  labs(y = "Scaled from 0 to 1") +
  theme_light() +
  ggtitle("Site outcomes")+
  scale_color_manual(
                     values=c("#335B8E", 
                              "#6CA18F", 
                              "#B5B867",
                              "#CC5234",
                              "#D9812C"
                              ))+
  theme(legend.title=element_blank())



f2 <- list(
  family = "Old Standard TT, serif",
  size = 12,
  color = "black"
)

a <- list(
  showticklabels = TRUE,
  tickangle = 270,
  tickfont = f2
)

m <- list(
  l = 50,
  r = 50,
  b = 300,
  t = 50,
  pad = 4
)

ggplotly(p, tooltip = "text") %>% 
  layout(xaxis = a, margin = m)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b <- ggplot(pnormx, aes(x=varname, y=scaled, group=site, 
                        text=paste(unscaledx, "<br>", site, "<br>", varname,  sep=""))) +
  geom_line(aes(color=site)) +
  geom_point(aes(color=site)) +
  theme(axis.text.x=element_text(size=1, angle=90)) +
  labs(x = "Indicators") +
  labs(y = "Scaled from 0 to 1") +
  theme_light() +
  ggtitle("Site signatures")




f3 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)

bx <- list(
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f3
)

ggplotly(b, tooltip = "text") %>% 
  layout(xaxis = bx)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Sankey analyses  ~~~~~~~==============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# restructuring the data appropriately
ltfuprob <- finalx %>% 
  # Only keep required variables
  select(unreachable, 
         ret_to_care, 
         dead,
         refused_to_return,
         ongoing,
         promise_to_return,
         missclass,
         selftransfer
         ) %>% 
  gather(lev1, value) %>% 
  group_by(lev1) %>% 
  summarize(val = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(lev2 = case_when(
    lev1 %in% c("ret_to_care",
                "dead",
                "refused_to_return")   ~  "Resolved Search",
    lev1 %in% c("ongoing",
                "promise_to_return")   ~  "In Progress",
    TRUE                               ~  lev1
  )) %>% 
  mutate(lev3 = case_when(
    lev1 %in% c("unreachable") |
    lev2 %in% c("Resolved Search",
                "In Progress")   ~  "LTFU",
    lev1 %in% c("missclass",
                "selftransfer")   ~  "Data Quality/Reporting",
    TRUE                               ~  lev2
  )) %>% 
  mutate(lev4 = "Unexplained Loss") %>% 
  mutate(lev1 = if_else(lev1==lev2, NA_character_ , lev1)) %>% 
  select(lev4, lev3, lev2, lev1, val) 


# Creating the dataset for level 1 and 2 pairs
lev1_2 <- ltfuprob %>% 
  select(lev2, lev1, val) %>% 
  group_by_if(is.character) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  filter(!is.na(lev1) & !is.na(lev2)) %>% 
  group_by(lev2) %>% 
  mutate(lev1p = val/sum(val)) %>% 
  ungroup() %>% 
  mutate(lev1pr = paste(round(lev1p*100, 0), "%", sep="")) %>% 
  # Changing names to make easier
  mutate(lev1 = str_to_title(str_replace_all(lev1, "_", " "))) %>% 
  mutate(lev1l = paste(lev1, ": ",  format(round(val,0), big.mark=",", scientific=F),
                       " (", lev1pr, ")", sep="")) %>% 
  rename(target = lev1,
         source = lev2,
         valu   = val)


# for lev 2 to 3 dyads
lev2_3 <- ltfuprob %>% 
  select(lev3, lev2, val) %>% 
  group_by_if(is.character) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  filter(!is.na(lev2) & !is.na(lev3)) %>% 
  group_by(lev3) %>% 
  mutate(lev2p = val/sum(val)) %>% 
  ungroup() %>% 
  mutate(lev2pr = paste(round(lev2p*100, 0), "%", sep="")) %>% 
  # Changing names to make easier
  mutate(lev2 = str_to_title(str_replace_all(lev2, "_", " "))) %>% 
  mutate(lev2l = paste(lev2, ": ",  format(round(val,0), big.mark=",", scientific=F), 
                       " (", lev2pr, ")", sep="")) %>% 
  rename(target = lev2,
         source = lev3,
         valu   = val)

  
lev3_4 <- ltfuprob %>% 
  select(lev4, lev3, val) %>% 
  group_by_if(is.character) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  filter(!is.na(lev4) & !is.na(lev3)) %>% 
  group_by(lev4) %>% 
  mutate(lev3p = val/sum(val)) %>% 
  ungroup() %>% 
  mutate(lev3pr = paste(round(lev3p*100, 0), "%", sep="")) %>% 
  # Changing names to make easier
  mutate(lev3 = str_to_title(str_replace_all(lev3, "_", " "))) %>% 
  mutate(lev3l = paste(lev3, ": ",  format(round(val,0), big.mark=",", scientific=F),
                       " (", lev3pr, ")", sep="")) %>% 
  rename(target = lev3,
         source = lev4,
         valu   = val)

custom <- " (14% of TX_CURR)"

lev4 <- ltfuprob %>% 
  select(lev4, val) %>% 
  group_by_if(is.character) %>% 
  summarize_all(list(~sum), na.rm=T) %>% 
  ungroup() %>% 
  filter(!is.na(lev4)) %>% 
  group_by(lev4) %>% 
  mutate(lev4p = val/sum(val)) %>% 
  ungroup() %>% 
  mutate(lev4pr = paste(round(lev4p*100, 0), "%", sep="")) %>% 
  # Changing names to make easier
  mutate(lev4 = str_to_title(str_replace_all(lev4, "_", " "))) %>% 
  mutate(lev4l = if_else(lev4p==1, 
                         paste(lev4, custom, sep=""),
                         paste(lev4, ": ",  format(round(val,0), big.mark=",", scientific=F),
                               " (", lev4pr, ")", sep=""))) 

  
  
# Creating the node labels vector
nodex <-      c(lev4$lev4l, lev1_2$lev1l, lev2_3$lev2l, lev3_4$lev3l)
node_names <- c(lev4$lev4, lev1_2$target, lev2_3$target, lev3_4$target)


# Links dataset with dyads
links <- bind_rows(lev1_2, lev2_3, lev3_4) %>% 
  mutate(source = str_to_title(str_replace_all(source, "_", " "))) %>% 
  select(source, target, valu)


p <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ",.r",
  arrangement="freeform",
  
  node = list(
    label = nodex,
    color = c("#335B8E",  #0
              "#6CA18F",  #1 
              "#B5B867",  #2 
              "#CC5234",  #3
              "#D9812C",  #4
              "#67A9CF",  #5
              "#D1E5F0",  #6
              "#9CB084",  #7
              "#6585CF",  #8
              "#7E6BC9",  #9
              "#FDDBC7",  #10
              "cyan",     #11
              "gold" #12
    ),
    pad = 15,
    thickness = 40,
    line = list(
      color = "grey",
      width = 0.5
    )
  ),
  
  link = list(
    source = match(links$source, node_names) - 1,
    target = match(links$target, node_names) - 1,
    value =  links$valu)
  ) %>% 
  layout(
    title = "",
    font = list(
      size = 16
    )
  )





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in patient level data for Retention analyses ~~~~~~~========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



ptpath <- "RawData/Retard_14jrs_FY18Q2_MT.xlsx"


# Pulling from the dataset provided by the team to get headers
phead <- read_excel(ptpath, sheet="Table1", skip=1,
                 n_max = 0)

hname <- names(phead)
hcol <- if_else(grepl("Date", hname), "date", "guess")

# From Lost-to-follow-up tab
pt <- read_excel(ptpath, sheet="Table1", skip=1, 
                   guess_max = 50000)

nms <- names(pt)
nms <- stringr::str_replace_all(nms,"\n","_")
nms <- stringr::str_replace_all(nms,"\r","_")
nms <- stringr::str_replace_all(nms," ","_")
nms <- stringr::str_replace_all(nms,":","_")
nms <- stringr::str_replace_all(nms,"#","num")
nms <- stringr::str_replace_all(nms,"%","perc")
nms <- stringr::str_replace_all(nms,"\\+","")
nms <- stringr::str_replace_all(nms,">","gt")
nms <- stringr::str_replace_all(nms,"<","lt")
nms <- stringr::str_replace_all(nms,">","gt")
nms <- stringr::str_replace_all(nms,"-","_")

names(pt) <- tolower(nms) 



















# Mapping this out!! yay!!
# Pulling in the shape files and coordinate data
mappath <- "C:/Temp_work/CDI_clustering/CIV_clustering_proj_20180511/CIV_DATIM"
mappath2 <- "C:/Temp_work/CDI_clustering/CIV_clustering_proj_20180511/SANTE_2012"



dshp <- readOGR(dsn   = mappath, 
                layer = 'CotedIvoireHealthDistrictsLsib2016Dec')

rshp <- readOGR(dsn   = mappath2, 
                layer = 'CL_DISTRICT_SANITAIRE')


# change CRS to 
rshp <- spTransform(rshp,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  

# Getting the coordinate data
coords <- read.csv("C:/Temp_work/CDI_clustering/CIV_clustering_proj_20180511/CoteDIvoire_Facilities_11142018.csv", 
                   header = T)


# filter for only sites in dataset 
coords1 <- coords %>%  select(uid, Latitude, Longitude, level4, level5, level6) %>% 
  filter(uid %in% unique(finalx$orgunituid))


# Merge onto dataset


