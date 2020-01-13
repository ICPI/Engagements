
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================ Script for SID analyses =========================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}


# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 

# Pulling in the load package function R file
# Load function to install list of packages
ldpkg <- dget("ldpkg.R")

# Running the ldpkg function which checks if package is installed 
# and then loads it if it is, otherwise, installs and then loads
ldpkg(c("tidyverse", "eply", "readxl", "plotly", 
        "scales", "lme4", "corrplot"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%X"))
tm <- str_replace_all(t, "[: ]", "_")
dt <- str_replace_all(dx, "[ ]", "")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in OU-IM dataset ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for pulling in zipped or unzipped files
# ld_dfile <- function(f, p){
# 
#   filelisto <- list.files(f, pattern=p)
#   
#   gfile_loco <- filelisto[1]
  
xlfolder <- "C:/temp_folder/SID_analyses/RawData"

filelisto <- list.files(xlfolder, pattern="Genie|genie|MER")

gfile_loco <- filelisto[1]

# Pulling the dataset with the correct column types
xfileo <- unzip(paste(xlfolder, "/", gfile_loco, sep=""), 
                list = TRUE) %>% .$Name

# Rough data pull to get variable names and assign datatype
fooo <- read_tsv(file=unz(paste(xlfolder, "/", gfile_loco, sep=""), 
                          xfileo), 
                 col_names = TRUE, 
                 n_max = 0) 

close(unz(paste(xlfolder, "/", gfile_loco, sep=""), 
          xfileo))

foonameso <- tolower(names(fooo))


# Creating the vector of column types
colvecxo <- as.vector(ifelse(grepl("qtr|targets|cumulative", foonameso), "d", "c"))

colveco <- paste(colvecxo, collapse = '')


# Pulling the dataset with the correct column types
datimo <- read_tsv(file=unz(paste(xlfolder, "/", gfile_loco, sep=""), 
                            xfileo), 
                   col_names = TRUE,
                   col_types = colveco)
close(unz(paste(xlfolder, "/", gfile_loco, sep=""), 
          xfileo))

names(datimo) <- tolower(names(datimo))


# ============= Pulling in UNAIDS epi data ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
natpath <- list.files(pattern="plhiv")
natcols <- c("text", "numeric", "numeric")

nat_tx <- read_xlsx(path=natpath, sheet = "unaids_tx", col_names = TRUE, 
                col_types = natcols,
                na = "", trim_ws = TRUE, 
                progress = readxl_progress(), .name_repair = "unique")


nat_plhiv <- read_xlsx(path=natpath, sheet = "unaids_plhiv", col_names = TRUE, 
                    col_types = natcols,
                    na = "", trim_ws = TRUE, 
                    progress = readxl_progress(), .name_repair = "unique")


nat_tx1 <- nat_tx %>% 
  mutate(indicator = "art")

nat_plhiv1 <- nat_plhiv %>% 
  mutate(indicator = "plhiv")


nat <- bind_rows(nat_tx1, nat_plhiv1) %>% 
  gather(period, valu, "2017", "2019") %>% 
  spread(indicator, valu)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the SID score data ~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pulling in the SID scores Excel files 
# xlfolder <- "../RawData/SID"

subfolder <- "SID"

sidfolder <- paste(xlfolder, "/", subfolder, sep="")

flist <- list.files(sidfolder)

xlcols <- c("text", rep("numeric",3))
xlcolsx <- c("text", rep("numeric",2))



# OperatingUnit SID buckets
reboot <- c(
  "Angola",
  "Botswana",
  "Cameroon",
  "Cote D'Ivoire",
  "Cote d'Ivoire",
  "Haiti",
  "Dominican Republic",
  "Mozambique",
  "South Africa",
  "Tanzania"
  )

scalew <- c(
  "Congo, Dem. Rep.",
  "Democratic Republic of the Congo",
  "Lesotho",
  "Malawi",
  "Nigeria",
  "South Sudan",
  "Uganda",
  "Ukraine",
  "Vietnam",
  "Zambia"
)

evolve <- c(
  "Ethiopia",
  "Burundi",
  "Eswatini",
  "Swaziland",
  "Kenya",
  "Namibia",
  "Rwanda",
  "Zimbabwe"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function for pulling in SID binary questions ~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# setting up the column types
qcols <- c("text", "numeric", "text")

qnames <- c("question",  
            "score",
            "ou")

# function that pulls the data from Excel files
q_pop <- function(y){
  
  # path to the excel file  
  xlpath <- paste(sidfolder, "/", y, sep="")
  
  # Pulling the dataset to get OU name
  xl <- read_xlsx(path=xlpath, sheet = "Question Raw Scores", col_names = TRUE, 
                  col_types = qcols,
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  
  # Getting the OU name
  ou <- names(xl)[3]
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= modification of indicator data ~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Modify the dataset structure
  q1 <- xl %>%
    select(-ou) %>% 
    mutate(operatingunit = ou)
  
  
  q1 }


qlist <- map(.x = flist, .f = ~q_pop(.x))

q_all19 <- bind_rows(qlist)


# Assigning period
q_all19x <- q_all19 %>% 
  mutate(bucket = case_when(
    operatingunit %in% reboot ~ "Reboot",
    operatingunit %in% scalew ~ "Scale w/ Fidelity",
    operatingunit %in% evolve ~ "Evolve to Sustain",
    operatingunit %in% reboot ~ "Regional",
    TRUE ~ "Other")) %>% 
  mutate(period="2019")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the 2017 binary questions ~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subfolder <- "SID_2017"

sidfolder <- paste(xlfolder, "/", subfolder, sep="")

flist17 <- list.files(sidfolder)

qlist17 <- map(.x = flist17, .f = ~q_pop(.x))

q_all17 <- bind_rows(qlist17)


# Assigning period
q_all17x <- q_all17 %>% 
  mutate(bucket = case_when(
    operatingunit %in% reboot ~ "Reboot",
    operatingunit %in% scalew ~ "Scale w/ Fidelity",
    operatingunit %in% evolve ~ "Evolve to Sustain",
    operatingunit %in% reboot ~ "Regional",
    TRUE ~ "Other")) %>% 
  mutate(period="2017")


qall <- bind_rows(q_all19x, q_all17x) %>% 
  filter(Question %ni% c("2.2999999999999998")) %>% 
  mutate(operatingunit = if_else( 
    operatingunit %in% c("Swaziland"), "Eswatini", operatingunit)) %>% 
  mutate(q_id = substr(Question, 0,3)) %>%
  mutate(q_idx = case_when(
    period %in% c("2017") & q_id %in% c("6.7") ~ "_6.8",
    period %in% c("2017") & q_id %in% c("6.8") ~ "_6.9",
    TRUE   ~        paste("_", q_id, sep=""))) %>% 
  filter(q_idx %in% c("_2.2", "_6.8", "_6.9")) %>% 
  mutate(colvar = paste("q", str_replace_all(q_idx, "\\.", "_"), sep="")) %>% 
  rename(score = "Raw Score") %>% 
  select(operatingunit, bucket, period, colvar, score) %>% 
  spread(colvar, score) %>% 
  mutate(Country = if_else(
    operatingunit %in% c("Congo, Dem. Rep."), "Democratic Republic of the Congo",
    if_else(
      operatingunit %in% c("Swaziland"), "Eswatini", 
      if_else(
        operatingunit %in% c("Cote D'Ivoire"), "Cote d'Ivoire", operatingunit 
      ))))
  

# comparing the country names between the SID and NAT data
natvec <- unique(nat$Country)
sidvec <- unique(qall$Country)

setdiff(sidvec, natvec)

# Change names of Lao
nat$Country <- if_else(nat$Country=="Lao People's Democratic Republic", "Laos", 
                if_else(nat$Country=="United Republic of Tanzania", "Tanzania",
                 if_else(nat$Country=="Myanmar", "Burma",
                  if_else(nat$Country=="Viet Nam", "Vietnam",
                        nat$Country))))
  
  
countrys <- c(
"Angola",
"Botswana",
"Burma",
"Burundi",
"Cambodia",
"Cameroon",
"Democratic Republic of the Congo",
"Cote d'Ivoire",
"Dominican Republic",
"El Salvador",
"Eswatini",
"Swaziland",
"Ethiopia",
"Ghana",
"Guatemala",
"Haiti",
"Honduras",
"India",
"Indonesia",
"Jamaica",
"Kazakhstan",
"Kenya",
"Laos",
"Lesotho",
"Malawi",
"Mozambique",
"Namibia",
"Nicaragua",
"Nigeria",
"Panama",
"Papua New Guinea",
"Rwanda",
"South Africa",
"South Sudan",
"Eswatini",
"Tanzania",
"Thailand",
"Uganda",
"Ukraine",
"Vietnam",
"Zambia",
"Zimbabwe"
)


sid_nat <- left_join(nat, qall) %>% 
  select(-operatingunit) %>% 
  mutate(art_cov = (art/plhiv)*100)
  

sid_natx <- sid_nat %>% 
  filter(Country %in% c(
    "Botswana",
    "Cameroon",
    "Cote d'Ivoire",
    "Democratic Republic of the Congo",
    "Eswatini",
    "Ethiopia",
    "Haiti",
    "Kenya",
    "Lesotho",
    "Malawi",
    "Mozambique",
    "Namibia",
    "Nigeria",
    "Rwanda",
    "South Africa",
    "South Sudan",
    "Tanzania",
    "Uganda",
    "Ukraine",
    "Zambia",
    "Zimbabwe"
  )) %>% 
  mutate(art   = as.integer(art),
         plhiv = as.integer(plhiv))


options(scipen = 999)


  m <- glmer(art/plhiv~as.factor(period)*q_2_2+
               (1|Country),
             # weights=plhiv,
             data=sid_natx,family="binomial")
  
  px <- as.data.frame(coef(summary(m)))
  
  pval <- px %>% select("Pr(>|z|)") %>% 
    mutate(factorx = row.names(px))
  
  se <- sqrt(diag(vcov(m)))
  
  tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                 se)  
  
  odf <- as.data.frame(exp(tab)) %>% 
    mutate(factorx = row.names(tab))
  
  odfx <- left_join(odf, pval)
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Next score
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  m <- glmer(art/plhiv~as.factor(period)*q_6_8+
               (1|Country),
             # weights=plhiv,
             data=sid_natx,family="binomial")
  
  px <- as.data.frame(coef(summary(m)))
  
  pval <- px %>% select("Pr(>|z|)") %>% 
    mutate(factorx = row.names(px))
  
  se <- sqrt(diag(vcov(m)))
  
  tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                 se)  
  
  odf <- as.data.frame(exp(tab)) %>% 
    mutate(factorx = row.names(tab))
  
  odfx <- left_join(odf, pval)
  
    
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Next score
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  m <- glmer(art/plhiv~as.factor(period)*q_6_9+
               (1|Country),
             # weights=plhiv,
             data=sid_natx,family="binomial")
  
  px <- as.data.frame(coef(summary(m)))
  
  pval <- px %>% select("Pr(>|z|)") %>% 
    mutate(factorx = row.names(px))
  
  se <- sqrt(diag(vcov(m)))
  
  tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                 se)  
  
  odf <- as.data.frame(exp(tab)) %>% 
    mutate(factorx = row.names(tab))
  
  odfx <- left_join(odf, pval)
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Next score for things
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
dfx <- sid_natx %>%
  select(Country, period, 
         art, plhiv, art_cov, 
         q_2_2,   
         q_6_8,   
         q_6_9
         ) %>% 
  gather(colvar, vals,
         art, plhiv, art_cov, 
         q_2_2,   
         q_6_8,   
         q_6_9) %>% 
  # Keeping scores from 2017 and ART coverage from 2018
  mutate(keeprow = case_when(
    period %in% c("2017") & colvar %in% c("q_2_2",
                                          "q_6_8",
                                          "q_6_9") ~ "Y",
    period %in% c("2019") & colvar %in% c("art", 
                                          "plhiv", 
                                          "art_cov" 
                                          ) ~ "Y",
    TRUE ~ "N"
  )) %>% 
  filter(keeprow %in% c("Y")) %>% 
  select(-period) %>% 
  spread(colvar, vals)
  
  
  vls_m <- glm(art/plhiv~q_2_2,
               data=dfx,family="quasibinomial")
  
  px <- coef(summary(vls_m))[,4]
  pval <- px[2]
  se <- sqrt(diag(vcov(vls_m)))
  
  tab <- exp(cbind(coef(vls_m), confint(vls_m)))
  
  o_q_2_2 <- as.data.frame(tab) %>%  
    slice(2:2) %>% 
    mutate(pvalue = pval) %>% 
    mutate(score = "q_2_2")
  
  
  vls_m <- glm(art/plhiv~q_6_8,
               data=dfx,family="quasibinomial")
  
  px <- coef(summary(vls_m))[,4]
  pval <- px[2]
  se <- sqrt(diag(vcov(vls_m)))
  
  tab <- exp(cbind(coef(vls_m), confint(vls_m)))
  
  o_q_6_8 <- as.data.frame(tab) %>%  
    slice(2:2) %>% 
    mutate(pvalue = pval) %>% 
    mutate(score = 'q_6_8')
  

  vls_m <- glm(art/plhiv~q_6_9,
               data=dfx,family="quasibinomial")
  
  px <- coef(summary(vls_m))[,4]
  pval <- px[2]
  se <- sqrt(diag(vcov(vls_m)))
  
  tab <- exp(cbind(coef(vls_m), confint(vls_m)))
  
  o_q_6_9 <- as.data.frame(tab) %>%  
    slice(2:2) %>% 
    mutate(pvalue = pval) %>% 
    mutate(score = 'q_6_9')
  
  
o_final <- bind_rows(o_q_2_2, o_q_6_8, o_q_6_9)  

write.csv(o_final, "odds_2018artcov_2017scores.csv")


dfviz <- sid_natx %>%
  select(Country, period, 
         art_cov, 
         q_2_2,   
         q_6_8,   
         q_6_9
  ) %>%
  gather(svars, vals,
         art_cov, 
         q_2_2,   
         q_6_8,   
         q_6_9) %>% 
  mutate(colvar = paste(svars, period, sep="_")) %>% 
  select(-period, -svars) %>% 
  spread(colvar, vals) %>% 
  mutate(art_cov_diff = art_cov_2019 - art_cov_2017) %>% 
  mutate(q_2_2_diff = q_2_2_2019 - q_2_2_2017) %>% 
  mutate(q_6_8_diff = q_6_8_2019 - q_6_8_2017) %>% 
  mutate(q_6_9_diff = q_6_9_2019 - q_6_9_2017)




cor(dfviz$art_cov_diff, dfviz$q_2_2_diff,
    method = "kendall")

cor.test(dfviz$art_cov_diff, dfviz$q_2_2_diff,
    method = "spearman", exact=FALSE)

p_2_2 <- ggplot(data=dfviz, aes(x=q_2_2_diff, y=art_cov_diff)) + 
         geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("Difference in scores 2017 to 2019") + ylab("Change in ART coverage 2017 to 2018") +
  theme_bw() +
  ggtitle("Question 2.2, rho=0.08, p-value=0.72")



cor(dfviz$art_cov_diff, dfviz$q_6_8_diff, use = "complete.obs",
    method = "kendall")
cor.test(dfviz$art_cov_diff, dfviz$q_6_8_diff,
         method = "spearman", exact=FALSE)


p_6_8 <- ggplot(data=dfviz, aes(x=q_6_8_diff, y=art_cov_diff)) + 
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("Difference in scores 2017 to 2019") + ylab("Change in ART coverage 2017 to 2018") +
  theme_bw() +
  ggtitle("Question 6.8, rho=0.19, p-value=0.42")


cor(dfviz$art_cov_diff, dfviz$q_6_9_diff, use = "complete.obs",
    method = "pearson")
cor.test(dfviz$art_cov_diff, dfviz$q_6_9_diff,
         method = "spearman", exact=FALSE)


p_6_9 <- ggplot(data=dfviz, aes(x=q_6_9_diff, y=art_cov_diff)) + 
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab("Difference in scores 2017 to 2019") + ylab("Change in ART coverage 2017 to 2018") +
  theme_bw() +
  ggtitle("Question 6.9, rho=0.55, p-value=0.011*")


ggarrange(p_2_2, p_6_8, p_6_9, ncol = 3, nrow=1, margin = 0.04) 



subplot(ggplotly(p_2_2), 
        ggplotly(p_6_8), 
        ggplotly(p_6_9))



  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Analyses for IAS abstract ~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

link_color <- c("#335B8E","#6CA18F","#CC5234")

link_viz <- ggplot(data=sid_merxx, aes(x=reorder(countryname, -link_diff), 
                                 y=link_diff, 
                                 fill=link,
                                 label=paste(round(link_diff,0), 
                                             "%\n(",
                                             round(link_2017,0), " to ",
                                             round(link_2019,0), ")",
                                             sep=""))) + 
  geom_bar(stat="identity") +
  geom_text(color="black",check_overlap = TRUE, size=3)+
  # scale_color_manual(values=color_group) +
  scale_fill_manual(values=alpha(link_color, 0.8)) +
  xlab("Country") + ylab("Change in linkage") +
  scale_y_continuous(limits = c(-50, 50), oob=squish) +  
  labs(fill = "Policy implemented in 2017 and 2019", color="Response")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(questvec[1])



tx_color <- c("#335B8E","#B2182B", "#6CA18F","#CC5234")

tx_viz <- ggplot(data=sid_merxx, aes(x=reorder(countryname, -tx_growth), 
                                      y=tx_growth, 
                                      fill=ret
                                      )) + 
  geom_bar(stat="identity") +
  geom_text(label=paste(round(sid_merxx$tx_growth,0),"%",
                        sep=""),
            color="black",check_overlap = TRUE, size=3.5) +
  geom_text(label=paste("", 
                        "\n\n\n\n(",
                        prettyNum(sid_merxx$TX_CURR_2017,big.mark=","), 
                        "\nto\n",
                        prettyNum(sid_merxx$TX_CURR_2019,big.mark=","), 
                        ")",
                        sep=""),
            color="black",check_overlap = F, size=2.3) +
  scale_fill_manual(values=alpha(tx_color, 0.8)) +
  xlab("Country") + ylab("TX_CURR growth") +
  scale_y_continuous(limits = c(-3, NA), oob=squish) +
  labs(fill = "Policy implemented", color="Response")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(questvec[2])



adol_tst_viz <- ggplot(data=sid_merxx, aes(x=reorder(countryname,-adol_tst_growth), 
                                     y=adol_tst_growth, 
                                     fill=adol_tst)) + 
  geom_bar(stat="identity") +
  geom_text(label=paste(round(sid_merxx$adol_tst_growth,0),"%",
                        sep=""),
            color="black",check_overlap = TRUE, size=3.5) +
  geom_text(label=paste("", 
                        "\n\n\n\n(",
                        prettyNum(sid_merxx$adol_HTS_TST_POS_2017,big.mark=","), 
                        "\nto\n",
                        prettyNum(sid_merxx$adol_HTS_TST_POS_2019,big.mark=","), 
                        ")",
                        sep=""),
            color="black",check_overlap = F, size=2.3) +
  scale_fill_manual(values=alpha(tx_color, 0.8)) +
  xlab("Country") + ylab("Testing growth in adolescents") +
  scale_y_continuous(limits = c(-100, 100), oob=squish) +
  labs(fill = "Policy implemented", color="Response")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(questvec[3])


adol_tx_viz <- ggplot(data=sid_merxx, aes(x=reorder(countryname, -adol_tx_growth), 
                                           y=adol_tx_growth, 
                                           fill=adol_tx)) + 
  geom_bar(stat="identity") +
  geom_text(label=paste(round(sid_merxx$adol_tx_growth,0),"%",
                        sep=""),
            color="black",check_overlap = TRUE, size=3.5) +
  geom_text(label=paste("", 
                        "\n\n\n\n(",
                        prettyNum(sid_merxx$adol_TX_CURR_2017,big.mark=","), 
                        "\nto\n",
                        prettyNum(sid_merxx$adol_TX_CURR_2019,big.mark=","), 
                        ")",
                        sep=""),
            color="black",check_overlap = F, size=2.3) +
  scale_fill_manual(values=alpha(tx_color, 0.8)) +
  xlab("Country") + ylab("TX_CURR growth in adolescents") +
  scale_y_continuous(limits = c(-20, 100), oob=squish) +
  labs(fill = "Policy implemented", color="Response")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(questvec[4])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function for pulling in Responsibility Matrix ~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rsubfolder <- "Resp_matrix"

rfolder <- paste(xlfolder, "/", rsubfolder, sep="")

rlist <- list.files(rfolder)

# setting up the column types
rcols <- c("text")

rnames <- c("elements",
            "serv_hostgovt",
            "serv_pepfar",
            "serv_gfund",
            "blah1",
            "nonserv_hostgovt",
            "nonserv_pepfar",
            "nonserv_gfund",
            "blah2",
            "strat_hostgovt",
            "strat_pepfar",
            "strat_gfund")

major_cats <- c("Programs",
                "Commodities",
                "Health Workforce",
                "Above Site (Systems)",
                "Program Implementation, Management, and Support")

minor_cats <- c(
  "Care and Treatment",
  "HIV Testing Services",
  "Prevention",
  "Orphans and Vulnerable Children",
  "Service Delivery Personnel: Facility-level staff total",
  "Non-Service Delivery Personnel: Facility-level staff total",
  "Community Health Workers/Lay Cadres staff number",
  "Secondment Staff number",
  "Condoms",
  "Male Circumcision Kits and Supplies",
  "Rapid Test Kits",
  "Antiretroviral Drugs",
  "Other Essential Drugs",
  "CD4",
  "Viral Load",
  "Reagents and Supplies",
  "Health Equipment",
  "PSM Costs",
  "Health Workforce",
  "Governance",
  "Institutional and Organizational Development",
  "Health Financing",
  "Health Management Information Systems",
  "Supply Chain Systems",
  "Laboratory Systems",
  "Other Systems Support",
  "Disease Surveillance",
  "Monitoring and Evaluation",
  "Surveys and Surveillance",
  "Research and Other Surveys",
  "At the Implementation Level",
  "At the Donor Level"
)


respcat <- c("serv_hostgovt"  ,  "serv_pepfar"     ,
             "serv_gfund"     ,  "nonserv_hostgovt",
             "nonserv_pepfar" ,  "nonserv_gfund"   ,
             "strat_hostgovt" ,  "strat_pepfar"    ,
             "strat_gfund")

# function that pulls the data from Excel files
r_pop <- function(z){
  
  # path to the excel file  
  rpath <- paste(rfolder, "/", z, sep="")
  
  # Pulling the dataset to get OU name
  rx <- read_xlsx(path=rpath, sheet = 1, range="B9:M65",
                  col_names = rnames, 
                  col_types = "text",
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  
  rmeta <- read_xlsx(path=rpath, sheet = 1, range="B3:B6",
                  col_names = "rmatrix", 
                  col_types = "text",
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  


  rxx <- rx %>% slice(2:nrow(rx)) %>% 
    mutate(major_catx = if_else(elements %in% major_cats,
                                elements, NA_character_)) %>% 
    mutate(xmajor_catx = if_else(!is.na(major_catx), 1, 0)) %>%
    fill(major_catx) %>% 
    filter(xmajor_catx==0) %>% 
    select(-c(xmajor_catx, blah1, blah2)) %>% 
    mutate(minor_catx = if_else(elements %in% minor_cats,
                                elements, NA_character_)) %>% 
    fill(minor_catx) %>% 
    filter(!is.na(elements)) %>% 
    gather(resp_catx, resp3level, respcat)
  
 
  # Getting the OU name
  rmeta1 <- rmeta %>% 
    mutate(vals = trimws(sub(".*:", "", rmatrix), 
                         which = c("both"), 
                         whitespace = "[ \t\r\n]")) %>% 
    mutate(colvars = gsub("[[:space:]]", "", sub(":.*", "", rmatrix))) %>% 
    select(colvars, vals) %>% 
    spread(colvars, vals)
  
  # Adding metadata to the dataset
  rfinal <- merge(x = rxx, y = rmeta1, by = NULL) %>% 
    rename(operatingunit = Country) %>% 
    select(operatingunit, EpidemicType, IncomeLevel, PEPFARCategorization,
           elements:resp3level)
  
  # merging definitions back
  rfinal }


rlistx <- map(.x = rlist, .f = ~r_pop(.x))

r_all <- bind_rows(rlistx)


r_all1 <- r_all %>% 
  mutate(bucket = case_when(
    operatingunit %in% reboot ~ "Reboot",
    operatingunit %in% scalew ~ "Scale w/ Fidelity",
    operatingunit %in% evolve ~ "Evolve to Sustain",
    operatingunit %in% reboot ~ "Regional",
    TRUE ~ "Other"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the SID raw scores for CSE data ~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


subfolder <- "SID"

sidfolder <- paste(xlfolder, "/", subfolder, sep="")

flist <- list.files(sidfolder)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function for pulling in SID raw final scores ~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that pulls the data from Excel files
cse_pop <- function(x){
  
  # path to the excel file  
  xlpath <- paste(sidfolder, "/", x, sep="")
  
  # Pulling the dataset from the excel file
  xl <- read_xlsx(path=xlpath, sheet = "Raw Element Data", col_names = TRUE, 
                  col_types = xlcols,
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  
  # Getting the OU name
  ou <- names(xl)[1]
  
  cse <- read_xlsx(path=xlpath, sheet = 4, col_names = "scorex", 
                  col_types = "numeric", range = "J162:J189",
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  
  scorevec <- cse$scorex[c(1,4,16,23,28)]
  scnamvec <- c("3.1",
                "3.2",
                "3.3",
                "3.4",
                "3.5")
  
  df <- as.data.frame(cbind(scnamvec,scorevec)) %>% 
    mutate(operatingunit = ou)
  
  df }


cselist <- map(.x = flist, .f = ~cse_pop(.x))

cse_all <- bind_rows(cselist)

# adding the OU categories
unique(cse_all$operatingunit)

# OperatingUnit SID buckets
reboot <- c(
  "Angola",
  "Botswana",
  "Cameroon",
  "Cote D'Ivoire",
  "Cote d'Ivoire",
  "Haiti",
  "Dominican Republic",
  "Mozambique",
  "South Africa",
  "Tanzania"
)

scalew <- c(
  "Congo, Dem. Rep.",
  "Democratic Republic of the Congo",
  "Lesotho",
  "Malawi",
  "Nigeria",
  "South Sudan",
  "Uganda",
  "Ukraine",
  "Vietnam",
  "Zambia"
)

evolve <- c(
  "Ethiopia",
  "Burundi",
  "Eswatini",
  "Kenya",
  "Namibia",
  "Rwanda",
  "Zimbabwe"
)

csedef <- c("3.1 Civil Society and Accountability",
            "3.2 Government Channels and Opportunities",
            "3.3 Impact of Civil Society Engagement",
            "3.4 Domestic Funding of Civil Society",
            "3.5 Civil Society Enabling Environment")



cse1 <- cse_all %>% 
  mutate(bucket = case_when(
    operatingunit %in% reboot ~ "Reboot",
    operatingunit %in% scalew ~ "Scale w/ Fidelity",
    operatingunit %in% evolve ~ "Evolve to Sustain",
    operatingunit %in% reboot ~ "Regional",
    TRUE ~ "Other"
  )) %>% 
  mutate(period="2019") %>% 
  mutate(subsubcat = case_when(
    scnamvec %in% "3.1"  ~ as.character(csedef[1]),
    scnamvec %in% "3.2"  ~ as.character(csedef[2]),
    scnamvec %in% "3.3"  ~ as.character(csedef[3]),
    scnamvec %in% "3.4"  ~ as.character(csedef[4]),
    scnamvec %in% "3.5"  ~ as.character(csedef[5])
  ))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the SID raw scores for CSE data ~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


subfolder <- "SID_2017"

sidfolder <- paste(xlfolder, "/", subfolder, sep="")

flist <- list.files(sidfolder)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Function for pulling in SID raw final scores ~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that pulls the data from Excel files
csex_pop <- function(x){
  
  # path to the excel file  
  xlpath <- paste(sidfolder, "/", x, sep="")
  
  # Pulling the dataset from the excel file
  xl <- read_xlsx(path=xlpath, sheet = "Raw Element Data", col_names = TRUE, 
                  col_types = xlcolsx,
                  na = "", trim_ws = TRUE, 
                  progress = readxl_progress(), .name_repair = "unique")
  
  # Getting the OU name
  ou <- names(xl)[1]
  
  cse <- read_xlsx(path=xlpath, sheet = 3, col_names = "scorex", 
                   col_types = "numeric", range = "J154:J181",
                   na = "", trim_ws = TRUE, 
                   progress = readxl_progress(), .name_repair = "unique")
  
  scorevec <- cse$scorex[c(1,4,16,23,28)]
  scnamvec <- c("3.1",
                "3.2",
                "3.3",
                "3.4",
                "3.5")
  
  df <- as.data.frame(cbind(scnamvec,scorevec)) %>% 
    mutate(operatingunit = ou)
  
  df }


csexlist <- map(.x = flist, .f = ~csex_pop(.x))

csex_all <- bind_rows(csexlist)

# adding the OU categories
unique(cse_all$operatingunit)

# OperatingUnit SID buckets
reboot <- c(
  "Angola",
  "Botswana",
  "Cameroon",
  "Cote D'Ivoire",
  "Cote d'Ivoire",
  "Haiti",
  "Dominican Republic",
  "Mozambique",
  "South Africa",
  "Tanzania"
)

scalew <- c(
  "Congo, Dem. Rep.",
  "Democratic Republic of the Congo",
  "Congo, Dem. Rep.",
  "Lesotho",
  "Malawi",
  "Nigeria",
  "South Sudan",
  "Uganda",
  "Ukraine",
  "Vietnam",
  "Zambia"
)

evolve <- c(
  "Ethiopia",
  "Burundi",
  "Eswatini",
  "Kenya",
  "Namibia",
  "Rwanda",
  "Zimbabwe"
)


csex1 <- csex_all %>% 
  mutate(bucket = case_when(
    operatingunit %in% reboot ~ "Reboot",
    operatingunit %in% scalew ~ "Scale w/ Fidelity",
    operatingunit %in% evolve ~ "Evolve to Sustain",
    operatingunit %in% reboot ~ "Regional",
    TRUE ~ "Other"
  )) %>% 
  mutate(period="2017") %>% 
  mutate(subsubcat = case_when(
    scnamvec %in% "3.1"  ~ as.character(csedef[1]),
    scnamvec %in% "3.2"  ~ as.character(csedef[2]),
    scnamvec %in% "3.3"  ~ as.character(csedef[3]),
    scnamvec %in% "3.4"  ~ as.character(csedef[4]),
    scnamvec %in% "3.5"  ~ as.character(csedef[5])
  ))


# Stacking all together
csefinal <- bind_rows(cse1, csex1) %>% 
  mutate(score = as.numeric(scorevec))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Exporting as Excel data ~~~~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exporting out the SID dataset
write.csv(sid1, paste("C:/temp_folder/SID_analyses/Output/",
                      "SIDdata_", dt, "_", tm, ".csv", sep=""), 
          na="", row.names = F)

# Exporting out the SID questions dataset
write.csv(q_all1, paste("C:/temp_folder/SID_analyses/Output/",
                      "SIDquestions_", dt, "_", tm, ".csv", sep=""), 
          na="", row.names = F)


# Exporting out the Responsibility Matrix dataset
write.csv(r_all1, paste("C:/temp_folder/SID_analyses/Output/",
                        "ResponsibilityMatrix_", dt, "_", tm, ".csv", sep=""), 
          na="", row.names = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Visualization of SID data ~~~~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Research Questions # 1 ~~~~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flowerPlotWrap <- function(dat, sl, sw, pl, sp){
  ggplot(data=dat, aes_string(x=sl, y=sw, color=pl)) + 
    geom_point() +facet_wrap(as.formula(sp)) # note the as.formula
}
pl.flower3 <- flowerPlotWrap(iris, sl='Sepal.Length', 
                             sw='Sepal.Width', pl='Petal.Length', 
                             sp= '~Species')
# ============= For the usual OUs by bucket ~~~~~~~~~~============

dfb_maj <- sid1 %>% 
  # filter out the 'Other' group
  filter(bucket %ni% c("Other")) %>% 
  group_by(period, major_cat, bucket) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))

# dfo_maj <- sid1 %>% 
#   # filter out the 'Other' group
#   filter(bucket %ni% c("Other")) %>% 
#   group_by(period, major_cat, operatingunit, bucket) %>% 
#   summarize(value = mean(value, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(!is.na(value)) %>% 
#   mutate(period = substr(period, 5, 9))

dfb_sub <- sid1 %>%
  # filter out the 'Other' group
  filter(bucket %ni% c("Other")) %>% 
  filter(major_cat %in% "Governance, Leadership, and Accountability") %>% 
  group_by(period, subcatx , bucket) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))


dfb_subx <- csefinal %>%
  # filter out the 'Other' group
  filter(bucket %ni% c("Other")) %>%  
  group_by(period, subsubcat , bucket) %>% 
  summarize(score = mean(score, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(score)) 



# dfo_sub <- sid1 %>%
#   # filter out the 'Other' group
#   filter(bucket %ni% c("Other")) %>% 
#   filter(major_cat %in% "Governance, Leadership, and Accountability") %>% 
#   group_by(period, subcatx, operatingunit, bucket) %>% 
#   summarize(value = mean(value, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(!is.na(value)) %>% 
#   mutate(period = substr(period, 5, 9))
# 
# setting the colors
color_group <- c("#335B8E","#6CA18F","#CC5234")

viz1 <- ggplot(data=dfb_maj, aes(x=period, 
                               y=value, 
                               colour=bucket, 
                               group = bucket,
                               label=round(value,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=bucket),alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group) +
  scale_fill_manual(values=color_group) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "Country Buckets", color="Country Buckets")+
  facet_wrap(~major_cat)+
  theme_bw()
  # geom_line(data=dfo_maj, aes(x=period, 
  #                          y=value, 
  #                          colour=bucket, 
  #                          group = operatingunit), size=.8, alpha=0.2)
  # theme(axis.text.x=element_text(size=15))


# Only looking at the "Governance, Leadership, and Accountability" domain
viz1x <- ggplot(data=dfb_sub, aes(x=period, 
                                 y=value, 
                                 colour=bucket, 
                                 group = bucket,
                                 label=round(value,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=bucket),alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group) +
  scale_fill_manual(values=color_group) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "Country Buckets", color="Country Buckets")+
  facet_wrap(~subcatx)+
  theme_bw()+
  ggtitle("Governance, Leadership, and Accountability")
  # geom_line(data=dfo_sub, aes(x=period, 
  #                             y=value, 
  #                             colour=bucket, 
  #                             group = operatingunit), size=.8, alpha=0.2)


viz1xx <- ggplot(data=dfb_subx, aes(x=period, 
                                  y=score, 
                                  colour=bucket, 
                                  group = bucket,
                                  label=round(score,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=bucket),alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group) +
  scale_fill_manual(values=color_group) +
  xlab("Year") + ylab("Average Score")+
  labs(fill = "Country Buckets", color="Country Buckets")+
  facet_wrap(~subsubcat)+
  theme_bw()+
  ggtitle("Civil Society Engagement")


viz1 
viz1x 
viz1xx

dfox_maj <- sid1 %>% 
  # filter out the 'Other' group
  filter(operatingunit %in% c("Cambodia", 
                              "Thailand", 
                              # "India", 
                              # "Burma", 
                              "Indonesia")) %>% 
  group_by(period, major_cat, operatingunit) %>% 
  summarize(value = median(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))


color_group1 <- c("#B2182B","#A379BB","#948D79")
color_group1x <- c("#B2182B","#A379BB","#948D79", 
                   "#6CA18F","#CC5234")


viz2 <- ggplot(data=dfox_maj, aes(x=period, 
                                 y=value, 
                                 colour=operatingunit, 
                                 group = operatingunit,
                                 label=round(value,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=operatingunit),alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group1) +
  scale_fill_manual(values=color_group1) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~major_cat)+
  theme_bw()


# theme(axis.text.x=element_text(size=15))
dfox_sub <- sid1 %>%
  filter(operatingunit %in% c("Cambodia", 
                              "Thailand", 
                              # "India", 
                              # "Burma", 
                              "Indonesia")) %>%   
  filter(major_cat %in% "Governance, Leadership, and Accountability") %>% 
  group_by(period, subcatx, operatingunit, bucket) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))


dfox_subx <- csefinal %>%
  filter(operatingunit %in% c("Cambodia", 
                              "Thailand", 
                              # "India", 
                              # "Burma", 
                              "Indonesia")) %>%   
  group_by(period, subsubcat , operatingunit) %>% 
  summarize(score = mean(score, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(score)) 


viz2x <- ggplot(data=dfox_sub, aes(x=period, 
                                  y=value, 
                                  colour=operatingunit, 
                                  group =operatingunit,
                                  label=round(value,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=operatingunit),alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group1) +
  scale_fill_manual(values=color_group1) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~subcatx)+
  theme_bw()+
  ggtitle("Governance, Leadership, and Accountability")


viz2xx <- ggplot(data=dfox_subx, aes(x=period, 
                                    y=score, 
                                    colour=operatingunit, 
                                    group =operatingunit,
                                    label=round(score,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=operatingunit),
             alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group1) +
  scale_fill_manual(values=color_group1) +
  xlab("Year") + ylab("Average Score")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~subsubcat)+
  theme_bw()+
  ggtitle("Civil Society Engagement")

viz2
viz2x
viz2xx



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dfop_maj <- sid1 %>% 
  # filter out the 'Other' group
  filter(operatingunit %in% c("Rwanda", 
                              "Namibia", 
                              "Zimbabwe")) %>% 
  group_by(period, major_cat, operatingunit) %>% 
  summarize(value = median(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))


color_group2 <- c("#CEB966","#9CB084","#6BB1C9")


viz3 <- ggplot(data=dfop_maj, aes(x=period, 
                                  y=value, 
                                  colour=operatingunit, 
                                  group = operatingunit,
                                  label=round(value,1))) + 
  geom_line(size=1, alpha=0.8) +
  geom_point(aes(fill=operatingunit),alpha=0.8,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group2) +
  scale_fill_manual(values=color_group2) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~major_cat)+
  theme_bw()


# theme(axis.text.x=element_text(size=15))
dfop_sub <- sid1 %>%
  filter(operatingunit %in% c("Rwanda", 
                              "Namibia", 
                              "Zimbabwe")) %>% 
  filter(major_cat %in% "Governance, Leadership, and Accountability") %>% 
  group_by(period, subcatx, operatingunit, bucket) %>% 
  summarize(value = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  mutate(period = substr(period, 5, 9))


dfop_subx <- csefinal %>%
  filter(operatingunit %in% c("Rwanda", 
                              "Namibia", 
                              "Zimbabwe")) %>% 
  group_by(period, subsubcat , operatingunit) %>% 
  summarize(score = mean(score, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(score)) 


viz3x <- ggplot(data=dfop_sub, aes(x=period, 
                                   y=value, 
                                   colour=operatingunit, 
                                   group =operatingunit,
                                   label=round(value,1))) + 
  geom_line(size=1, alpha=0.8) +
  geom_point(aes(fill=operatingunit),alpha=0.8,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group2) +
  scale_fill_manual(values=color_group2) +
  xlab("Year") + ylab("Average Score (0 to 10)")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~subcatx)+
  theme_bw()+
  ggtitle("Governance, Leadership, and Accountability")


viz3xx <- ggplot(data=dfop_subx, aes(x=period, 
                                     y=score, 
                                     colour=operatingunit, 
                                     group =operatingunit,
                                     label=round(score,1))) + 
  geom_line(size=1, alpha=0.7) +
  geom_point(aes(fill=operatingunit),
             alpha=0.7,size=9, color="darkgrey", pch=21)+
  geom_text(color="white",check_overlap = TRUE, size=4)+
  scale_color_manual(values=color_group2) +
  scale_fill_manual(values=color_group2) +
  xlab("Year") + ylab("Average Score")+
  labs(fill = "OUs", color="OUs")+
  facet_wrap(~subsubcat)+
  theme_bw()+
  ggtitle("Civil Society Engagement")


viz3
viz3x
viz3xx




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Research Questions # 2 (SID-MER) ~~~~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sid_mer1 <- sid_mer %>%
  select(restype, TX_CURR_2017, TX_CURR_2019) %>% 
  group_by(restype) %>% 
  summarize_all(list(~sum(., na.rm=T))) %>% 
  ungroup()

  


