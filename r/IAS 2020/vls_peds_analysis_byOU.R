
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#================ Peds VLS analyses =========================
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
ldpkg(c("tidyverse", "eply", "readxl", "plotly", "lme4", "binom", "Hmisc"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%X"))
tm <- str_replace_all(t, "[: ]", "_")
dt <- str_replace_all(dx, "[ ]", "")





xfolder <- "C:/temp_folder/vls_peds"

flist <- list.files(xfolder, pattern="ovc_vls")

xlcols <- c("text","text", rep("numeric",8))

xlpath <- paste(xfolder, "/", flist[1], sep="")

# Pulling the dataset from the excel file
xl <- read_xlsx(path=xlpath, sheet = "Data", col_names = TRUE, 
                col_types = xlcols,
                na = "", trim_ws = TRUE, 
                progress = readxl_progress(), .name_repair = "unique")


df <- xl %>% 
  mutate(ovc_dreams = if_else(OVC==1 | DREAMS==1, "1) OVC", "2) None")) %>% 
  select(OU, PSNU, ovc_dreams, 
         TX_CURR, TX_PVLS_D, TX_PVLS_N) %>% 
  group_by(OU, PSNU, ovc_dreams) %>% 
  summarise_all(list(~sum(., na.rm=T))) %>% 
  ungroup() %>% 
  filter(TX_PVLS_D>0) %>% 
  filter(TX_PVLS_N<=TX_PVLS_D) %>% 
  # filter(TX_PVLS_D<=TX_CURR) %>% 
  mutate(vlc = TX_PVLS_D/TX_CURR)


options(scipen = 999)

log_vls <- function(x){
  
dfx <- df %>% filter(OU %in% x)  

ouname <- unique(dfx$OU)
# Logistic Regression
vls_m <- glm(TX_PVLS_N/TX_PVLS_D ~ ovc_dreams + vlc,
            weights=TX_PVLS_D,
            data=dfx,family="binomial")

px <- coef(summary(vls_m))[,4]
pval <- px[2]
se <- sqrt(diag(vcov(vls_m)))

tab <- exp(cbind(coef(vls_m), confint(vls_m)))

vlsodf <- as.data.frame(tab) %>%  
  slice(2:2) %>% 
  mutate(pvalue = pval) %>% 
  mutate(ou = ouname)

}

ouvec <- unique(df$OU)
  
vlslist <- map(.x=ouvec, .f=~log_vls(.x))

vodd_df <- bind_rows(vlslist)

write.csv(vodd_df, "ou_ORs_vlcadj.csv")


# # unweighted
# log_vls_uw <- function(x){
#   
#   dfx <- df %>% filter(OU %in% x)  
#   
#   ouname <- unique(dfx$OU)
#   # Logistic Regression
#   vls_m <- glm(TX_PVLS_N/TX_PVLS_D ~ ovc_dreams + vlc,
#                # weights=TX_PVLS_D,
#                data=dfx,family="binomial")
#   
#   px <- coef(summary(vls_m))[,4]
#   pval <- px[2]
#   se <- sqrt(diag(vcov(vls_m)))
#   
#   tab <- exp(cbind(coef(vls_m), confint(vls_m)))
#   
#   vlsodf <- as.data.frame(tab) %>%  
#     slice(2:2) %>% 
#     mutate(pvalue = pval) %>% 
#     mutate(ou = ouname)
#   
# }
# 
# ouvec <- unique(df$OU)
# 
# vlslistx <- map(.x=ouvec, .f=~log_vls_uw(.x))
# 
# vodd_dfx <- bind_rows(vlslistx)



dfb <- df %>%
  select(-c(PSNU)) %>% 
  group_by(OU, ovc_dreams) %>% 
  summarise_all(list(~sum(., na.rm=T))) %>% 
  ungroup() 

  
  dfstbin <- as.data.frame(binom.exact(dfb$TX_PVLS_N, dfb$TX_PVLS_D, conf.level = 0.95))
  
  stb <- binconf(dfb$TX_PVLS_N, dfb$TX_PVLS_D, alpha=0.05,
                 include.x=T, include.n=T, return.df=T)
  
  dfinal <- as.data.frame(cbind(dfb, stb)) %>% 
    mutate(dtype = "vls")


  dfstbinx <- as.data.frame(binom.exact(dfb$TX_PVLS_D, dfb$TX_CURR, conf.level = 0.95))
  
  stbx<- binconf(dfb$TX_PVLS_D, dfb$TX_CURR, alpha=0.05,
                 include.x=T, include.n=T, return.df=T)
  
  dfinalx <- as.data.frame(cbind(dfb, stbx)) %>% 
    mutate(ovc_dreams = case_when(
      ovc_dreams %in% c("2) None") ~ "4) None (VLC)",
      ovc_dreams %in% c("1) OVC") ~ "3) OVC (VLC)",
    )) %>% 
    mutate(dtype = "vlc")
  
  
  
  final1 <- bind_rows(dfinal, dfinalx) %>% 
    mutate(vls = round((TX_PVLS_N/TX_PVLS_D)*100)) %>% 
    mutate(vlcx = round((TX_PVLS_D/TX_CURR)*100)) %>% 
    mutate(labelx = case_when(
      dtype %in% c("vls") ~ paste("\n\n\n\n\n", vls, "%\n(", 
                                  prettyNum(TX_PVLS_N, big.mark = ","),
                                  "/\n",
                                  prettyNum(TX_PVLS_D, big.mark = ","),
                                  ")", sep=""),
      dtype %in% c("vlc") ~ paste("\n\n\n\n\n", vlcx, "%\n(", 
                                  prettyNum(TX_PVLS_D, big.mark = ","),
                                  "/\n",
                                  prettyNum(TX_CURR, big.mark = ","),
                                  ")", sep="")
      
    )) 
  
  ovcvec <- unique(final1$ovc_dreams)
  


  color_group1 <- c("#335B8E",
                    "#6CA18F",
                    "#B2B2B2",
                    "#DDDDDD")
    
  textcolor <- c("white",
                    "white",
                    "black",
                    "black")
  
  
  ggplot(final1, aes(x=OU, y=PointEst, fill=ovc_dreams, color=ovc_dreams,
                     label=labelx)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge(), width=0.8) +
    geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.3, size=0.9,
                  position=position_dodge(.8), color="black") + 
    geom_text(aes(color=ovc_dreams),check_overlap = TRUE, size=3.5,
              position=position_dodge(width=0.8)) +
    scale_color_manual(values=textcolor) +
    scale_fill_manual(values=color_group1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    xlab("Country") + ylab("%VLS or % VLC")+
    labs(fill = "OVC districts vs non-OVC", color="OVC districts vs non-OVC")+
    theme_bw()+
    ggtitle("Viral Load Suppression and Coverage (VLS & VLC)")
  
  
  
  