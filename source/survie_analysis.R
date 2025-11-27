# test mortalité 
library(tidyverse)
library(patchwork)
library(kableExtra)
library(gtsummary)
#library(FactoMineR)
#library(glmmTMB)
library(factoextra)
library(survival)
library(survminer)
library(xts)
library(lubridate)
library(readr)

metadata <- read.csv2("./Data_larvae/20240807_meta_data.csv")

metadata <- metadata %>% separate(ID, c("Camera", "ID2")) 
metadata <- metadata %>%  mutate(Camera=str_to_lower(Camera), fullID=paste0(Camera,ID2))

str(metadata)
metadata$date_adulte <- as.Date(metadata$date_adulte, format="%d-%m")
metadata$date_mort <- as.Date(metadata$date_mort, format="%d-%m")

metadata_sub <- metadata %>% dplyr::filter(replicat!=5 & replicat!=7)

metadata_sub <- metadata_sub %>% mutate(Time=as.numeric(difftime(date_mort,date_adulte, units="days")), 
                                        Status=rep(1)) %>% drop_na(date_mort)

# KM par replicat 
surv_mod <- survfit(Surv(Time, Status) ~ traitement+replicat, data = metadata_sub)

Fig_surv <- ggsurvplot(surv_mod, surv.median.line = "hv", conf.int = TRUE,
                      color = "traitement", break.x.by=2, legend.title="traitement")

Fig_surv$plot + theme_bw() + 
  theme (panel.grid = element_blank(),legend.text = element_text(size = 8),  
         legend.position = "right") + facet_wrap(vars(replicat))+
  xlab("Days post emergence") + ylab("Survival probability") +
  ggtitle("Survival of mosquitoes exposed to sublethal dose of permethrin at larval stage")

# tous replicat confondus 
surv_modall <- survfit(Surv(Time, Status) ~ traitement, data = metadata_sub)

Fig_surv_all <- ggsurvplot(surv_modall, surv.median.line = "hv", conf.int = TRUE,
                       color = "traitement", break.x.by=2, legend.title="traitement")

Fig_surv_all$plot + theme_bw() + 
  theme (panel.grid = element_blank(),legend.text = element_text(size = 8),  
         legend.position = "right") +
  xlab("Days post emergence") + ylab("Survival probability") +
  ggtitle("Survival of mosquitoes exposed to sublethal dose of permethrin at larval stage")

# Survival Median Time by individual 

surv_modall_ind <- survfit(Surv(Time, Status) ~ fullID, data = metadata_sub)
as.data.frame(surv_modall_ind)
surv_median(surv_modall_ind)
Fig_surv_ID <- ggsurvplot(surv_modall_ind, surv.median.line = "hv", conf.int = TRUE,
                           color = "ID2", break.x.by=2, legend.title="ID2")

Fig_surv_ID$plot + theme_bw() + 
  theme (panel.grid = element_blank(),legend.text = element_text(size = 8),  
         legend.position = "none") +
  xlab("Days post emergence") + ylab("Survival probability") +
  ggtitle("Survival of mosquitoes exposed to sublethal dose of permethrin at larval stage")

