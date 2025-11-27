# Visualisation data from script 06_joinall_param
library(dplyr)
library(tidyverse)
library(patchwork)
library(kableExtra)
library(gtsummary)
library(FactoMineR)
library(glmmTMB)
library(factoextra)
library(lubridate)

# load data 
all_param <- readRDS("./output/all_param.rds") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(replicat=as.numeric(sub(".*R([0-9]+).*", "\\1", Video)),
 Lot=gsub("[^0-9]","", str_extract(Video,"lot\\d.*")),
         Lot=dplyr::case_when(is.na(Lot) ~ "1",
                               .default = Lot), 
 ID_join=str_c(Rpi, replicat,ID,Lot))

all_param_SP <- readRDS("./output/all_param_SP.rds") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(replicat=as.numeric(sub(".*R([0-9]+).*", "\\1", Video)),
         Lot=gsub("[^0-9]","", str_extract(Video,"lot\\d.*")),
         Lot=dplyr::case_when(is.na(Lot) ~ "1",
                              .default = Lot),
         ID_join=str_c(Rpi, replicat,ID,Lot))

# load metadata 
metadata_orange <- read_csv2("./data_larvae/20240807_meta_data_orange.csv") %>% 
  mutate_if(is.character, as.factor)

metadata_rouge <- read_csv2("./data_larvae/20250613_données_DL50_concentration_rouge.csv") %>% 
  mutate_if(is.character, as.factor)


metadata_orange_sub <- metadata_orange %>% filter(replicat!=c(5)) %>% droplevels() %>% 
  mutate(date_eclosion2=as.Date(date_eclosion, format="%d-%m"),
         date_nymphe2=as.Date(date_nymphe, format="%d-%m"),
         date_adulte2=as.Date(date_adulte, format="%d-%m"),
         date_mort2=as.Date(date_mort, format="%d-%m"),
         duree_vie=date_mort2-date_eclosion2,
         duree_nymphe=date_nymphe2-date_eclosion2, 
         Experiment=rep("Orange")) %>% 
        rename(Rpi=camera) %>% rename(Video=ID,ID=plaque) %>% 
  mutate(Rpi=tolower(Rpi), 
         Lot=rep("1"), 
         ID_join=str_c(Rpi, replicat,ID,Lot),
         traitement=str_replace(traitement,"perm","perm_orange")) %>% 
  dplyr::select(replicat,Video,Rpi,ID,date_eclosion,passage_nymphe,date_nymphe,
                date_adulte,date_mort,sexe,traitement,position_LAM,moniteur,LAMetsurv,date_eclosion2,
                date_nymphe2,date_adulte2,date_mort2,duree_vie,duree_nymphe,ID_join, Lot, Experiment)



metadata_rouge_sub <- metadata_rouge %>% filter(replicat!=c(1)) %>% droplevels() %>% 
  mutate(date_eclosion2=as.Date(date_eclosion, format="%m/%d/%y"),
         date_nymphe2=as.Date(date_nymphe, format="%m/%d/%y"),
         date_adulte2=as.Date(date_adulte, format="%m/%d/%y"),
         date_mort2=as.Date(date_mort, format="%m/%d/%y"),
         duree_vie=date_mort2-date_eclosion2,
         duree_nymphe=date_nymphe2-date_eclosion2,
         Experiment=rep("Rouge")) %>% 
         rename(Rpi=camera) %>% rename(Video=ID,ID=plaque) %>%
         mutate(Rpi=tolower(Rpi),
                Lot= if_else(str_detect(Rpi, "b"), "2", "1"),
                Rpi=gsub("b", "", Rpi),
                ID_join=str_c(Rpi, replicat,ID,Lot),
                traitement=str_replace(traitement,"perm","perm_rouge")) %>% 
  dplyr::select(replicat,Video,Rpi,ID,date_eclosion,passage_nymphe,date_nymphe,
                date_adulte,date_mort,sexe,traitement,position_LAM,moniteur,LAMetsurv,date_eclosion2,
                date_nymphe2,date_adulte2,date_mort2,duree_vie,duree_nymphe,ID_join,Lot, Experiment)
  

metadata_all <- rbind(metadata_orange_sub, metadata_rouge_sub ) %>% 
  mutate_if(is.character, as.factor) %>% dplyr::filter(replicat!=7)
  
unique(metadata_all$replicat)

# join metadata and param estim 
summary(data_complete)
data_complete <- inner_join(all_param, metadata_all, by=c("Experiment", "ID_join","Lot")) %>% 
  mutate(Traitement= as.factor(case_when(
    Traitement == "Permethrin" ~ "Permethrin_orange",
    TRUE ~ Traitement)))


data_complete_SP <- inner_join(all_param_SP, metadata_all, by=c("Experiment", "ID_join","Lot"))
# Save 
 saveRDS(data_complete, "./output/data_complete.rds")
# saveRDS(data_complete_SP, "./output/data_complete_SP.rds")



# plot of prop_time_lost 

ggplot(data_complete)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Replicat)

# effet traitement 
ggplot(data_complete)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Traitement)

#effet replicat
ggplot(data_complete)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Replicat)

# effet experiment
ggplot(data_complete)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Experiment)

# selection des ID inf à 20% de temps perdu pour les representations graphiques ultérieures 

data_complete_inf20 <- data_complete %>% dplyr::filter(Prop_time_lost<0.2)

ggplot(data_complete_inf20)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Traitement)

## visualisation pannel 


loop.list <- c("Prop_time_moving","Average_Speed", "Average_Speed_Moving", 
               "Traveled_Dist", "Traveled_Dist_Moving",
               "Max_bout_activ", "Max_bout_inactiv", "Max_speed")
plot_list <- list()

for (n in loop.list)
{
  plot_list[[n]] <- ggplot(data=data_complete_inf20)+
    geom_jitter(aes_string(x="Traitement", y=n, color="Traitement")) +
    geom_boxplot(aes_string(x="Traitement", y=n, color="Traitement"), width = 0.15, position = position_dodge(0.9)) +
    geom_violin(aes_string(x="Traitement", y=n,
                           fill = "Traitement",
                           colour="Traitement"),alpha=0.3, linewidth = 0) +
    theme_light()+
    theme(legend.position = "none")+
    ylab(paste(n))+
    scale_color_manual(
      values = c("#648FFF", "#FE6100","red"))+
    scale_fill_manual(
      values = c("#648FFF", "#FE6100","red"))
  #print(plot_list[[n]])
}

wrap_plots(plot_list[1:4], nrow=2)
wrap_plots(plot_list[5:8], nrow=2)

# Same for spatial 
data_complete_SP_inf20 <- data_complete_SP %>% dplyr::filter(Prop_time_lost<0.2)

loop.list_SP <- c("Prop_time_inside","Nb_entries", "Average_Speed_moving", 
                "Traveled_Dist_Moving","Latency")
plot_list_SP <- list()

for (n in loop.list_SP)
{
  plot_list_SP[[n]] <- ggplot(data=data_complete_SP_inf20)+
    geom_jitter(aes_string(x="Traitement", y=n, color="Traitement")) +
    geom_boxplot(aes_string(x="Traitement", y=n, color="Traitement"), width = 0.15, position = position_dodge(0.9)) +
    geom_violin(aes_string(x="Traitement", y=n,
                           fill = "Traitement",
                           colour="Traitement"),alpha=0.3, linewidth = 0) +
    theme_light()+
    theme(legend.position = "none")+
    ylab(paste(n))+
    scale_color_manual(
      values = c("#648FFF", "#FE6100","red"))+
    scale_fill_manual(
      values = c("#648FFF", "#FE6100","red"))
  #print(plot_list[[n]])
}

wrap_plots(plot_list_SP[1:5], nrow=3)

# plot visu temps nymphose 
ggplot(data_complete_inf20)+
  geom_boxplot(aes(x=Traitement, y=duree_nymphe, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=duree_nymphe, colour=Traitement))+
  #geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  scale_color_manual(
    values = c("#648FFF", "#FE6100","red"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100","red"))


ggplot(data_complete_inf20)+
  geom_jitter(aes(x=Traitement, y=duree_vie, colour=Traitement))+
  geom_boxplot(aes(x=Traitement, y=duree_vie, colour=Traitement))+
  #geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  scale_color_manual(
    values = c("#648FFF", "#FE6100","red"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100","red"))

# visualisation d'une correlation entre prop_time_moving, vitesse, et durée de vie par ex 

plot(data_complete_inf20$Prop_time_moving,data_complete_inf20$duree_vie)
# nope
plot(data_complete_inf20$Prop_time_moving,data_complete_inf20$duree_nymphe)
# nope

plot(data_complete_inf20$Average_Speed_Moving,data_complete_inf20$duree_nymphe)
plot(data_complete_inf20$Average_Speed_Moving,data_complete_inf20$duree_vie)

plot(data_complete_inf20$Max_speed,data_complete_inf20$duree_vie)
plot(data_complete_inf20$Max_speed,data_complete_inf20$duree_nymphe)

plot(data_complete_inf20$Max_bout_inactiv,data_complete_inf20$duree_vie)
plot(data_complete_inf20$Max_bout_inactiv,data_complete_inf20$duree_nymphe)

# Effectif LAM 

data_complete_inf20 %>% group_by(Traitement, position_LAM) %>% 
  summarise(n=n()) %>% print(n = 100)

# Etrange certains numero de moniteur ... A confirmer avec le fiches de données. 
