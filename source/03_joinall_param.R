## Script for joining all tab and parameters estimated for both experiment orange and red 
library(furrr)
library(dplyr)
library(future)
library(purrr)
library(zoo)
library(stringr)
library(here)
library(tidyverse)

# Import data by individual for orange and red 
corres_ID_orange <- read_csv2("./data_larvae/Res_orange_1fps/corres_Arena_ID_orange.csv") %>% 
  select(c(1:2)) %>% mutate(Arena=as.integer(str_extract(Arena, "\\d.*")))
  
corres_ID_rouge<- read_csv2("./data_larvae/Res_rouge_1fps/corres_ID_rouge_1.csv") %>%
  select(c(1:4)) %>% 
  mutate(Arena=as.integer(str_extract(Arena, "\\d.*")),
         Rpi=str_replace(Rpi, "rpi02","rpi2"))


corres_ttmt_orange <- read_csv2("./data_larvae/Res_orange_1fps/corr_traitement_plaque.csv") %>% 
  select(c(1:4)) %>% rename(Rpi=Camera)

corres_ttmt_rouge<- read_csv2("./data_larvae/Res_rouge_1fps/corr_ttmt_plaque_rouge.csv") %>% 
  mutate(Lot=as.character(Lot), Lot2=Lot, Rpi=str_replace(Rpi, "rpi02","rpi2")) %>% dplyr::select(!c(Lot))


## orange 

res_by_ind_orange <- read.delim("./data_larvae/Res_orange_1fps/Results_by_ind.csv", sep=";") %>% 
  mutate(
         Rpi=str_extract(Video, "rpi\\d*"), 
         Replicat=str_extract(Video,"R\\d*"),
         #ArenaN=str_extract(Arena, "\\d.*"),
         Experiment=rep("Orange")) %>% 
  inner_join(corres_ID_orange) %>% 
  mutate(fullID2=str_c(Rpi, Arena, Replicat, ID), 
         Plaque=as.integer(str_sub(ID,2,2))) %>% 
  inner_join(corres_ttmt_orange)


## rouge
res_by_ind_rouge <- read.delim("./data_larvae/Res_rouge_1fps/Results_by_ind_complet.csv", sep=";") %>% 
  mutate(Lot=gsub("[^0-9]","", str_extract(Video,"lot\\d.*")),
         Lot2=dplyr::case_when(is.na(Lot) ~ "1",
                               .default = Lot),
    Rpi=str_extract(Video, "rpi\\d*"),
    Rpi=str_replace(Rpi, "rpi02","rpi2"), 
    Replicat=str_extract(Video,"R\\d*"),
    #ArenaN=str_extract(Arena, "\\d.*"),
    Experiment=rep("Rouge")) %>% 
  inner_join(corres_ID_rouge) %>% 
  mutate(fullID2=str_c(Rpi, Arena, Replicat, ID, Lot2),
         Plaque=as.integer(str_sub(ID,2,2)),
         Rpi=str_replace(Rpi, "rpi02","rpi2")) %>% 
    inner_join(corres_ttmt_rouge)


# Import data spatial for both 

res_by_ind_orange_SP <- read.delim("./data_larvae/Res_orange_1fps/Spatial/Element_Tous_bords_0.csv", sep=";") %>% 
                                     mutate(
                                       Rpi=str_extract(Video, "rpi\\d*"), 
                                       Replicat=str_extract(Video,"R\\d*"),
                                       #ArenaN=str_extract(Arena, "\\d.*"),
                                       Experiment=rep("Orange")) %>% 
                                     inner_join(corres_ID_orange) %>% 
                                     mutate(fullID2=str_c(Rpi, Arena, Replicat, ID),
                                            Plaque=as.integer(str_sub(ID,2,2))) %>% 
                                    inner_join(corres_ttmt_orange)

                                   

res_by_ind_rouge_SP <- read.delim("./data_larvae/Res_rouge_1fps/Spatial/Element_Tous_bords_0.csv", sep=";") %>% 
  mutate(Lot=gsub("[^0-9]","", str_extract(Video,"lot\\d.*")),
         Lot2=dplyr::case_when(is.na(Lot) ~ "1",
                               .default = Lot),
         Rpi=str_extract(Video, "rpi\\d*"),
         Rpi=str_replace(Rpi, "rpi02","rpi2"),
         Replicat=str_extract(Video,"R\\d*"),
         #ArenaN=str_extract(Arena, "\\d.*"),
         Experiment=rep("Rouge")) %>% 
  inner_join(corres_ID_rouge) %>% 
  mutate(fullID2=str_c(Rpi, Arena, Replicat, ID, Lot2),
         Plaque=as.integer(str_sub(ID,2,2))) %>% 
  inner_join(corres_ttmt_rouge)


# Import estimated parameter for both 
estim_param_orange <- read_csv("./output/tab_param_estim_orange.csv") %>% 
  mutate(Arena=as.integer(str_extract(Arena, "\\d.*")))


estim_param_rouge <- read_csv("./output/tab_param_estim_rouge.csv") %>% 
  mutate(Arena=as.integer(str_extract(Arena, "\\d.*")), 
         Lot=as.character(Lot),
         Lot2=as.character(Lot2))

# Join individual parameter with estimated for orange and red 
all_param_orange <- inner_join(res_by_ind_orange, estim_param_orange)
all_param_rouge <- inner_join(res_by_ind_rouge, estim_param_rouge)

# subset variables 
all_param_orange_sub <- all_param_orange %>% 
  dplyr::filter(Replicat!="R05") %>% 
  dplyr::select(Video, Arena, Beginning_seq, End_seq, Prop_time_lost,Moving_threshold,
                Prop_time_moving,Average_Speed, Average_Speed_Moving, Traveled_Dist, Traveled_Dist_Moving,
                Meander, Meander_moving, Traveled_Dist_Moving, Rpi, Replicat, Experiment, ID, fullID2,
                Max_bout_activ, Max_bout_inactiv, Max_speed, Traitement, Plaque)

all_param_rouge_sub <- all_param_rouge %>%  
  dplyr::select(Video, Arena, Beginning_seq, End_seq, Prop_time_lost,Moving_threshold,
                Prop_time_moving,Average_Speed, Average_Speed_Moving, Traveled_Dist, Traveled_Dist_Moving,
                Meander, Meander_moving, Traveled_Dist_Moving, Rpi, Replicat, Experiment, ID, fullID2,
                Max_bout_activ, Max_bout_inactiv, Max_speed, Traitement, Plaque)

# SPATIAL 
names(res_by_ind_orange_SP)

all_param_orange_sub_SP <- res_by_ind_orange_SP %>% 
  dplyr::filter(Replicat!="R05") %>% 
  dplyr::select(Video, Arena, Mean_Distance,Latency, Prop_time_inside,Time_inside,
                Nb_entries,Prop_time_lost,
                Prop_time_moving,Average_Speed, Average_Speed_moving, Traveled_Dist, Traveled_Dist_Moving,
                Meander, Meander_moving, Traveled_Dist_Moving, Rpi, Replicat, Experiment, ID, fullID2,
                Traitement, Plaque)

all_param_rouge_sub_SP <- res_by_ind_rouge_SP %>%  
  dplyr::select(Video, Arena, Mean_Distance,Latency, Prop_time_inside,Time_inside,
                Nb_entries,Prop_time_lost,
                Prop_time_moving,Average_Speed, Average_Speed_moving, Traveled_Dist, Traveled_Dist_Moving,
                Meander, Meander_moving, Traveled_Dist_Moving, Rpi, Replicat, Experiment, ID, fullID2,
                Traitement, Plaque)


# rbind both for graphic purpose (data visu check)

all_param <- rbind(all_param_orange_sub,all_param_rouge_sub)

all_param_SP <- rbind(all_param_orange_sub_SP,all_param_rouge_sub_SP)

# save
saveRDS(all_param, "./output/all_param.rds")
saveRDS(all_param_SP, "./output/all_param_SP.rds")


# STOP 
# analysis and final plot on other script 
