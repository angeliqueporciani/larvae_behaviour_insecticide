# First view of data over 1 hour of recording of An.gambiae KdrKis larvae 

# Load package ---- 
library(dplyr)
#library(tidyverse)
library(MoveR)
library(kableExtra)
library(readr)
library(stringr)
library(ggplot2)

# import data ---- 
corres_ID <- read_csv2("./data_larvae/Data_vid_rouge/corres_ID_rouge.csv") %>% select(c(1:4))
res_by_ind_ATA <- read.delim("./data_larvae/Data_vid_rouge/Results/Results_by_ind.csv", sep=";") 

# data management ----
## unique ID attribution 
corres_ID <- corres_ID %>% 
  mutate(Arena=as.numeric(str_extract(Arena, "\\d.*")))

res_by_ind_ATA <- res_by_ind_ATA %>% 
  mutate(
    Replicat = str_extract(Video, "R\\d*"),
    Rpi=str_extract(Video, "rpi\\d*"),
    Passage=str_extract(Video, "0\\dP")
   # ArenaN = str_extract(Arena, "\\d.*"),
  ) %>% inner_join(corres_ID) %>% 
  mutate(fullID = str_c(Rpi, Replicat, ID),
             fullID2 = str_c(Rpi, Arena, Replicat, ID, Passage))
#%>% select(!20:26)

ggplot(res_by_ind_ATA)+
  geom_point(aes(x=fullID2, y=Prop_time_lost, colour=fullID2))+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")+
  facet_wrap(~Passage)


res_by_ind_ATA %>% filter(Prop_time_lost>0.20) %>% nrow()
# 49 individuals need to be corrected 

49/240# 20% de "mauvais" tracking 

# Which One ? 
res_by_ind_ATA %>% filter(Prop_time_lost>0.20) %>% select(fullID2, Video, Prop_time_lost) %>% 
 arrange(Prop_time_lost)  %>% 
  kbl(caption = "Id of individuals with >20% proportion of data lost", booktabs = T)%>%
  kable_styling(full_width = T)

# total des ID trier par ordre croissant de temps perdu ---- 

res_by_ind_ATA %>% select(fullID2, Video, Prop_time_lost) %>% 
  arrange(Prop_time_lost)  %>% 
  kbl(caption = "Id of individuals with >20% proportion of data lost", booktabs = T)%>%
  kable_styling(full_width = T)
