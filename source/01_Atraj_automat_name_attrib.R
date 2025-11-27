# Note : ce script n'est valable que pck les arènes ont été definies dans le même ordre sur le logiciel animalTA 
# Si fait autrement, renomer les individus directement sur le logiciel utilisé pour le tracking. 

# Load package ----
library(tidyverse)
library(dplyr)
library(here)

## RED data 1fps 

# load corrected data from Animal TA -----
#dataATA <- read.delim("./Corr_coord_data/rpi2_10fps_R05_Corrected.csv",sep=";")
# subdat <- dataATA[1:500,]
corres_ID <- read_csv2("./data_larvae/Data_vid_rouge/corres_ID_rouge_1.csv") %>% select(c(1:4))

#str(dataATA)

# importation de tous les fichier du dossier ---- 
files_names <- list.files(here("./data_larvae/Res_rouge_1fps/coordinates/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}


# for (i in 1:nb_files) {
#   assign(data_names[[i]], 
#                          read_csv2(paste(here("./Corr_coord_data", files_names[i]))))
# }

list_file <- vector("list", length(files_names))
for (i in 1:nb_files) {
  list_file[[i]] <- read.delim(paste(here("./data_larvae/Res_rouge_1fps/coordinates", files_names[i])), sep=";")
}

Replicat_name <- vector("list", length(data_names))
for (n in 1:length(data_names)){
  temp_name <- strsplit(data_names[[n]], split="_") %>% unlist()
  Replicat_name[[n]] <- temp_name
  #Replicat_name[[n]] <-  paste0(temp_name[1],temp_name[3])
}

names(list_file) <- data_names

#corres_ID <- corres_ID %>% dplyr::select(!Replicat)

# Transfo table for automatic name attribution and bind ID -----
datalong1 <- list_file[[1]] %>%mutate(Exp=unique(names(list_file[1])), .after=Time) %>%
  pivot_longer(
  !c(1:3),
  names_to = c("Axe", "Arena", "Ind"),
  names_pattern = "(.*)_(.*)_(.*)",
  values_to = "posXY"
) %>% mutate(Rpi=str_extract(Exp, "rpi\\d*"), Replicat=str_extract(Exp,"R\\d*")) %>%
  inner_join(corres_ID) %>% mutate(#Replicat=Replicat_name[[1]][2],
                                   Passage=Replicat_name[[1]][3],
                                       ArenaN=str_extract(Arena, "\\d.*"),
                                       fullID=str_c(Replicat, ID),fullID2=str_c(ArenaN,Replicat, ID))

datalong5 <- list_file[[5]] %>%mutate(Exp=unique(names(list_file[5])), .after=Time) %>%
  pivot_longer(
    !c(1:3),
    names_to = c("Axe", "Arena", "Ind"),
    names_pattern = "(.*)_(.*)_(.*)",
    values_to = "posXY"
  ) %>% mutate(Rpi=str_extract(Exp, "rpi\\d*"), Replicat=str_extract(Exp,"R\\d*")) %>%
  inner_join(corres_ID) %>% mutate(#Replicat=Replicat_name[[5]][2],
                                   Passage=Replicat_name[[5]][3],
                                   ArenaN=str_extract(Arena, "\\d.*"),
                                   fullID=str_c(Replicat, ID),fullID2=str_c(ArenaN,Replicat, ID))

## boucle 

data_longer <- vector("list", length(list_file))

for (n in 1:length(list_file)){
  data_longer[[n]] <- list_file[[n]] %>% 
    mutate(Exp=unique(names(list_file[n])), .after=Time) %>% 
  pivot_longer(
    !c(1:3),
    names_to = c("Axe", "Arena", "Ind"),
    names_pattern = "(.*)_(.*)_(.*)",
    values_to = "posXY"
  ) %>% 
    mutate(Rpi=str_extract(Exp, "rpi\\d*"), Replicat=str_extract(Exp,"R\\d*"),
           Lot=gsub("[^0-9]","", str_extract(Exp,"lot\\d.*")),
           Lot2=dplyr::case_when(is.na(Lot) ~ "1",
                                 .default = Lot)) %>% 
    inner_join(corres_ID) %>% 
    mutate(Replicat=rep(Replicat_name[[n]][2]), 
                                     Passage=Replicat_name[[n]][3],
                                     ArenaN=str_extract(Arena, "\\d.*"),
                                     fullID=str_c(Replicat, ID), 
                      fullID2=str_c(Rpi, ArenaN, Replicat, ID, Passage, Lot2))
}


# transfo in wider table to analyse with MoveR ---- 

data_transf <- data_longer[[1]] %>% select(Frame, Time,posXY, Axe, Arena,fullID2) %>%
  pivot_wider(
    names_from = c(Axe, Arena, fullID2),
    values_from = posXY
  )

head(data_longer[[1]])

## loop 

for (n in 1:length(data_longer))
{
  data_wid_temp <- data_longer[[n]] %>% select(Frame, Time, posXY, Axe, Arena, fullID2) %>% 
    pivot_wider(
      names_from = c(Axe, Arena,fullID2),
      values_from = posXY
    )
  write_delim(data_wid_temp, paste0("./data_larvae/Res_rouge_1fps/Named_Coordinates/Named_",data_names[[n]],".csv"), delim=";")
            
}

rm(list=ls()) 

#######################
## orange data 1fps ##
######################

# load corrected data from Animal TA -----
#dataATA <- read.delim("./Corr_coord_data/rpi2_10fps_R05_Corrected.csv",sep=";")
# subdat <- dataATA[1:500,]
corres_ID_or <- read_csv2("./data_larvae/corres_Arena_ID_orange.csv") %>% 
  select(c(1:2)) %>% 
  mutate(Arena=as.numeric(str_extract(Arena, "\\d.*")))

#str(dataATA)

# importation de tous les fichier du dossier ---- 
files_names <- list.files(here("./data_larvae/Res_orange_1fps/coordinates/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}


list_file <- vector("list", length(files_names))
for (i in 1:nb_files) {
  list_file[[i]] <- read.delim(paste(here("./data_larvae/Res_orange_1fps/coordinates", files_names[i])), sep=";")
}

Replicat_name <- vector("list", length(data_names))
for (n in 1:length(data_names)){
  temp_name <- strsplit(data_names[[n]], split="_") %>% unlist()
  Replicat_name[[n]] <- temp_name
  #Replicat_name[[n]] <-  paste0(temp_name[1],temp_name[3])
}

names(list_file) <- data_names


# Transfo table for automatic name attribution and bind ID -----

## boucle 

data_longer <- vector("list", length(list_file))

for (n in 1:length(list_file)){
  data_longer[[n]] <- list_file[[n]] %>% 
    mutate(Exp=unique(names(list_file[n])), .after=Time) %>% 
    pivot_longer(
      !c(1:3),
      names_to = c("Axe", "Arena", "Ind"),
      names_pattern = "(.*)_(.*)_(.*)",
      values_to = "posXY"
    ) %>% 
    mutate(Rpi=str_extract(Exp, "rpi\\d*"), Replicat=str_extract(Exp,"R\\d*"),
           Arena=as.numeric(str_extract(Arena, "\\d.*"))) %>% 
    inner_join(corres_ID_or) %>% 
    mutate(Replicat=rep(Replicat_name[[n]][3]), 
           #Passage=Replicat_name[[n]][3],
           ArenaN=str_extract(Arena, "\\d.*"),
           fullID=str_c(Replicat, ID), 
           fullID2=str_c(Rpi, ArenaN, Replicat, ID))
}


# transfo in wider table to analyse with MoveR ---- 

## loop 

for (n in 1:length(data_longer))
{
  data_wid_temp <- data_longer[[n]] %>% select(Frame, Time, posXY, Axe, Arena, fullID2) %>% 
    pivot_wider(
      names_from = c(Axe, Arena,fullID2),
      values_from = posXY
    )
  write_delim(data_wid_temp, paste0("./data_larvae/Res_orange_1fps/Named_Coordinates/Named_",data_names[[n]],".csv"), delim=";")
  
}

rm(list=ls()) 
