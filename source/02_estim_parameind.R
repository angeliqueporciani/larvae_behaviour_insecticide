# test en individuel 
library(momentuHMM)
library(furrr)
library(dplyr)
library(future)
library(purrr)
library(zoo)
library(stringr)
library(here)
library(tidyverse)
source("./source/fun_estim_param.R")

# Step of analyse 

# 1 import data from files for each replicate and rpi 
# 2 put them in a tibble (one row=one individual)
# 3 CRCW 
# 4 estimation of speed (step by minute) with euclidian distance 
# 5 extract max speed 
# 6 estimation of bout length 
# 7 add identifier = file name + plate position with file of correspondance. 
# 8 export in csv with fullID2 as key for binding later to data base by individual 

# 1.Import 
# recup nom des dossier pour chaque replicat 
#files_names <- list.files(here("./Data_larvae/Res_orange_1fps/Detailed_data/"))
files_names <- list.files(here("./Data_larvae/Res_rouge_1fps/Detailed_data/"))

nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- files_names[i]
}

# recup nom des fichier par dossier 

# files_names2 <- list.files(here("./Data_larvae/Res_orange_1fps/Detailed_data/"))
# nb_files <- length(files_names)
# data_names <- vector("list",length=nb_files)
# 

list_file2 <- vector("list", length(files_names))
for (i in 1:length(files_names)) {
  #list_file2[[i]] <- list.files(paste(here("./Data_larvae/Res_orange_1fps/Detailed_data", 
  list_file2[[i]] <- list.files(paste(here("./Data_larvae/Res_rouge_1fps/Detailed_data", 
                                                   files_names[i])))
  #list_file2[[i]] <- strsplit(list_file2[[i]], split=".csv")
}
names(list_file2) <- data_names

tib_all_file <- as_tibble(list_file2)

# pivot to have one row-oneID
tib_all_file <- tib_all_file %>% 
  pivot_longer(everything(), names_to=c("file_name"))

# Import all the data 
fun_read_ind_data <- function(namefile1, namefile2){
  #read.delim(paste0("./data_larvae/Res_orange_1fps/Detailed_data/",namefile1,"/",namefile2), sep=";")
  read.delim(paste0("./data_larvae/Res_rouge_1fps/Detailed_data/",namefile1,"/",namefile2), sep=";")
  
}

tib_all_file <- tib_all_file %>% 
  mutate(data=map2(file_name, value, fun_read_ind_data))            
tib_all_file <- tib_all_file %>% 
  mutate(Arena=as.character(strsplit(value, split="Ind0.csv")))
tib_all_file <- tib_all_file %>% 
  mutate(Arena=gsub("_", "", Arena))

# preparation for join 
tib_all_file <- tib_all_file %>%
  mutate(Lot=gsub("[^0-9]","", str_extract(file_name,"lot\\d.*")),
         Lot2=dplyr::case_when(is.na(Lot) ~ "1",
                               .default = Lot),
        Rpi=str_extract(file_name, "rpi\\d*"), 
        Rpi=str_replace(Rpi, "rpi2","rpi02"),
         Replicat=str_extract(file_name,"R\\d*"),
         ArenaN=str_extract(Arena, "\\d.*"),
         #fullID2=str_c(Rpi, ArenaN, Replicat, ID),
         Experiment=rep("Rouge")) %>% mutate_if(is.character,as.factor)

#corres_ID_orange <- read_csv2("./data_larvae/corres_Arena_ID_orange.csv") %>% select(c(1:2))
corres_ID_rouge<- read_csv2("./data_larvae/Res_rouge_1fps/corres_ID_rouge_1.csv") %>% 
  select(c(1:4)) %>% mutate_if(is.character,as.factor)

# join 
tib_all_file_j <- tib_all_file %>% inner_join(corres_ID_rouge) %>% mutate_if(is.character,as.factor)

summary(tib_all_file_j$Replicat)

# apply CRCW function to all ID 

# apply 
tib_all_file_j <- tib_all_file_j %>% 
  mutate(CRCW=future_map(data, poss.CRCW)) 

# speed estimation after CRCW replacement of NA 
## apply 
tib_all_file_j <- tib_all_file_j %>% 
  mutate(CRCW2=future_map(CRCW, poss.step_fun))


# Apply all 

tib_all_file_j <- tib_all_file_j %>%
  mutate(Max_speed=future_map_dbl(CRCW2, poss.max_speed),
    Max_bout_activ=future_map_dbl(CRCW2, poss.fun_Lbout_activ),
        Max_bout_inactiv=future_map_dbl(CRCW2, poss.fun_Lbout_inactiv))

# creation of unique ID 
tib_all_file_j <- tib_all_file_j %>%
  mutate(Rpi=str_replace(Rpi, "rpi02","rpi2"),
    fullID2=str_c(Rpi, ArenaN, Replicat, ID, Lot2))


# extraction juste des paramètres pour les graphiques 

tab_param_rouge <- tib_all_file_j %>% 
  select(fullID2, Experiment, Max_bout_activ,
                                            Max_bout_inactiv, Max_speed, 
                                            Arena, Replicat, Rpi, Lot, Lot2) %>% 
  as.data.frame()


# Save 
#saveRDS(tib_all_file, "./output/tib_all_file_orange.rds")
saveRDS(tib_all_file_j, "./output/tib_all_file_rouge.rds")

write.csv(tab_param_rouge, "./output/tab_param_estim_rouge.csv")
