# Ce Script est fait pour nettoyer les données de tracking provenant d'Animal TA 
# avec le package MoveR 

#  load packages 

# Load package ----
library(tidyverse)
library(dplyr)
library(here)
library(MoveR)
library(data.table)

# importation de tous les fichier du dossier ---- 
files_names <- list.files(here("./Data_larvae/Res_rouge_1fps/Named_Coordinates/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}

list_file <- vector("list", length(files_names))
for (i in 1:nb_files) {
  list_file[[i]] <- MoveR::readAnimalTA(paste(here("./Data_larvae/Res_rouge_1fps/Named_Coordinates/", 
                                                   files_names[i])), rawDat=FALSE)
  list_file[[i]] <- MoveR::setInfo(list_file[[i]], frameR = 1, imgRes = c(1080, 720)) 
}

# retrieve 23 and 24th arena from listfile[[3]]
#list_file2 <- list_file[-3]

# NA filtering 

list_files_NAfilt<- list()

for (i in 1:length(list_file)){
#for (i in 3){
    
  print(i)

    filter.InfX <-
    MoveR::filterFunc(
      list_file[[i]],
      toFilter = "x.pos",
      customFunc = function(x)
       is.na(x)
    )
  
  ## first specify the second filter
  filter.InfY <-
    MoveR::filterFunc(
      list_file[[i]],
      toFilter = "y.pos",
      customFunc = function(x)
        is.na(x)
    )
  
  ## then merge the previously specifed filter
  filter.Inf <-
  MoveR::mergeFilters(filters = list(filter.InfX, filter.InfY),
                        cond = TRUE)
  
  trackDat.NAfilt<-MoveR::filterTracklets(list_file[[i]],
                                                 filter = filter.Inf,
                                                 splitCond = TRUE,
                                                 minDur = MoveR::getInfo(list_file[[i]], "frameR"))
  list_files_NAfilt[[i]] <- trackDat.NAfilt[[2]]
  
}


# save the cleaned dataset as .csv compressed as .gz
for (n in 1:length(list_files_NAfilt)){
  data.table::fwrite(
    MoveR::convert2List(list_files_NAfilt[[n]]),
    paste(paste0("./Data_larvae/Res_rouge_1fps/NA_filtered_data/NAfilt_", data_names[n]), 
          "csv", "gz", sep = "."),
    sep = ";",
    dec = ".",
    na = "NA",
  )
}

## Same for orange

rm(list=ls()) 

files_names <- list.files(here("./Data_larvae/Res_orange_1fps/Named_Coordinates/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}

list_file <- vector("list", length(files_names))
for (i in 1:nb_files) {
  list_file[[i]] <- MoveR::readAnimalTA(paste(here("./Data_larvae/Res_orange_1fps/Named_Coordinates/", 
                                                   files_names[i])), rawDat=FALSE)
  list_file[[i]] <- MoveR::setInfo(list_file[[i]], frameR = 1, imgRes = c(1080, 720)) 
}

# NA filtering 

list_files_NAfilt<- list()

for (i in 1:length(list_file)){
  #for (i in 3){
  
  print(i)
  
  filter.InfX <-
    MoveR::filterFunc(
      list_file[[i]],
      toFilter = "x.pos",
      customFunc = function(x)
        is.na(x)
    )
  
  ## first specify the second filter
  filter.InfY <-
    MoveR::filterFunc(
      list_file[[i]],
      toFilter = "y.pos",
      customFunc = function(x)
        is.na(x)
    )
  
  ## then merge the previously specifed filter
  filter.Inf <-
    MoveR::mergeFilters(filters = list(filter.InfX, filter.InfY),
                        cond = TRUE)
  
  trackDat.NAfilt<-MoveR::filterTracklets(list_file[[i]],
                                          filter = filter.Inf,
                                          splitCond = TRUE,
                                          minDur = MoveR::getInfo(list_file[[i]], "frameR"))
  list_files_NAfilt[[i]] <- trackDat.NAfilt[[2]]
  
}


# save the cleaned dataset as .csv compressed as .gz
for (n in 1:length(list_files_NAfilt)){
  data.table::fwrite(
    MoveR::convert2List(list_files_NAfilt[[n]]),
    paste(paste0("./Data_larvae/Res_orange_1fps/NA_filtered_data/NAfilt_", data_names[n]), 
          "csv", "gz", sep = "."),
    sep = ";",
    dec = ".",
    na = "NA",
  )
}


rm(list=ls()) 
