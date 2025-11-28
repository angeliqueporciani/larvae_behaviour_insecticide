# Script to estimate length of active/inactive bout for each individuals selected (vid+LAM)

# library(MoveR)
library(tidyverse)
library(dplyr)
library(here)
library(MoveR)
library(data.table)
library(zoo)
source("./source/fun_split_bout.R")

# 1.Filter NA : other script, application of NA filter (loss of tracking) Import data already cleaned -----
# Pb with this approach : separation by tracklet do not conserve frame order... loss of timline information. 


##############################
# ROUGE bout length estimation 
##############################


# importation de tous les fichier NA filter du dossier ---- 
files_names <- list.files(here("./Data_larvae/Res_orange_1fps/NA_filtered_data/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv.gz")
}

list_file <- vector("list", length(files_names))
for (i in 1:nb_files) {
  list_file[[i]] <- data.table::fread(
    paste(here("./Data_larvae/Res_orange_1fps/NA_filtered_data/", files_names[i])),
    sep = ";",
    dec = ".")
}


# selection des ID retenu a la fin ou dans le script d'analyse des paramètres. 

# # selected individual for R08 
# ID_selected <- read.csv("../ID_selected_MoveR.csv")
# ID_rpi108 <- ID_selected %>% filter(str_detect(fullID3, ".108"))
# selIDrpi108 <- as.character(ID_rpi108$fullID3)

# Convert the data to a list of tracklets - here the data were saved as a varList object (a list of variable) a more simple structure than tracklets
list_res_bout<-vector("list", length(list_file))

##########################################
## RUN

for (n in 1:length(list_file)){
  print(n)
  #convert to tracklet
  trackDat<- MoveR::convert2Tracklets(list_file[[n]], by = "identity")
  
  # compute speed, mean speed, speed variance and Time in second 
  trackDat<-
    try(MoveR::analyseTracklets(trackDat,
                            customFunc = list(
                              speed = function(x)
                                MoveR::speed(x,
                                             timeCol = "frame",
                                             #scale = 1/37,# for in cm by second
                                             frameR=1, 
                                             timeU="s"), 
                              ### speed
                              slideMeanSpeed = function (y)
                                MoveR::slidWindow(y$speed,
                                                  Tstep = 1, 
                                                  statistic = "mean",   
                                                  na.rm = T),
                              ### speed variance
                              slideVarSpeed = function (y)
                                MoveR::slidWindow(y$speed,
                                                  Tstep = 1, 
                                                  statistic = "var", 
                                                  na.rm = T),
                              
                              TimeSec = function(x)
                                x[["frame"]] / 1,
              
                              activity1 = function(x)
                                MoveR::activity1(x,
                                                 speedCol = "speed",
                                                 minSpeed = 0.3),

                              MaxSpeed=function(x) max(x[["speed"]], na.rm=TRUE)
                            )))
  
  # Sampling tracklet to 1 second 
# trackDat_byS <-
#   try(MoveR::resampTracklets(trackDat, timeCol = "TimeSec",  Tstep = 1))
# 
# trackDat_byS<-
#   try(MoveR::analyseTracklets(trackDat_byS,
#                           customFunc = list(
#                             activity1 = function(x)
#                               MoveR::activity1(x, 
#                                                speedCol = "speed", 
#                                                minSpeed = 0.3), 
#                             MaxSpeed=function(x) max(x[["speed"]], na.rm=TRUE)
#                           )))

# apply split bout function 
res_Lb<-list()

df_res <- data.frame(Max_bout_activ=NA, 
                     Max_bout_inactiv=NA, 
                     Max_Speed=NA, fullID3=NA)

for (i in 1:length(trackDat)){
  res_Lb[[i]]<-try(split_by_repetition(trackDat[[i]]$activity1))
  df_res[i,] <- try(data.frame(Max_bout_activ=res_Lb[[i]][1], 
                               Max_bout_inactiv=res_Lb[[i]][2], 
                               Max_Speed=unique(trackDat[[i]]$MaxSpeed), 
                               fullID3=names(trackDat)[i]))
}
list_res_bout[[n]]<-df_res
}
summary(trackDat)

res_tab<- do.call(rbind, list_res_bout)

write.csv(res_tab, "./Data_larvae/Data_vid_rouge/res_bouts_Mspeed_rouge.csv")


#### TEST Error gestion ####


trackDat<- MoveR::convert2Tracklets(list_file[[1]], by = "identity")

trackDat<-
  MoveR::analyseTracklets(trackDat,
                          customFunc = list(
                            speed = function(x)
                              MoveR::speed(x,
                                           timeCol = "frame",
                                           # scale = 1/37,# for in cm by second
                                           frameR=5, 
                                           timeU="s"), 
                            ### speed
                            slideMeanSpeed = function (y)
                              MoveR::slidWindow(y$speed,
                                                Tstep = 5, 
                                                statistic = "mean",   
                                                na.rm = T),
                            ### speed variance
                            slideVarSpeed = function (y)
                              MoveR::slidWindow(y$speed,
                                                Tstep = 5, 
                                                statistic = "var", 
                                                na.rm = T),
                            TimeSec = function(x)
                              x[["frame"]] / 5,
                            activity1 = function(x)
                              MoveR::activity1(x, 
                                               speedCol = "speed", 
                                               minSpeed = 0.3), 
                            MaxSpeed=function(x) max(x[["speed"]], na.rm=TRUE)
                            
                          ))

trackDat_byS <-
  try(MoveR::resampTracklets(trackDat, timeCol = "frame",  Tstep = 5))

str(trackDat)
trackDat_byS<-
  try(MoveR::analyseTracklets(trackDat_byS,
                              customFunc = list(
                                activity1 = function(x)
                                  MoveR::activity1(x, 
                                                   speedCol = "speed", 
                                                   minSpeed = 0.3), 
                                MaxSpeed=function(x) max(x[["speed"]], na.rm=TRUE)
                              )))

res_Lb<-list()
df_res <- data.frame(Max_bout_activ=NA, Max_bout_inactiv=NA, Max_Speed=NA, fullID3=NA)

for (i in 1:length(trackDat)){
  res_Lb[[i]]<-try(split_by_repetition(trackDat[[i]]$activity1))
  df_res[i,] <- try(data.frame(Max_bout_activ=res_Lb[[i]][1]/5, 
                               Max_bout_inactiv=res_Lb[[i]][2]/5, 
                               Max_Speed=unique(trackDat[[i]]$MaxSpeed), 
                               fullID3=names(trackDat)[i]))
}

