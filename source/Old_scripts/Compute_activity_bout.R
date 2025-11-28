# Estimates activity and bouts of activity for estimating the lenghs of resting time 
# and max speed maybe to correspond to Lutz paper 
library(MoveR)
library(readr)
library(tidyverse)

# 1.Filter NA : other script, application of NA filter (loss of tracking) Import data already cleaned -----
# Pb with this approach : separation by tracklet do not conserve frame order... loss of timline information. 
rpi1_R08C<-data.table::fread(
  "./Data_larvae/data_larves_1hr/rpi1_10fps_R08_Corrected_cleanedData.csv.gz",
  sep = ";",
  dec = ".")

# Import tracking data from Animal TA directly 

#rpi1_R08C<-MoveR::readAnimalTA("./Data_larvae/data_larves_1hr/rpi1_10fps_R08_Corrected.csv", rawDat = FALSE)

# selected individual for R08 
ID_selected <- read.csv("../ID_selected_MoveR.csv")
ID_rpi108 <- ID_selected %>% filter(str_detect(fullID3, ".108"))
selIDrpi108 <- as.character(ID_rpi108$fullID3)

# Select only individual that are alive until LAM 

rpi1_R08C_sub<- rpi1_R08C %>% filter(identity %in% selIDrpi108)
#trackDat_R08_sub<- rpi1_R08C[selIDrpi108]

# Convert the data to a list of tracklets - here the data were saved as a varList object (a list of variable) a more simple structure than tracklets

trackDat_R08_sub_Cleaned <- MoveR::convert2Tracklets(rpi1_R08C_sub, by = "identity")

# set some useful information
trackDat_R08_sub_Cleaned <- MoveR::setInfo(trackDat_R08_sub_Cleaned, frameR = 10, scale = 1/37, imgRes = c(720, 1080)) 

# draw tracklet 
MoveR::drawTracklets(trackDat_R08_sub_Cleaned,
                     imgRes = c(1080, 720),
                     #selId = selIDrpi108,
                     timeWin = list(c(0, 3000)), 
                     #colGrad = viridis::viridis(10))
                     colId = "identity", 
                     legend=F)

# I want speed by second and there are two ways to do that. 
#First, 
# I estimate speed in frame by second on the complete dataset (10fps)
# and then I made a sampling. 
# Second : I made a sampling (rediscretization) and then, I apply an esimation of the speed. 
# I think the result will be different as the first method estimate the speed based on 0.1 second rate and the second on the distance run on 1 second. 
# by now, only the 1st method run because the second generate NA and therefore, the speed cannot be estimated later. 
# So I made a first try based on this method and the result could be discussed. For the definition of the threshold we need to take this into accound. 
# Maybe I could estimate a mean speed (moving average on 10 step) and then make the sampling ? 


# 2.1. Estimates of speed on 10fps 
# compute the speed of the particle along its trajectory,
# expressing the speed as cm/second
trackDat_R08_sub_Cleaned <-
  MoveR::analyseTracklets(trackDat_R08_sub_Cleaned,
                          customFunc = list(
                            speed = function(x)
                              MoveR::speed(x,
                                           timeCol = "frame",
                                           scale = 1/37,# for in cm by second
                                           frameR=10, 
                                           timeU="s"), 
                            ### speed
                            slideMeanSpeed = function (y)
                              MoveR::slidWindow(y$speed,
                                                Tstep = 10, 
                                                statistic = "mean", 
                                                na.rm = T),
                            ### speed variance
                            slideVarSpeed = function (y)
                              MoveR::slidWindow(y$speed,
                                                Tstep = 10, 
                                                statistic = "var", 
                                                na.rm = T)
                          ))



# quick visu 
plot(trackDat_R08_sub_Cleaned[[1]]$speed, type='l')
plot(trackDat_R08_sub_Cleaned[[1]]$slideMeanSpeed, type='l')

# 2.B. Resampling tracklet to 1 second 
trackDat_R08_sub_Cleaned<-
  MoveR::analyseTracklets(trackDat_R08_sub_Cleaned,
                          customFunc = list(
                            # convert the time expressed in frame in second using a conversion factor of 25 frame per second
                            TimeSec = function(x)
                              x[["frame"]] / 10
                          ))

# resample the tracklets every 1 seconds
trackDat_R08_sub_Cleaned_byS <-
  MoveR::resampTracklets(trackDat_R08_sub_Cleaned, timeCol = "frame",  Tstep = 10)

# NA en plein millieu ... Je ne comprend pas d'ou ils vient puisqu'ils sont censé avoir été supprimé ... 
trackDat_R08_sub_Cleaned_byS[[1]][134:140,]
filter(trackDat_R08_sub_Cleaned[[1]], frame%in%c(1340:1370))

# plot of estimated value of speed and Mean Speed 
plot(trackDat_R08_sub_Cleaned_byS[[1]]$slideMeanSpeed, type="l")
plot(trackDat_R08_sub_Cleaned_byS[[1]]$speed, type="l")

## check the size of the tracklets
trackSize <- unlist(lapply(trackDat_R08_sub_Cleaned, function(x)
  nrow(x)))


## check the size of the tracklets after resampling
trackSize1s <- unlist(lapply(trackDat_R08_sub_Cleaned_byS, function(x)
  nrow(x)))

## Compare the tracklets size
cbind(trackSize, trackSize1s)


# draw trajectory by second 

MoveR::drawTracklets(trackDat_R08_sub_Cleaned_byS,
                     timeCol = "TimeSec",
                     imgRes = c(1080, 720),
                     #selId = selIDrpi108,
                     timeWin = list(c(0, 300)), 
                     #colGrad = viridis::viridis(10))
                     colId = "identity", 
                     legend=F)


# 2.B. Speed estimation on 1second----
# convert the time unit (frame) to seconds using analyseTracklets (according to a frame rate of 10 fps)
trackListV1 <-
  MoveR::analyseTracklets(trackDat_R08_sub_Cleaned,
                          customFunc = list(
                            # convert the time expressed in frame in second using a conversion factor of 25 frame per second
                            TimeSec = function(x)
                              x[["frame"]] / 10
                          ))

# 2.B.resample the tracklets every 1 seconds-----
## check the size of the tracklets
trackSize <- unlist(lapply(trackListV1, function(x)
  nrow(x)))

## resample the tracklets every 1 seconds
trackListSampled1S <- MoveR::resampTracklets(trackListV1,
                                             timeCol = "frame",
                                             Tstep = 10)

## check the size of the tracklets after resampling
trackSize1s <- unlist(lapply(trackListSampled1S, function(x)
  nrow(x)))

## Compare the tracklets size
cbind(trackSize, trackSize1s)

which(trackListSampled1S[[1]][is.na(trackListSampled1S[[1]])])

trackListSampled1S <-
  MoveR::analyseTracklets(trackListSampled1S,
                          customFunc = list(
                            speed = function(x)
                              MoveR::speed(x,
                                           timeCol = "TimeSec",
                                           scale = 1)
                          ))

trackListSampled1S[[1]][135:140,]
trackListSampled1S[[1]][135,]


sum(is.na(trackListSampled1S[[1]]$speed))

# 3. activity based on estimated or calculated threshold ----

###activity
trackDat_R08_sub_Cleaned_byS<-
  MoveR::analyseTracklets(trackDat_R08_sub_Cleaned_byS,
                          customFunc = list(
                            activity1 = function(x)
                            MoveR::activity1(x, 
                                             speedCol = "speed", 
                                             minSpeed = 0.3), 
                            MaxSpeed=function(x) max(x[["speed"]], na.rm=TRUE)
                          ))

# seuil de 0.3 cm/s as in animalTA for now and corresponding to mean larval length
sum(is.na(trackDat_R08_sub_Cleaned_byS[[1]]$activity1))
# there is some NA I apply the method provided by gilestro team (ethoscopy)
# replacement with the previous values 

# 4. Extract bout length (transfo of tracklet in list or test if custom fonction can apply on tracklet...)

#function of bout extraction 
split_by_repetition <- function(vec) {
  if (sum(is.na(vec))>0){
    vec <- na.locf(vec)
  }
  if (sum(is.na(vec))==0){
    vec <- vec}
  # Identify the indices where consecutive numbers are broken or a repetition changes
  breaks <- c(0, which(diff(vec) != 0), length(vec))
  
  # Use those break points to split the vector into sublists
  result <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
    vec[(breaks[i] + 1):breaks[i + 1]]
  })
  
  # now keep only ones corresponding to 1 
  filtered_active <- Filter(function(x) all(x == 1), result)
  
  # same for 0 
  filtered_inactive <- Filter(function(x) all(x == 0), result)
  
  res_Lb1<-max(unlist(lapply(filtered_active, length)))
  res_Lb_in<-max(unlist(lapply(filtered_inactive, length)))
  return(c(res_Lb1, res_Lb_in))
}

# apply 
res_Lb<-list()
df_res <- data.frame(Max_bout_activ=NA, Max_bout_inactiv=NA, fullID3=NA)

for (i in 1:length(trackDat_R08_sub_Cleaned_byS)){
  res_Lb[[i]]<-split_by_repetition(trackDat_R08_sub_Cleaned_byS[[i]]$activity1)
  df_res[i,] <- data.frame(Max_bout_activ=res_Lb[[i]][1], Max_bout_inactiv=res_Lb[[i]][2], fullID3=names(trackDat_R08_sub_Cleaned_byS)[i])
}

# Traitement des NA => replacement par la précedente valeur 
#(discutable mais c'est ce qu'il font dans ethoscopy)
# a garder pour les HMM surtout pour les TAngle  

