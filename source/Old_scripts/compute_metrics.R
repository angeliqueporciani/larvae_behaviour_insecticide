# Metrics computation 
# Test des metriques pouvant être generée par MoveR en plus de celle fournies par AnimalTA. 
# liste des metriques complete sur Notion. 

# load package 
library(MoveR)
library(dplyr)

#library(tidyverse)

#load data for 1 rpi and 1 replicate (here R09)
rpi1_R09dat <- MoveR::readAnimalTA("../Name_corr_data/rpi1_10fps_R09_Corrected.csv", rawDat = FALSE)
trackDat <- MoveR::setInfo(rpi1_R09dat, frameR = 10, scale = 1/37, imgRes = c(720, 1080)) 
rpi1_1109031 <- trackDat[4]

ID_selected <- read.csv("/home/angelique/ownCloud/Indeed/ID_selected_MoveR.csv")
ID_rpi108 <- ID_selected %>% filter(str_detect(fullID3, ".108"))
selIDrpi108 <- as.character(ID_rpi108$fullID3)
# load filtered data from previous script 02_filterCleanDataset
rpi1_R08C<-data.table::fread(
  "../Data_larvae/data_larves_1hr/rpi1_10fps_R08_Corrected_cleanedData.csv.gz",
  sep = ";",
  dec = ".")

# convert the data to a list of tracklets - here the data were saved as a varList object (a list of variable) a more simple structure than tracklets
trackDat_R08_Cleaned <- MoveR::convert2Tracklets(rpi1_R08C, by = "identity")
names(trackDat_R08_Cleaned)

# set some useful information
trackDat_R08_Cleaned <- MoveR::setInfo(trackDat_R08_Cleaned, frameR = 10, scale = 1/37, imgRes = c(720, 1080)) 
str(trackDat_R08_Cleaned)

MoveR::drawTracklets(trackDat_R08_Cleaned,
                     imgRes = c(1080, 720),
                     selId = selIDrpi108,
                     timeWin = list(c(0, 3000)), 
                     #colGrad = viridis::viridis(10))
                     colId = "identity", 
                     legend=F)



tst_sub<-slice(trackDat_R08_Cleaned[[1]] , seq(1, nrow(trackDat_R08_Cleaned[[1]] ), 10))

list_trkdata<-MoveR::convert2List(trackDat_R08_Cleaned, by = "identity")

tst_tracklet<-  MoveR::analyseTracklets(trackListSampled1S,
                                        customFunc = list(
                                          speed = function(x)
                                            MoveR::speed(x,
                                                         timeCol = "frame",
                                                         scale = 1)))

# sampling to 1 second before computing speed and TA (limits NA and provide constant time step)

# convert the time unit (frame) to seconds using analyseTracklets (according to a frame rate of 25 fps)
trackListV1 <-
  MoveR::analyseTracklets(trackDat_R08_Cleaned,
                          customFunc = list(
                            # convert the time expressed in frame in second using a conversion factor of 25 frame per second
                            TimeSec = function(x)
                              x[["frame"]] / 10
                          ))


# resample the tracklets every 1 seconds
## check the size of the tracklets
trackSize <- unlist(lapply(trackListV1, function(x)
  nrow(x)))

## resample the tracklets every 1 seconds
trackListSampled1S <- MoveR::resampTracklets(trackListV1,
                                             timeCol = "TimeSec",
                                             Tstep = 1)

str(trackListSampled1S)

## check the size of the tracklets after resampling
trackSize1s <- unlist(lapply(trackListSampled1S, function(x)
  nrow(x)))

## Compare the tracklets size
cbind(trackSize, trackSize1s)

#draw new tracklet 
MoveR::drawTracklets(trackListSampled1S,
                     imgRes = c(1080, 720),
                     #selId = selIDrpi108,
                     timeWin = list(c(0, 3000)), 
                     #colGrad = viridis::viridis(10))
                     colId = "identity", 
                     legend=F)

names(trackListSampled1S)

# specify a list of custom function to smooth the existing metrics over time----
customFuncList <- list(
  
  ### speed
  slideMeanSpeed = function (x)
    MoveR::slidWindow(x$speed,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T),
  ## turning angles
  slideMeanAngle = function (x)
    MoveR::slidWindow(x$turnAngle,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T)
)
head(trackDat3)

# compute the smoothed metrics over time
TemporalRes_wtd_sec <- MoveR::temporalTrend(
  tst_tracklet,
  timeCol = "frame",
  Tstep =  1,
  sampling =  1,
  customFunc = customFuncList,
  wtd = TRUE
)

# 1. Speed threshold for class active or inactive 
#To do that, we can plot the distribution of particles’ speed and find the valley 
# in the bimodal distribution. 

# compute speed, turn angle and traveled distance for each frame 
trackDat2 <-
  MoveR::analyseTracklets(tst_tracklet,
                          customFunc = list(
                            speed = function(x)
                              MoveR::speed(x,
                                           timeCol = "frame",
                                           scale = 1),
                            turnAngle = function(x)
                              MoveR::turnAngle(
                                x,
                                unit = "radians",
                                timeCol = "frame",
                                scale = 1)
                            # distTraveled = function(x)
                            #   MoveR::distTraveled(x, 
                            #                       step = 1)
                            ))


# plot the distribution of particles' speed
## retrieve the log-transformed speed over all tracklets
speedLog <- log10(MoveR::convert2List(trackDat2)$speed)

## computes kernel density estimates of the distribution and plot it
speedLogDens <- data.frame(stats::density(speedLog, na.rm = T)[c(1,2)])
plot(speedLogDens, type = "l")

# find the valley between the two peak of the bimodal distribution using the cardidates package
## if needed install the package install.packages("cardidates")
## identify the peaks of the bimodal distribution and keep only the data between the peaks
peaks <- cardidates::peakwindow(speedLogDens)$peaks$x
speedLogDensReduced <- speedLogDens[-which(speedLogDens$x < peaks[1] | speedLogDens$x > peaks[2]),]

## the valley correspond to the local minimum between the peaks
valley <- speedLogDensReduced$x[which.min(speedLogDensReduced$y)]

## add the peaks and the valley to the plot
abline(v = peaks, col="darkgreen")
abline(v = valley, col="firebrick")

## as the the data were log-transformed we need to back-transform the value of the valley to find the threshold to determine the activity state

activTresh <- 10^valley
activTresh # 1.549865
#We have found the threshold above which the particles are considered as active.

# Functions list ---- 
# Specify the batch of function to pass to the analyseTracklet function for metric computation over each tracklet
customFuncList_smooth = list(
  ## compute
  ###activity
  activity1 = function(x)
    MoveR::activity1(x, 
                     speedCol = "speed", 
                     minSpeed = activTresh),
  ###sinuosity
  sinuosity = function(x)
    MoveR::sinuosity(x, 
                     timeCol = "frame", 
                     scale = 1),
  # Smooth
  ## turning angles
  slideMeanAngle = function (y)
    MoveR::slidWindow(y$turnAngle,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T),
  
  ### speed
  slideMeanSpeed = function (y)
    MoveR::slidWindow(y$speed,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T),

  ### smooth traveled distance
  slideMeanTraveledDist = function (y)
    MoveR::slidWindow(y$distTraveled,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T)
    )


split_by_repetition <- function(vec) {
  # Identify the indices where consecutive numbers are broken or a repetition changes
  breaks <- c(0, which(diff(vec) != 0), length(vec))
  
  # Use those break points to split the vector into sublists
  result <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
    vec[(breaks[i] + 1):breaks[i + 1]]
  })
  
  return(result)
}


# computation of metrics for 1 individual -----

trackDat3 <-
  MoveR::analyseTracklets(trackDat2,
                          customFunc = customFuncList_smooth)


splited_bout<-split_by_repetition(trackDat3[[1]]$activity1)

filtered_bout_inactif <- Filter(function(x) all(x == 0), splited_bout)
Lbout_inactif <- lapply(filtered_bout_inactif, length) 
res_length_bout1 <-do.call(rbind,Lbout_inactif) 
max(res_length_bout1)#2836 frame = 283.6 seconde, 5 min environ. 


head(trackDat3[[1]], 20)
summary(trackDat3[[1]])

plot(trackDat3[[1]]$frame, type="l",trackDat3[[1]]$slideMeanSpeed)
par(mfrow=c(1,1))

Ht = circular::circular(
  MoveR::convert2List(trackDat3 )[["TurnAngle"]],
  type = "angle",
  units = "radians",
  zero = 0
)
# taille des bout d'activité basé sur l'estimation de l'activité basé sur la vitesse 


# specify a list of custom function to smooth the existing metrics over time----
customFuncList <- list(

  ### speed
  slideMeanSpeed = function (x)
    MoveR::slidWindow(x$speed,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T),
  ## turning angles
  slideMeanAngle = function (x)
    MoveR::slidWindow(x$turnAngle,
                      Tstep = 10,
                      statistic = "mean",
                      na.rm = T)
)
head(trackDat3)

# compute the smoothed metrics over time
TemporalRes_wtd_sec <- MoveR::temporalTrend(
  trackDat3,
  timeCol = "frame",
  Tstep = 10 * 1,
  sampling = 10 * 1,
  customFunc = customFuncList,
  wtd = TRUE
)

temp_Dat_sec=do.call(cbind, TemporalRes_wtd_sec)
plot(temp_Dat_sec$MeanSpeed)
max(temp_Dat_sec$MeanSpeed)
colnames(temp_Dat_sec)=c("MeanSpeed", "Time", "nbTracklet", "MeanAngle", "Frame2", "nbTracklet2")
temp_Dat_sec=temp_Dat_sec[,c(1,2,4)] 
temp_Dat_sec=temp_Dat_sec%>% mutate(ID=rep("11R0903A1"))
write.csv(temp_Dat_sec,"./temp_Dat_sec.csv")
#smooth by minutes
TemporalRes_wtd_min <- MoveR::temporalTrend(
  trackDat3,
  timeCol = "frame",
  Tstep = 60 * 10,
  sampling = 60 * 10,
  customFunc = customFuncList,
  wtd = TRUE
)

temp_Dat_min=do.call(cbind, TemporalRes_wtd_min)

# plot the result
par(mfrow = c(2, 4))
for (p in seq_along(TemporalRes_wtd_min)) {
  plot(
    NULL,
    ylim = c(round(
      min(TemporalRes_wtd_min[[p]][, 1], na.rm = T),
      digits = 1
    ),
    round(
      max(TemporalRes_wtd_min[[p]][, 1], na.rm = T),
      digits = 1
    )),
    xlim = c(
      min(TemporalRes_wtd_min[[p]]$frame, na.rm = T),
      max(TemporalRes_wtd_min[[p]]$frame, na.rm = T)
    ),
    main = names(TemporalRes_wtd_min)[p],
    ylab = names(TemporalRes_wtd_min)[p],
    xlab = "Time (frames)"
  )
  lines(TemporalRes_wtd_min[[p]][, 1] ~ TemporalRes_wtd_min[[p]]$frame , col = "darkred")
}


## test avec data d'olivier sans NA 

dataOR <- MoveR::readAnimalTA("./data_olivier/corrected_coordinates/20230209-140234-C003i_ZEB772_0001_Corrected.csv", rawDat = FALSE)
MoveR::drawTracklets(dataOR)

trackDat2 <-
  MoveR::analyseTracklets(dataOR,
                          customFunc = list(
                            speed = function(x)
                              MoveR::speed(x,
                                           timeCol = "frame",
                                           scale = 1),
                            turnAngle = function(x)
                              MoveR::turnAngle(
                                x,
                                unit = "radians",
                                timeCol = "frame",
                                scale = 1),
                            distTraveled = function(x)
                              MoveR::distTraveled(x, 
                                                  step = 1)
                          ))

df_dataOR <-as.data.frame(dataOR[[1]][,c(4,5,7)])
  
  str(df_dataOR)
MoveR::turnAngle(df_dataOR,
                 timeCol = "frame",
                 unit = "radians",
                 scale = 1)
library(trajr)

trajr::TrajAngles(df_dataOR,
                  lag = 1)
