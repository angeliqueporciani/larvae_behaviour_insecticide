# Test HMM on sample of 10 min 
# load package, data and a little bit of DM ---- 
library(dplyr)
library(tidyverse)
library(momentuHMM)

dataATA <- read.delim("./Name_corr_data/rpi1_10fps_R09_Corrected.csv",sep=";")
dataCM <- read.csv("./temp_Dat_sec.csv") %>% select(!c(1)) %>% drop_na()# data calculated from MoveR functions 

datalong <- dataATA %>% pivot_longer(
  !c(1, 2),
  names_to = c("Axe", "Arena", "Ind"),
  names_pattern = "(.*)_(.*)_(.*)",
  values_to = "posXY")

R0903A1=datalong %>% filter(Ind=="11R0903A1") %>% pivot_wider(
  names_from = c(Axe),
  values_from = posXY
)


# Prep data for package MomentuHMM ---- 

dat=prepData(dataCM, coordNames = NULL)
hist(dat$MeanSpeed)
hist(dat$MeanAngle)
plot(dat$MeanSpeed, type="l")
plot(dat$MeanAngle, type="l")
summary(dataCM)
head(dat)
dist = list(MeanSpeed = "norm", MeanAngle = "vm")
Par0_m1 <- list(MeanSpeed=c(0.1,0.7,0.2,0.8), MeanAngle=c(0.1, 8))

m1 <- fitHMM(data = dat, nbStates = 2, dist = dist, Par0 = Par0_m1,
             estAngleMean = list(angle=TRUE))
summary(m1)
m1
plot(m1)

# test avec plus de variable sur 5min 

dataHMM1 <- read.delim("./Results_data_ATA/Results/Detailed_data/rpi1_10fps_R09.avi/Arena_23Ind0.csv", sep=";")
sub5min <- dataHMM1 %>% filter(Time<300)
plot(sub5min$Speed, type="l")

#juste test HMM ()

subHMM_data <- slice(dataHMM1, seq(1, nrow(dataHMM1), 5)) %>% drop_na()

plot(subHMM_data$Dist_to_All_borders_1, type="l")
hist(subHMM_data$Dist_to_All_borders_1)

dat2=prepData(subHMM_data, coordNames = NULL) %>% select(c("ID", "Time", "Speed","Distance", "Dist_to_All_borders_1"))
plot(dat2$Speed, type="l")
hist(dat2$Speed)


plot(dat2$Distance, type="l")
hist(dat2$Distance)

# calcul moyenne mobile pour reduire la frequence et eviter les NA (HMM n'aime pas les NA) 
library(xts)
library(forcats)
sub5min <- sub5min %>% 
  mutate(MA=stats::filter(sub5min$Speed, filter=rep(1/10, 10), 
                                               method=c("convolution"),sides=2, circular=FALSE))

ggplot(sub5min)+
  aes(Time,Speed)+
  geom_line(alpha = 1/4) +
  geom_line(aes(y=MA))
dat2=prepData(sub5min, coordNames = NULL)

naLoc <- ifelse(is.na(sub5min$Speed), 1, 0)

Tstep=10
  smoothed <- stats::filter(sub5min$Speed, filter = rep(1/Tstep, Tstep), 
                            method = "convolution", circular = F, sides = 2)
  
plot(smoothed)  
  if (isTRUE(na.rm)) {
    smoothedNaCount <- stats::filter(naLoc, rep(1, Tstep), 
                                     sides = 2)
    smoothed <- smoothed * (Tstep/(Tstep - smoothedNaCount))}

# test HMM sur data olivier sans NA (15 min de recording 

dataOl <- read.csv2("./data_olivier/Results/Detailed_data/20230209-140234-C003i_ZEB772_0001.avi/Arena_0Ind0.csv")


plot(dataOl$Time, dataOl$Speed, type="l")
plot(dataOl$Time, dataOl$Dist_to_Tous_bords_0, type="l")

dataOL_sub <- dataOl %>% select(c(1:3))
dataOL_sub <- dataOL_sub %>% mutate_if(is.character, as.numeric)
str(dataOL_sub)
# use trajr to estimate turnAngle 
trj <- TrajFromCoords(
  dataOL_sub,
  xCol = 2,
  yCol = 3,
  timeCol = "Time",
  fps = 30,
  spatialUnits = "cm",
  timeUnits = "fps"
)
plot(trj)

trj <- trajr::TrajFromCoords(df[, c("x.pos", "y.pos", timeCol)], 
                             timeCol = 3, spatialUnits = "NA", timeUnits = "NA")
trj <- trajr::TrajScale(trj, scale, units = "NA")
turnAngle <- trajr::TrajAngles(trj, 
                               lag = 1)
