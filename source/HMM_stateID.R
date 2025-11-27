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

R0903A1_HMM <- dataATA %>% select(c(2:4)) %>%  
  slice(seq(1, nrow(dataATA), by=10)) 
colnames(R0903A1_HMM) <- c("Time", "X", "Y")

R0903A1_HMM2 <- R0903A1_HMM %>% mutate(X2=X/37, Y2=Y/37)

# Prep data for package MomentuHMM ---- 

# Goal : comparaison distribution et plot de vitesse calculée par trajR/moveR and 
# step length calculé par prepData  

## en pixel 
dat2=prepData(R0903A1_HMM,coordNames = c("X","Y"))
hist(dat2$step/37*10)
hist(dat2$step/37)

plot(dat2$step, type="l")
plot(dat2$step/37, type="l")

# en mm 
hist(dat$MeanSpeed*10)
plot(dat$MeanSpeed*10, type="l")

hist(dat2$angle)

# en cm 
dat=prepData(dataCM, coordNames = NULL)
hist(dat$MeanSpeed)
hist(dat$MeanAngle)

plot(dat$MeanSpeed, type="l")
plot(dat$MeanAngle, type="l")
summary(dataCM)

# Goal : test HMM on step length in pixel and in mm. 
dat2=prepData(R0903A1_HMM,coordNames = c("X","Y"))
dat3=prepData(R0903A1_HMM2,coordNames = c("X2","Y2"))

# plot 
plot(dat2$step, type="l")
plot(dat2$step/37, type="l")
plot(dat3$step, type="l")
# same same 

# HMM now 
## HMM on px and step only 

dat2 <- dat2 %>% mutate(step=c(na.locf(step)))
                        
summary(dat2$step)
str(dat2$step)
dist_m1 = list(step = "gamma")
Par0_m1 <- list(step=c(0.01,10, 0.001,0.3, 0.5, 0.1))

m1 <- fitHMM(data = dat2, nbStates = 2, dist = list(step = "gamma"), 
             Par0 = Par0_m1)
             #estAngleMean = list(angle=TRUE))
m1
plot(m1)

## HMM on cm and step only 
dat3 <- dat3 %>% mutate(step=c(na.locf(step)))

summary(dat3$step)
str(dat3$step)
dist_m2 = list(step = "gamma")
Par0_m2 <- list(step=c(0.001,0.25, 0.0001,0.03, 0.01, 0.8))

m2 <- fitHMM(data = dat3, nbStates = 2, dist = list(step = "gamma"), 
             Par0 = Par0_m2)

m2
plot(m1)
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

dataOl <- read.csv2("./data_olivier/Results/Detailed_data/20230209-140234-C003i_ZEB772_0001.avi/Arena_0Ind0.csv") %>% 
  mutate_if(is.character, as.numeric)

str(dataOl)

plot(dataOl$Time, dataOl$Speed, type="l")
plot(dataOl$Time, dataOl$Dist_to_Tous_bords_0, type="l")

library(trajr)
str(dataOl)

trj <- TrajFromCoords(
  dataOl,
  xCol = 2,
  yCol = 3,
  timeCol = "Time",
  fps = 30,
  spatialUnits = "cm",
  timeUnits = "fps"
)
plot(trj)
str(trj2)
trj2 <- TrajFromTrjPoints(trj, c(1, which(Mod(trj$Distance) >= 0.3)))
plot(trj2)

plot(trajr::TrajAngles(trj2,
                  lag = 1))

trj2$Tangle<-trajr::TrajAngles(trj2,
                               lag = 1)

# essai d'uniformiser le calcul du TA par seconde 

resampled <- TrajResampleTime(trj, 1)
plot(resampled$polar)


par(mar = c(5, 4, .5, .5))
plot(trj, lwd = 2)
lines(trj, pch = 16)
points(resampled, col = "red", draw.start.pt = FALSE)


TAresampled<-trajr::TrajAngles(resampled,
                               lag = 1)

TAresampled<-c(0,0, TAresampled)

length(TAresampled)

derivresample<-TrajDerivatives(resampled)
speedreS<-derivresample$speed
speedreS<-c(0,speedreS)
Time<-derivresample$speedTimes
Time<-c(0, Time)

df_HMM_OR<-data.frame(Time=Time, Speed=speedreS, TurnAngle=TAresampled)# replacer les NA par des 0 
hist(df_HMM_OR$Speed)


plot(Speedresample$acceleration ~ Speedresample$accelerationTimes, type = 'l', col = 'red', 
     yaxt = 'n',
     xlab = 'Time (s)',
     ylab = expression(paste('Change in speed (', m/s^2, ')')))
axis(side = 2, col = "red")
lines(Speedresample$speed ~ Speedresample$speedTimes, col = 'blue')
axis(side = 4, col = "blue")
mtext('Speed (m/s)', side = 4, line = 3)
abline(h = 0, col = 'lightGrey')

datHMM=prepData(df_HMM_OR, coordNames = NULL)

dist = list(Speed = "norm", TurnAngle = "vm")
Par0_m1 <- list(Speed=c(0.0001,0.2,0.0002,0.5), TurnAngle=c(0.1, 1))

m1 <- fitHMM(data = datHMM, nbStates = 2, dist = dist, Par0 = Par0_m1,
             estAngleMean = list(angle=TRUE))
m1
plot(m1)
plotSat(m1)

