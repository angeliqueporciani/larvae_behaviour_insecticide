# load packages ----
library(momentuHMM)
library(dplyr)
library(MoveR)
library(zoo)
library(here)
library(purrr)
library(tidyverse)
library(furrr)

# load data-----
ID_selected <- read.csv("./ID_selected_MoveR.csv")# ID avec moins de 20% de perte de tracking 

files_names <- list.files(here("./Data_larvae/data_larves_1hr/Named_corr_Coord/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}


for(i in 1: length(files_names)) { 
 # x <- read.csv2(here::here("data",files_list[i]))
  x <-  MoveR::readAnimalTA(paste(here("./Data_larvae/data_larves_1hr/Named_corr_Coord/", files_names[i])),rawDat = FALSE)
  assign(data_names[[i]], x)
}
rm(x)


# select 

ID_rpi211 <- ID_selected %>% filter(Replicat=="R11" & str_detect(fullID3, ".211"))
sel_ID_rpi211 <- as.character(ID_rpi211$fullID3)

rpi2_R11_selID <- Named_rpi2_10fps_R11_Corrected[sel_ID_rpi211]

# Creation of tibble by rpi and replicate-----

list_selID <- list()
for (i in 1:length(rpi2_R11_selID)){
  list_selID[[i]] <- rpi2_R11_selID[[i]]
}
#list_selID <- list_selID[-c(1,2)]
# name column creation 
funname <-  function(dfr){
  ID=unique(dfr$identity)
  return(ID)
}

# Apply 
tib_selID <- tibble(data=list_selID) %>% 
  mutate(rpi=rep("rpi2"),replicat=11, ID=map_chr(data,funname))

# Test on only one replicate 
#rpi1_R08dat <- read.csv2("./Data_larvae/data_larves_1hr/rpi1_10fps_R08_Corrected.csv")
# rpi1_R08dat2 <- MoveR::readAnimalTA("./Data_larvae/data_larves_1hr/rpi1_10fps_R08_Corrected.csv", rawDat = FALSE)
# ID_selected <- read.csv("./ID_selected_MoveR.csv")# ID avec moins de 20% de perte de tracking 
# ID_rpi108 <- ID_selected %>% filter(str_detect(fullID3, ".108"))# que pour le R08 rpi1
# selIDrpi108 <- as.character(ID_rpi108$fullID3)
# rpi1_R08dat2_selID <- rpi1_R08dat2[selIDrpi108]# subset des ID 

# select frame each second (rediscretization of the trajectory point)
# sub_list <- list()
# for (i in 1:length(ID_list)){
# sub_list[[i]] <-  ID_list[[i]] %>% 
#   slice(seq(1, nrow(ID_list[[i]]), by=10)) 
# }
# Transform in function as I no longer work with list 

fun_slicetime <- function(dfr){
  sub_dfr <-  dfr %>% slice(seq(1, nrow(dfr), by=5)) 
  
}

# apply 

tib_selID <- tib_selID %>% mutate(sub_datafr=map(data, fun_slicetime)) 

#str(tib_rpi1_R06[[1]][[1]])

# Function creation for applying CRCW 
# clean NA interpolation and replace NA in TA by last non null observation 

CRCW_HMMdata_fun <- function(datafr){
  temp_df <- datafr %>% mutate(Time=frame/10, ID=identity, x=x.pos, y=y.pos)
  crwOut1 <- crawlWrap(obsData=temp_df, Time.name = "Time", time.scale = "seconds",attempts=1)
  clean_test_ID <- data.frame(crwOut1$crwPredict) 
  clean_test_ID <- clean_test_ID%>%  dplyr::select(ID, mu.x, mu.y, Time)
  # preparation data for HMM format 
  datHMM=prepData(clean_test_ID, type="UTM", coordNames = c("mu.x","mu.y"))
  return(datHMM)
}

poss.CRCW.HMM = possibly(.f = CRCW_HMMdata_fun, otherwise = NA)

# estimation of speed and TA by trajR or HMM directly (same result)

tib_selID <- tib_selID %>% mutate(dataHMM=map(sub_datafr,poss.CRCW.HMM))

#head(tib_rpi1_R06$dataHMM[[1]])

# imputation of NA data of trajectories 
# crwOut1 <- crawlWrap(obsData=ID_test, Time.name = "Time", time.scale = "seconds",attempts=1)
# clean_test_ID <- data.frame(crwOut1$crwPredict) 
# clean_test_ID <- clean_test_ID%>%  dplyr::select(ID, mu.x, mu.y, Time)
# 
# # preparation data for HMM format 
# datHMM=prepData(clean_test_ID, type="UTM", coordNames = c("mu.x","mu.y"))

# replacement of NA in TA estimation by most non NA recent value 
#datHMM$angle <- c(NA, na.locf(datHMM$angle))

fun_angleNA <- function(dfr){
  dfr$angle <- c(NA, na.locf(dfr$angle))
  return(dfr)
}

fun_angleNA.2 <- function(dfr){
  dfr <- dfr %>% mutate(angle=c(NA, na.locf(dfr$angle)))
  return(dfr)
}

poss.angleNA.2 = possibly(.f = fun_angleNA.2, otherwise = NA)

tib_selID <- tib_selID %>% 
  filter(!is.na(dataHMM)) %>% 
  mutate(dataHMM2=map(dataHMM,poss.angleNA.2))

# peculiar case, need to be automatised when my brain are ready. 
tib_selID$dataHMM[[14]]$angle
tib_selID$dataHMM2[[14]] <- tib_selID$dataHMM[[14]] %>% 
  mutate(angle=c(NA, 0,na.locf(tib_selID$dataHMM[[14]]$angle)))

## Application of HMM on each individuals ---- 
### function HMM ----- 

HMM_vid.fun <- function(dfr.HMM){
  
  dist2 = list(step = "gamma", angle = "wrpcauchy")
  
  Par0 <- list(step=c(0.01,0.5,19,0.9,0.2, 0.2, 0.1, 0.1, 0.1), angle=c(1, 2,0.1,0.5, 0.2, 0.2))
  #gamma and wpcauchy 
  m1 <- fitHMM(data = dfr.HMM, nbStates = 3, dist = dist2, Par0 = Par0,
               estAngleMean = list(angle=TRUE))
  
  Par0_2 <- getPar0(model=m1, nbStates=3,
                    DM=list(step=list(mean=~1, sd=~1, zeromass=~1),angle=list(mean=~1, concentration=~1)),
                    estAngleMean=list(angle=TRUE),
                    circularAngleMean=list(angle=TRUE))
  
  m2 <- fitHMM(data = dfr.HMM, nbStates = 3, dist = dist2, Par0 = Par0_2$Par,
               estAngleMean = list(angle=TRUE), circularAngleMean = list(angle=TRUE), 
               DM=list(step=list(mean=~1, sd=~1, zeromass=~1),angle=list(mean=~1, concentration=~1)))
  return(m2)
}

poss.HMM.vid = possibly(.f = HMM_vid.fun, otherwise = NA)

tib_selID <- tib_selID  %>% mutate(HMM_fit=future_map(dataHMM2,poss.HMM.vid))   

saveRDS(tib_selID, "./Data_larvae/data_larves_1hr/Output/VID_HMM_rpi211.rds")

######################################################
# paramètre initiaux choix 
######################################################
summary(datHMM$step)

summary(datHMM$angle)
#choice of initial values based on data modality Median and max 
# or min(1stQ), median, max regarding of the number of states 

# Select HMM and apply to all individuals 
## Selection of distribution for TA 
dist1 = list(step = "gamma", angle = "vm")
stepM <- c(0.0001, 15)
stepSD <- c(0.000001, 5)
stepZM <- c(0.9, 0.1)
angleM <- c(0.001, 2)
angleCo <- c(3,1)

Par0_m1 <- list(step=c(stepM,stepSD,stepZM), angle=c(angleM, angleCo))

# distri von misses pour angle 
m1 <- fitHMM(data = datHMM, nbStates = 2, dist = dist1, Par0 = Par0_m1,
             estAngleMean = list(angle=TRUE))
m1


dist2 = list(step = "gamma", angle = "wrpcauchy")
angleM <- c(0.001, 2)
angleCo2 <- c(0.9,0.3)

Par0_m2 <- list(step=c(stepM,stepSD,stepZM), angle=c(angleM, angleCo2))

m2 <- fitHMM(data = datHMM, nbStates = 2, dist = dist2, Par0 = Par0_m2,
             estAngleMean = list(angle=TRUE))

m2

dist2b = list(step = "weibull", angle = "wrpcauchy")
stepSh_WB <- c(0.0001, 15)
stepSc <- c(2, 5)
stepZM <- c(0.9, 0.1)


Par0_m2b <- list(step=c(stepSh_WB,stepSc,stepZM), angle=c(angleM, angleCo2))

m2b <- fitHMM(data = datHMM, nbStates = 2, dist = dist2b, Par0 = Par0_m2b,
             estAngleMean = list(angle=TRUE))

m2b

AIC(m1, m2, m2b)

dist2c = list(step = "weibull", angle = "vm")
Par0_m2c <- list(step=c(stepSh_WB,stepSc,stepZM), angle=c(angleM, angleCo))

m2c <- fitHMM(data = datHMM, nbStates = 2, dist = dist2c, Par0 = Par0_m2c,
              estAngleMean = list(angle=TRUE))

m2c
AIC(m1,m2, m2b, m2c)

## choix du nombre d'état 

dist2b = list(step = "weibull", angle = "wrpcauchy")
stepSh_WB3 <- c(0.0001, 10, 15)
stepSc3 <- c(2, 3, 5)
stepZM3 <- c(0.9, 0.1, 0.1)
angleM3 <- c(0.001, 3, 1)
angleCo23 <- c(0.9,0.1,0.3)

Par0_m3 <- list(step=c(stepSh_WB3,stepSc3, stepZM3), angle=c(angleM3,angleCo23))

m3 <- fitHMM(data = datHMM, nbStates = 3, dist = dist2b, Par0 = Par0_m3,
             estAngleMean = list(angle=TRUE))

m3

AIC(m2b, m3)

plot(m3, ask = F)

Par0_m3b <- list(step=c(0.01,0.5,19,0.9,0.2, 0.2, 0.1, 0.1, 0.1), angle=c(1, 2,0.1,0.5, 0.2, 0.2))
#gamma and wpcauchy 
m3b <- fitHMM(data = datHMM, nbStates = 3, dist = dist2, Par0 = Par0_m3b,
             estAngleMean = list(angle=TRUE))

m3b
AIC(m2,m3, m3b)

plot(m3b, ask=F)

par_m3b <- getPar0(m3b)

m3c <- fitHMM(data = datHMM, nbStates = 3, dist = dist2, Par0 = par_m3b$Par,
             estAngleMean = list(angle=TRUE))

m3e
plot(m3c, ask=F)

m3d <- fitHMM(data = datHMM, nbStates = 3, dist = dist1, Par0 = par_m3b$Par,
              estAngleMean = list(angle=TRUE))
AIC(m1, m2, m3, m3b,m3c, m3d)# 3c le meilleur 

# distribution gamma and wrpcauchy choosen

# Test for estimating mean and concentration of TA with circular-circular regression instead of linear circular regression
# Allow to define angle as directionnal persistance instead of the value of angle and concentration. 

DM <- list(angle = list(mean = ~1, concentration= ~1))

par_m3c <- getPar0(m3b)
Par0 <- getPar0(model=m3b, nbStates=3,
                DM=list(step=list(mean=~1, sd=~1, zeromass=~1),angle=list(mean=~1, concentration=~1)),
                estAngleMean=list(angle=TRUE),
                circularAngleMean=list(angle=TRUE))

m3e <- fitHMM(data = datHMM, nbStates = 3, dist = dist2, Par0 = Par0$Par,
              estAngleMean = list(angle=TRUE), circularAngleMean = list(angle=TRUE), 
              DM=list(step=list(mean=~1, sd=~1, zeromass=~1),angle=list(mean=~1, concentration=~1)))

AIC(m3c, m3e)
m3d
plot(m3e,ask=F)
timeInStates(m3e)
timeInStates(m3c)

plotPR(m3c)
plotStates(m3d)


# switching state pb ---- 

lower_bounds <- list(step = c(0.0001, 0.5, 20))  # Limites inférieures
upper_bounds <- list(step = c(3, 20, 100))    # Limites supérieures

m3e <- fitHMM(data = datHMM, nbStates = 3, dist = dist2, Par0 = Par0$Par,
              estAngleMean = list(angle=TRUE), circularAngleMean = list(angle=TRUE), 
              DM=list(step=list(mean=~1, sd=~1, zeromass=~1),angle=list(mean=~1, concentration=~1)),
              estPar0 = list(
                lower = lower_bounds,  # Limites inférieures
                upper = upper_bounds   # Limites supérieures
              ))
m3e


## test with bounds 
nbStates <- 3
stepDM <- matrix(c(1,0,0,0,0,0,0,0,0,
                   0,1,1,0,0,0,0,0,0,
                   1,1,1,0,0,0,0,0,0,
                   0,0,0,1,0,0,0,0,0,
                   0,0,0,0,1,0,0,0,0,
                   0,0,0,0,0,1,0,0,0,
                   0,0,0,0,0,0,1,0,0,
                   0,0,0,0,0,0,0,1,0,
                   0,0,0,0,0,0,0,0,1),
                 nrow = 3*nbStates,
                 byrow = T,
                 dimnames = list(c(paste0("mu_", 1:nbStates),
                                   paste0("sd_", 1:nbStates),
                                   paste0("zeromass_", 1:nbStates)),
                                 c("wmu_13","wmu_2","wmu_3",
                                   paste0("wsd_", 1:nbStates),
                                   paste0("wzeromass_", 1:nbStates))))

stepDM<-matrix(c(1,0,0,0,0,0,0,0,0,
                 0,1,1,0,0,0,0,0,0,
                 1,1,1,0,0,0,0,0,0,
                 0,0,0,1,0,0,0,0,0,
                 0,0,0,0,1,0,0,0,0,
                 0,0,0,0,0,1,0,0,0,
                 0,0,0,0,0,0,1,0,0,
                 0,0,0,0,0,0,0,1,0,
                 0,0,0,0,0,0,0,0,1),nrow=3*nbStates,byrow=TRUE,
               dimnames=list(c(paste0("mu_",1:nbStates),paste0("sd_",1:nbStates),
                               paste0("zeromass_",1:nbStates)),
                             c(paste0("wmu_",1:nbStates,":(Intercept)"),
                               "wsd:(Intercept)","wsd_2","wsd_3",
                               paste0("zeromass_",1:nbStates,":(Intercept)"))))


stepworkBounds <- matrix(c(-Inf,Inf,
                           0,Inf,
                           0,Inf,
                           -Inf,Inf,
                           -Inf,Inf,
                           -Inf,Inf,
                           -Inf,Inf,
                           -Inf,Inf,
                           -Inf,Inf),nrow=ncol(stepDM),byrow = TRUE,dimnames=list(c("wmu_13","wmu_2","wmu_3",
                                                                                    paste0("wsd_", 1:nbStates),
                                                                                    paste0("wzeromass_", 1:nbStates)),
                                                                                  c("lower","upper")))


Par <- list(step=c(0.01,0.5,19,0.9,0.2, 0.2, 0.9, 0.1, 0.1), angle=c(0.9,0.5,0.001))
nbStates <- 3
stateNames <- c("resting", "foraging", "transit")
dist <- list(step = "gamma", angle = "wrpcauchy")

### construct pseudo-design matrix constraining parameters (to avoid label switching across imputations)
# constrain step length mean parameters: transit > resting
# stepDM<-matrix(c(1,1,0,0,0,0,
#                  0,0,1,0,0,0,
#                  1,0,0,0,0,0,
#                  0,0,0,1,0,0,
#                  0,0,0,0,1,0,
#                  0,0,0,0,0,1, 
#                  0,0,0,1,0,0,
#                  0,0,0,0,1,0,
#                  0,0,0,0,0,1),3*nbStates,9,byrow=TRUE)
#                dimnames=list(c(paste0("mean_",1:nbStates),paste0("sd_",1:nbStates), paste0("zeromass_", 1:nbStates)),
#                              c("mean_13:(Intercept)","mean_1","mean_2:(Intercept)",
#                                paste0("sd_",1:nbStates,":(Intercept)"), 
#                                paste0("zeromass_",1:nbStates,":(Intercept)"))))

stepworkBounds<-matrix(c(-Inf, 0,0,rep(-Inf,6),
                         rep(Inf,ncol(stepDM))),ncol(stepDM),2,
                       dimnames=list(colnames(stepDM),c("lower","upper")))
# constrain turning angle concentration parameters: transit > resting
angleDM<-matrix(c(1,1,0,
                  0,0,1,
                  1,0,0),nbStates,3,byrow=TRUE,
                dimnames=list(paste0("concentration_",1:nbStates),
                              c("concentration_13:(Intercept)","concentration_1","concentration_2:(Intercept)")))

angleworkBounds <- matrix(c(-Inf,-Inf,-Inf,Inf,0,Inf),3,2,dimnames=list(colnames(angleDM),c("lower","upper")))

angleDM <- matrix(c(1,0,0,
                    0,1,1,
                    0,1,0),
                  nrow=nbStates,
                  byrow=TRUE,
                  dimnames=list(paste0("concentration_",1:nbStates),
                                c("concentration_1:(Intercept)","concentration_23:(Intercept)",
                                  "concentration_2")))

DM <- list(step=stepDM, angle=angleDM)

workBounds <- list(step=stepworkBounds, angle=angleworkBounds)

Par0 <- getParDM(nbStates = 3, dist = list(step = "gamma", angle = "wrpcauchy"),
                 Par = Par, DM = DM)
                 estAngleMean = list(angle = TRUE))

#fixPar <- list(dive = c(-100, NA, NA))

# set prior to help prevent working parameters from straying along boundary
prior <- function(par){sum(dnorm(par,0,10,log=TRUE))}

nfsFits_WB <- fitHMM(datHMM, nbStates = nbStates, dist = dist,
                    Par0 = Par, 
                    #DM = DM, 
                    estAngleMean = list(angle = FALSE), 
                    #retryFits = 5,
                    prior = prior, 
                    workBounds = workBounds,
                    stateNames=stateNames) 

nfsFits_WB
plotPR(nfsFits_WB)

nfsFits_DM <- fitHMM(datHMM, nbStates = nbStates, dist = dist,
                     Par0 = Par, 
                     DM = DM, 
                     estAngleMean = list(angle = FALSE), 
                     #retryFits = 5,
                     prior = prior, 
                     #workBounds = workBounds,
                     stateNames=stateNames) 

nfsFits_DM
plotPR(nfsFits_DM)


nfsFits_B <- fitHMM(datHMM, nbStates = nbStates, dist = dist,
                     Par0 = Par, 
                     DM = DM, 
                     estAngleMean = list(angle = FALSE), 
                     #retryFits = 5,
                     prior = prior, 
                     workBounds = workBounds,
                     stateNames=stateNames) 

nfsFits_B
plotPR(nfsFits_B)


# Automatisation to all individuals selected -----

tib_sub <- tibble(data=sub_list)

# function for prep data before applied HMM ---- 
crw.prep.data <- function(df){

  data_temp <- df %>% mutate(Time=frame/10, ID=identity, x=x.pos, y=y.pos)
  crwOut <- try(crawlWrap(obsData=data_temp, Time.name = "Time", time.scale = "seconds",attempts=1))
  
  return(crwOut)
}

prep.data.HMM <- function(dfcrw){
  dat_CRW <- try(data.frame(dfcrw$crwPredict) %>%  dplyr::select(ID, mu.x, mu.y, Time))
  datHMM <- try(prepData(dat_CRW, type="UTM", coordNames = c("mu.x","mu.y")))
}


datHMM$angle <- try(c(NA, na.locf(datHMM$angle)))# pas sure que ça soit pertinent pour le modèle finalement 

# function HMM 
hmm.fit <- function(dataHMM)
{
dist2 = list(step = "gamma", angle = "wrpcauchy")
Par0_m3 <- list(step=c(0.01,0.5,19,0.02,0.2, 10, 0.1, 0.1, 0.1), angle=c(1, 2,0.1,0.3, 0.7, 0.1))

m3 <- try(fitHMM(data = dataHMM, nbStates = 3, dist = dist2, Par0 = Par0_m3,
             estAngleMean = list(angle=TRUE)))

par_m3 <- try(getPar0(m3))

m3b <- try(fitHMM(data = dataHMM, nbStates = 3, dist = dist2, Par0 = par_m3$Par,
              estAngleMean = list(angle=TRUE)))
return(m3b)
}

# CRW for imputation of missed data 
tib_sub <- tib_sub %>% mutate(data_crw=map(data, crw.prep.data))
# prep data for hmm
tib_sub <- tib_sub %>% mutate(data_HMM_cleand=map(data_crw, prep.data.HMM))
# HMM 3 states on all individual 

tib_sub <- tib_sub %>% mutate(HMM_mod=map(data_HMM_cleand,hmm.fit))

fun.step.est <- function(hmmmod){tryCatch(as.data.frame(hmmmod$CIreal$step$est),
                                          error = function(e) NA)}
fun.angle.est <- function(hmmmod){tryCatch(as.data.frame(hmmmod$CIreal$angle$est), error = function(e) NA)}

fun.ID <- function(df){unique(df$identity)}
as.data.frame(m3$CIreal$step$lower)

tib_sub <- tib_sub %>% mutate(Step_est=map(HMM_mod,fun.step.est), 
                              Angle_est=map(HMM_mod,fun.angle.est),
                              fullID3=map(data, fun.ID))

step_df <- tib_sub %>% dplyr::select(Step_est, fullID3) %>% 
  filter(Step_est!="NA") %>% 
  unnest(cols=c(Step_est, fullID3))

plot(step_df$`state 1`<0.000001)


angle_df <- tib_sub %>% dplyr::select(Angle_est, fullID3) %>% 
 filter(Angle_est!="NA") %>% 
  unnest(cols=c(Angle_est, fullID3))
