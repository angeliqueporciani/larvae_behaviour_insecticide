# load packages ----
library(momentuHMM)
library(dplyr)
library(MoveR)
library(zoo)
library(here)
library(purrr)
library(tidyverse)
library(furrr)


# Test avec les data de rouge 

# critère de comparaison : nb d'état detecté + vitesse de compilation + pertinence 


# load data-----
files_names <- list.files(here("./Data_larvae/Data_vid_rouge/Named_Coordinates/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".csv")
}


for(i in 1: length(files_names)) { 
  # x <- read.csv2(here::here("data",files_list[i]))
  x <-  MoveR::readAnimalTA(paste(here("./Data_larvae/Data_vid_rouge/Named_Coordinates/", files_names[i])),rawDat = FALSE)
  assign(data_names[[i]], x)
}
rm(x)

# test du script sur 1 ID 

ID_test <- Named_rpi01_R03_02P_lot2_rouge_Coordinates[1][[1]] 
ID_test2 <- Named_rpi01_R03_02P_lot2_rouge_Coordinates[2][[1]] 

ID_test$frame2 <- seq(1,nrow(ID_test), 1)
ID_test$Time <- ID_test$frame2/5
ID_test2$frame2 <- seq(1,nrow(ID_test2), 1)
ID_test2$Time <- ID_test2$frame2/5

plot(ID_test$x.pos, ID_test$y.pos)
plot(ID_test2$x.pos, ID_test2$y.pos)

# slice each 5 rows to have 1 data per second 
ID_test2 <-  ID_test %>% slice(seq(0, nrow(ID_test), by=5)) 

# CRCW 

temp_df_l <- ID_test2 %>% mutate(ID=identity, x=x.pos, y=y.pos)
crwOut1_l <- crawlWrap(obsData=temp_df_l, Time.name = "Time", time.scale = "seconds", attempts=1)
clean_test_ID <- data.frame(crwOut1_l$crwPredict) 
res_CRWC <- clean_test_ID%>%  dplyr::select(ID, mu.x, mu.y, Time)
  
# preparation data for HMM format and estimation of step length by second  

datHMM_1ID=prepData(res_CRWC, type="UTM", coordNames = c("mu.x","mu.y"))

# calcul de la somme des pas par pas de temps de 1, 5, 20, 15, 30
n <- nrow(datHMM_1ID)

test_time <- c(1,5,10,15,30)
list_datHMM <- list()

for (i in 1:length(test_time)){
  
  datHMM_1ID$groupe <- ceiling(seq_len(n) / test_time[i])# creation des groupes
  
  resultat <- datHMM_1ID %>%
    group_by(groupe) %>%
    summarise(across(step, sum))#estimation de la somme par groupe 
  
  res_temp<- datHMM_1ID %>% slice(seq(0, nrow(datHMM_1ID), by=test_time[i]))
  
  list_datHMM[[i]] <- cbind(res_temp, resultat) %>% 
    dplyr::select(c(1,4,7,9)) %>% 
    mutate(step_cm=step/37, Time=groupe) %>% 
    dplyr::select(c(1,2,5)) %>% na.omit()
    list_datHMM[[i]] <- prepData(list_datHMM[[i]], coordNames = NULL)# prep pour HMM 
}


class(list_datHMM[[1]])

par(mfrow=c(3,2))
plot(list_datHMM[[1]]$step_cm, type="l")
plot(list_datHMM[[2]]$step_cm, type="l")
plot(list_datHMM[[3]]$step_cm, type="l")
plot(list_datHMM[[4]]$step_cm, type="l")
plot(list_datHMM[[5]]$step_cm, type="l")
par(mfrow=c(1,1))

#dist2 = list(step = "gamma")

Par0_1s <- list(list(step=c(0.01,mean(list_datHMM[[1]]$step_cm, na.rm = TRUE)
,max(list_datHMM[[1]]$step_cm, na.rm = TRUE),0.9,0.2, 0.2, 0.1, 0.1, 0.1)))

# Par0 <- list(step=c(0.01,0.5,19,0.9,0.2, 0.2, 0.1, 0.1, 0.1))
# Par0_15 <- list(step=c(0.01,0.5,40,0.9,0.2, 0.2, 0.1, 0.1, 0.1))
max(list_datHMM[[2]]$step_cm, na.rm = TRUE)
#gamma 1s
res_HMM_1 <- list()

res_HMM_2 <- list()
dist2 = list(step_cm = "gamma")

for (i in 2:length(list_datHMM)){
  print(i)
  
  Par0_list <- list(step_cm=c(0.0001,mean(list_datHMM[[i]]$step_cm, na.rm = TRUE)
                                ,max(list_datHMM[[i]]$step_cm, na.rm = TRUE),0.1,0.2, 0.2))
  
  res_HMM_1[[i]] <- fitHMM(data = list_datHMM[[i]], nbStates = 3, dist = dist2, Par0 = Par0_list,
                      DM=list(step_cm=list(mean=~1, sd=~1)))
  
  Par0_2 <- getPar0(model= res_HMM_1[[i]], nbStates=3,
                    DM=list(step_cm=list(mean=~1, sd=~1)))
  
  res_HMM_2[[i]] <- fitHMM(data =list_datHMM[[i]] , nbStates = 3, dist = dist2, 
                           Par0 = Par0_2$Par,
                  DM=list(step_cm=list(mean=~1, sd=~1)))
  

}

lapply(res_HMM_2, function(x){x$CIreal$step_cm$est})
lapply(res_HMM_1, function(x){x$CIreal$step_cm$est})


# comparaison 1s et 5s 

## gamma 1s 
Par0_1s_a <- list(step_cm=c(0.01,0.5,2,0.01,0.2, 0.2, 0.1, 0.1, 0.1))

Par0_1s_b <- list(step_cm=c(0.001,mean(list_datHMM[[1]]$step_cm, na.rm = TRUE)
                          ,max(list_datHMM[[1]]$step_cm, na.rm = TRUE),0.0001,0.2, 0.2, 0.1, 0.1, 0.1))
T1<-Sys.time()
m_1s_a <- fitHMM(data = list_datHMM[[1]], nbStates = 3, dist = dist2, Par0 = Par0_1s_a)
m_1s_b <- fitHMM(data = list_datHMM[[1]], nbStates = 3, dist = dist2, Par0 = Par0_1s_b)

m_1s_a
m_1s_b

T2<-Sys.time() 

Tdiff_1s= difftime(T2, T1) 

Par0_2 <- getPar0(model=m_1s, nbStates=3,
                  DM=list(step_cm=list(mean=~1, sd=~1, zeromass=~1)))

m2_1s <- fitHMM(data = list_datHMM[[1]], nbStates = 3, dist = dist2, Par0 = Par0_2$Par,
                DM=list(step_cm=list(mean=~1, sd=~1, zeromass=~1)))

# Gamma 5s 
Par0_5s_a <- list(step_cm=c(0.0001,0.5, 4,
                          0.00001,0.1, 0.2))


Par0_5s_b <- list(step_cm=c(0.0001,mean(list_datHMM[[2]]$step_cm, na.rm=T), max(list_datHMM[[2]]$step_cm)-1,
                          0.00001,0.2, 0.2))


T1<-Sys.time()
m_5s_a <- fitHMM(data = list_datHMM[[2]], nbStates = 3, dist = dist2, Par0 = Par0_5s_a,
             estAngleMean = list(angle=TRUE))

m_5s_b <- fitHMM(data = list_datHMM[[2]], nbStates = 3, dist = dist2, Par0 = Par0_5s_b,
                 estAngleMean = list(angle=TRUE))

AIC(m_5s_a,m_5s_b)

T2<-Sys.time() 
Tdiff_5s= difftime(T2, T1) 

Par0_2_5s<- getPar0(model=m_5s, nbStates=3,
                  DM=list(step_cm=list(mean=~1, sd=~1)))

m2_5s <- fitHMM(data = list_datHMM[[2]], nbStates = 3, dist = dist2, Par0 = Par0_2_5s$Par,
             DM=list(step_cm=list(mean=~1, sd=~1)))

m2_5s

m2_1s

Tdiff_5s
Tdiff_1s
# 10 fois plus rapide pour le 5s = ok on peut partir la dessus mais reste la question du parametrage 

# choix du paramètrage optmimal en se basant sur l'AIC ? 


