# HMM on time step of 1second 

# load package 
library(momentuHMM)
library(dplyr)

# Function HMM

HMM_vid.fun <- function(dfr.HMM){
  
  dist2 = list(step_distance = "gamma")
  
  Par0 <- list(step_distance=c(0.01,0.5,3,0.2,0.2, 0.2, 0.1, 0.1, 0.1))
  #gamma and wpcauchy 
  m1 <- fitHMM(data = dfr.HMM, nbStates = 3, dist = dist2, Par0 = Par0,
               estAngleMean = list(angle=TRUE))
  
  Par0_2 <- getPar0(model=m1, nbStates=3,
                    DM=list(step_distance=list(mean=~1, sd=~1, zeromass=~1)))
  
  m2 <- fitHMM(data = dfr.HMM, nbStates = 3, dist = dist2, Par0 = Par0_2$Par,
               DM=list(step_distance=list(mean=~1, sd=~1, zeromass=~1)))
  return(m2)
}

poss.HMM.vid = possibly(.f = HMM_vid.fun, otherwise = NA)

# load data 

tib_all_file_orange <- readRDS("./output/tib_all_file_orange.rds")
tib_all_file_rouge <- readRDS("./output/tib_all_file_rouge.rds")

head(tib_all_file_orange$CRCW2[[1]])
tst <- tib_all_file_orange$CRCW2[[1]]
# 1 transfo en momentuhmm object le crcw 
# creer un nouvelle variable avec juste ID, Time and Step 
# et la passer direct en prep data pour pouvoir appliquer les HMM dessus directement 
temp.df <- tst %>% dplyr::select(Time, ID, step_distance)
HMM_dat <- prepData(temp.df,  coordNames = NULL)


# Preparation des data pour etre compatible avec le format momentuHMM

fun.preData <- function(dfr){
  temp.df <- dfr %>% dplyr::select(Time, ID, step_distance)
  HMM_dat <- prepData(temp.df,  coordNames = NULL)
  return(HMM_dat[-1,])
  
}

poss.HMM.pdata = possibly(.f = fun.preData, otherwise = NA)

tib_all_file_orange <- tib_all_file_orange %>% 
  mutate(HMM_data=future_map(CRCW2,poss.HMM.pdata))   

tib_all_file_rouge <- tib_all_file_rouge %>% 
  mutate(HMM_data=future_map(CRCW2,poss.HMM.pdata))   


# TEST HMM avant lancement boucle 
head(tib_all_file_rouge$HMM_data[[288]])
head(tib_all_file_orange$HMM_data[[288]])
head(tib_all_file_orange)
head(tib_all_file_rouge)
dim(tib_all_file_rouge)
unique(tib_all_file_rouge$Rpi)

tst2 <- tib_all_file_rouge$HMM_data[[1]]
essHMM <- poss.HMM.vid(tst2)
plot(essHMM)


# RUN HMM
## orange
tib_all_file_orange_1stp <- tib_all_file_orange[1:96,]
tib_all_file_orange_2stp <- tib_all_file_orange[97:193,]
tib_all_file_orange_3stp <- tib_all_file_orange[194:288,]

tib_all_file_orange_1stp <- tib_all_file_orange_1stp %>%
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))

tib_all_file_orange_2stp <- tib_all_file_orange_2stp %>% 
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))  


tib_all_file_orange_3stp <- tib_all_file_orange_3stp %>% 
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))  

tib_all_file_orange_HMM <- rbind(tib_all_file_orange_1stp,tib_all_file_orange_2stp,
                                 tib_all_file_orange_3stp)

saveRDS(tib_all_file_orange_HMM, "./output/tib_all_file_orange_HMM.rds")


# Red 
tib_all_file_rouge_1stp <- tib_all_file_rouge[1:96,]
tib_all_file_rouge_2stp <- tib_all_file_rouge[97:193,]
tib_all_file_rouge_3stp <- tib_all_file_rouge[194:288,]

tib_all_file_rouge_1stp <- tib_all_file_rouge_1stp %>% 
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))  

tib_all_file_rouge_2stp <- tib_all_file_rouge_1stp %>% 
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))  

tib_all_file_rouge_3stp <- tib_all_file_rouge_1stp %>% 
  mutate(HMM_res=future_map(HMM_data,poss.HMM.vid))  


tib_all_file_rouge_HMM <- rbind(tib_all_file_rouge_1stp,tib_all_file_rouge_2stp,
                                 tib_all_file_rouge_3stp)

saveRDS(tib_all_file_rouge_HMM, "./output/tib_all_file_rouge_HMM.rds")
