# 10 : Recuperation des paramètres des HMM estimés dans le script 09 

# Load package 
library(momentuHMM)
library(dplyr)
library(ggplot2)
library(furrr)
library(future)

# Load Data 
data_complete <- readRDS("./output/data_complete.rds")
corres_ttmt_ID <- data_complete %>% select(fullID2, Traitement, sexe)

tib_all_file_rouge_HMM <- readRDS("./output/tib_all_file_rouge_HMM.rds")
tib_all_file_orange_HMM <- readRDS("./output/tib_all_file_orange_HMM.rds")


# Function
fun_extract_step <- function(res.HMM){
  res_l <- tryCatch(res.HMM$CIreal$step$est %>% data.frame(), 
                    error = function(e) NA)
  return(res_l)
  
}

fun_TiS_extract <- function(res.HMM){
  res_l <- tryCatch(timeInStates(res.HMM) %>% data.frame(), 
                    error = function(e) NA)
  return(res_l)
}



# Apply to all 
tib_all_file_orange_HMM <- tib_all_file_orange_HMM %>% 
  mutate(step_est=future_map(HMM_res, fun_extract_step),
         TiS_est=future_map(HMM_res, fun_TiS_extract))

tib_all_file_rouge_HMM <- tib_all_file_rouge_HMM %>% 
  mutate(step_est=future_map(HMM_res, fun_extract_step),
         TiS_est=future_map(HMM_res, fun_TiS_extract))

# NA check 

tib_all_file_orange_HMM %>% filter(HMM_res== "NA") %>% summarise(n=n())# 3
tib_all_file_rouge_HMM %>% filter(HMM_res== "NA") %>% summarise(n=n())# 6

# Recuperation des variable d'interet uniquement pour step et Time in State 

step_df_orange <- tib_all_file_orange_HMM %>% filter(!is.na(HMM_res)) %>% 
  inner_join(corres_ttmt_ID) %>% 
  select(Arena, Lot2, Rpi, Replicat,ArenaN, Experiment, fullID2, step_est, Traitement, sexe) %>% 
  unnest(step_est) %>% 
  mutate(metrics=rep(c("mean", "sd","zeromass"), length.out=714))

step_df_rouge <- tib_all_file_rouge_HMM %>% filter(!is.na(HMM_res)) %>% 
  inner_join(corres_ttmt_ID) %>% 
  select(Arena, Lot2, Rpi, Replicat,ArenaN, Experiment, fullID2, step_est, Traitement, sexe) %>% 
  unnest(step_est) %>% mutate(metrics=rep(c("mean", "sd","zeromass"), length.out=846))

# transfo matrix for sort step estimate values 
mat.step_or <- step_df_orange %>% dplyr::filter(metrics=="mean") %>% 
  select(c(8:10))%>% as.matrix()

mat.step_rou <- step_df_rouge %>% dplyr::filter(metrics=="mean") %>% 
  select(c(8:10))%>% as.matrix()

# sorting values 
for (i in 1:nrow(mat.step_or)){
  mat.step_or[i,] <- sort(mat.step_or[i,])
}

for (i in 1:nrow(mat.step_rou)){
  mat.step_rou[i,] <- sort(mat.step_rou[i,])
}


step_df_orange_orderedSt <- step_df_orange %>% 
  dplyr::filter(metrics=="mean") %>% 
  select(!c(8:10)) %>% 
  cbind(mat.step_or) %>% 
  #mutate_if(is.character,as.factor) %>% 
  pivot_longer(cols =c(11:13), names_to ="State" )


step_df_rouge_orderedSt <- step_df_rouge %>% 
  dplyr::filter(metrics=="mean") %>% 
  select(!c(8:10)) %>% 
  cbind(mat.step_rou) %>% 
  #mutate_if(is.character,as.factor) %>% 
  pivot_longer(cols =c(11:13), names_to ="State" )


# plot for visual check Orange
step_df_orange_orderedSt %>% filter(value<3) %>% 
  ggplot()+
  geom_boxplot(aes(y=value, 
                   x=Traitement, 
                   colour=Traitement, 
                   group=Traitement))+
  theme(legend.position="none")+
  facet_grid(~State)

step_df_orange_orderedSt %>% filter(value<3) %>% 
  ggplot()+
  geom_line(aes(y=value, 
                x=State, 
                colour=fullID2, 
                group=fullID2))+
  theme(legend.position="none")


# plot for visual check Rouge
step_df_rouge_orderedSt %>% filter(value<3) %>% 
  ggplot()+
  geom_boxplot(aes(y=value, 
                   x=Traitement, 
                   colour=Traitement, 
                   group=Traitement))+
  theme(legend.position="none")+
  facet_grid(~State)

step_df_rouge_orderedSt %>% filter(value<3) %>% 
  ggplot()+
  geom_line(aes(y=value, 
                x=State, 
                colour=fullID2, 
                group=fullID2))+
  theme(legend.position="none")



#saveRDS(LAM_lambda_df,"./LAM_data/Output/LAM_lambda_df.rds")

# plot for visual check
step_df_orange_orderedSt %>% filter(value<3) %>% 
ggplot()+
  geom_boxplot(aes(y=value, 
                  x=Traitement, 
                  colour=Traitement, 
                  group=Traitement))+
  theme(legend.position="none")+
  facet_grid(~State)

step_df_orange_orderedSt %>% filter(value<3) %>% 
  ggplot()+
  geom_line(aes(y=value, 
                  x=State, 
                  colour=fullID2, 
                  group=fullID2))+
  theme(legend.position="none")
  #facet_grid(~Pos.State)




# Analyse Orange  
filt_or_step <- step_df_orange_orderedSt %>% filter(value<3)
hist(sqrt(filt_or_step$value))


glm_st1 <- glmmTMB(value~Traitement, data=subset(filt_or_step, State=="state.1"), 
                   family=gaussian)

glm_st1.b <- glmmTMB(sqrt(value)~Traitement+(1|Replicat), data=subset(filt_or_step, State=="state.1"), 
                   family=Gamma)

AIC(glm_st1, glm_st1.b)

res <- simulateResiduals(glm_st1.b)
plot(res)

performance(glm_st1.b)

glm_st2.b <- glmmTMB(sqrt(value)~Traitement+(1|Replicat), data=subset(filt_or_step, State=="state.2"), 
                     family=Gamma)

res <- simulateResiduals(glm_st2.b)
plot(res)
summary(glm_st2.b)

glm_st3.b <- glmmTMB(sqrt(value)~Traitement(1|Replicat), data=subset(filt_or_step, State=="state.3"), 
                     family=Gamma)

res <- simulateResiduals(glm_st3.b)
plot(res)

# Same for red 
filt_rou_step <- step_df_rouge_orderedSt %>% filter(value<3)

hist(filt_rou_step$value)
hist(sqrt(filt_rou_step$value))
hist(log(filt_rou_step$value))# bof 

plot(filt_rou_step$value)
subST1 <- subset(filt_rou_step, State=="state.3")
hist(subST1$value)


glm_st1.rou <- glmmTMB(value~Traitement, data=subset(filt_rou_step, State=="state.1"), 
                   family=gaussian)

glm_st2.rou <- glmmTMB(sqrt(value)~Traitement, data=subset(filt_rou_step, State=="state.2"), 
                       family=gaussian)
check_distribution(glm_st2.rou)

glm_st1.rou.2 <- glmmTMB(value~Traitement+sexe+(1|Replicat), data=subset(filt_rou_step, State=="state.1"), 
                       family=gaussian)

glm_st1.rou.3 <- glmmTMB(sqrt(value)~Traitement+sexe+(1|Replicat), data=subset(filt_rou_step, State=="state.1"), 
                         family=gaussian)

glm_st1.rou.b <- glmmTMB(sqrt(value)~Traitement+(1|Replicat), data=subset(filt_rou_step, State=="state.1"), 
                     family=Gamma)

glm_st1.rou.c <- glmmTMB(value~Traitement+sexe+(1|Replicat), data=subset(filt_rou_step, State=="state.1"), 
                         family=ziGamma)
check_model(glm_st1.rou.c)
res <- simulateResiduals(glm_st1.rou.c)
plot(res)


glm_st1.rou.b.2 <- glmmTMB(sqrt(value)~Traitement+sexe+(1|Replicat), data=subset(filt_rou_step, State=="state.1"), 
                         family=Gamma)

AIC(glm_st1.rou, glm_st1.rou.2, glm_st1.rou.3)

res <- simulateResiduals(glm_st1.rou)
plot(res)


res <- simulateResiduals(glm_st1.b.rou)
plot(res)

performance(glm_st1.rou.b)
check_model(glm_st1.rou.b)

check_distribution(glm_st1.rou.b)
check_distribution(glm_st1.rou.2)

glm_st2.b <- glmmTMB(sqrt(value)~Traitement+(1|Replicat), data=subset(filt_or_step, State=="state.2"), 
                     family=Gamma)

res <- simulateResiduals(glm_st2.b)
plot(res)
summary(glm_st2.b)

glm_st3.b <- glmmTMB(sqrt(value)~Traitement(1|Replicat), data=subset(filt_or_step, State=="state.3"), 
                     family=Gamma)

res <- simulateResiduals(glm_st3.b)
plot(res)
## TIS pb a regler pour retrouver les etats 
# link to state order in function of the values of lambda
# loop for create a function that attributes a position on the ordering list of state values 
# old school coding maybe need to be improved later on.
mat.TiS <- LAM_TiS_df %>% select(c(4:7))%>% as.matrix()

m_order_step <- matrix(data=NA, nrow=nrow(mat.step_or), ncol=3, 
                       dimnames=list(NULL, c("Pos.st1.l", "Pos.st2.l","Pos.st3.l")))
m_order_TiS <- matrix(data=NA, nrow=nrow(mat.TiS), ncol=4, 
                      dimnames=list(NULL, c("Pos.st1.tis", "Pos.st2.tis","Pos.st3.tis", "Pos.st4.tis")))

# loop (maybe an apply could do the job in one row)
for (i in 1:nrow(mat.step_or)){
  m_order_step[i,] <- order(m_order_step[i,])
}

# loop (maybe an apply could do the job in one row)
for (i in 1:nrow(mat.TiS)){
  m_order_TiS[i,] <- order(m_order_TiS[i,])
}

# transform as dataframe 
m_order_step_df <- as.data.frame(m_order_step)

m_order_TiS_df <- as.data.frame(m_order_TiS)

# bind to LAM data 
step_df_orange_mean <- step_df_orange %>% filter(metrics=="mean")

LAM_lambda_df <- cbind(step_df_orange_mean,m_order_step_df)

LAM_ordered2 <- LAM_lambda_df %>% pivot_longer(cols =c(8:10), names_to ="State" ) %>% 
  inner_join(corres_ttmt_ID) %>% 
  mutate(Pos.State=case_when(State=="state.1"~Pos.st1.l,
                             State=="state.2"~Pos.st2.l,
                             State=="state.3"~Pos.st3.l))
head(LAM_ordered2)

LAM_TiS_df <- cbind(LAM_TiS_df, m_order_TiS_df)

