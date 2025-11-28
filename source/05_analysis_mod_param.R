# Analysis of treatment effect on parameters 
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(car)
library(lme4)
library(performance)

## Analysis split for red and orange 

data_complete <- readRDS("./output/data_complete.rds")
data_complete_SP <- readRDS("./output/data_complete_SP.rds")

# Modèle Orange concentration and inf 20 time lost
data_orange_inf20 <- data_complete %>% dplyr::filter(Experiment=="Orange"&Prop_time_lost<0.2)
data_orange_SP <- data_complete_SP %>% dplyr::filter(Experiment=="Orange" & Prop_time_lost<0.2)

### Prop time moving 
ggplot(data_orange_inf20)+
  geom_boxplot(aes(x=Traitement, y=Prop_time_moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Prop_time_moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modPTM <- glmmTMB(sqrt(Prop_time_moving)~Traitement+sexe+(1|Replicat), 
                             data=data_orange_inf20)
res <- simulateResiduals(modPTM)
plot(res)

summary(modPTM)
emmeans(modPTM, pairwise~Traitement+sexe, infer=TRUE, type="response")


### Average speed moving 

ggplot(data_orange_inf20)+
  geom_boxplot(aes(x=Traitement, y=Average_Speed_Moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Average_Speed_Moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modAVM <- glmmTMB(Average_Speed_Moving~Traitement*sexe+(1|Replicat), 
                  data=data_orange_inf20)

Anova(modAVM)

modAVM <- glmmTMB(Average_Speed_Moving~Traitement+sexe+(1|Replicat), 
                  data=data_orange_inf20)

res <- simulateResiduals(modAVM)
plot(res)

summary(modAVM)
emmeans(modAVM, pairwise~Traitement+sexe, infer=TRUE, type="response")



### Traveled distance Moving

ggplot(data_orange_inf20)+
  geom_boxplot(aes(x=Traitement, y=Traveled_Dist_Moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Traveled_Dist_Moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modTVM <- glmmTMB(Traveled_Dist_Moving~Traitement*sexe+(1|Replicat), 
                  data=data_orange_inf20)

Anova(modTVM)

modTVM <- glmmTMB(Traveled_Dist_Moving~Traitement+sexe+(1|Replicat), 
                  data=data_orange_inf20)

res <- simulateResiduals(modTVM)
plot(res)


summary(modTVM)
emmeans(modTVM, pairwise~Traitement+sexe, infer=TRUE, type="response")


### Max speed
ggplot(data_orange_inf20)+
  geom_boxplot(aes(x=Traitement, y=Max_speed, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Max_speed, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modMS <- glmmTMB(Max_speed~Traitement*sexe+(1|Replicat), 
                  data=data_orange_inf20)
Anova(modMS)
modMS <- glmmTMB(Max_speed~Traitement+sexe+(1|Replicat), 
                 data=data_orange_inf20)
summary(modMS)



res <- simulateResiduals(modMS)
plot(res)


emmeans(modMS, pairwise~Traitement+sexe, infer=TRUE, type="response")

### Max bout inactiv 

ggplot(data_orange_inf20_sub)+
  geom_boxplot(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

hist(data_orange_inf20_sub$Max_bout_inactiv)
data_orange_inf20_sub <- data_orange_inf20 %>% dplyr::filter(Max_bout_inactiv<1500)


modMI <- glmmTMB(Max_bout_inactiv~Traitement*sexe+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2())

Anova(modMI)
modMI <- glmmTMB(Max_bout_inactiv~Traitement+sexe+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2(), zi=~1)

summary(modMI)



res <- simulateResiduals(modMI)
plot(res)#  

emmeans(modMI, pairwise~Traitement+sexe, infer=TRUE, type="response")

### Max bout activ 

ggplot(data_orange_inf20_sub)+
  geom_boxplot(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

hist(data_orange_inf20_sub$Max_bout_inactiv)
data_orange_inf20_sub <- data_orange_inf20 %>% dplyr::filter(Max_bout_inactiv<1500)


modMI <- glmmTMB(Max_bout_inactiv~Traitement*sexe+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2())

Anova(modMI)
modMI <- glmmTMB(Max_bout_inactiv~Traitement+sexe+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2(), zi=~1)

summary(modMI)



res <- simulateResiduals(modMI)
plot(res)#  

emmeans(modMI, pairwise~Traitement+sexe, infer=TRUE, type="response")



## Same for red concentration 

data_rouge_inf20 <- data_complete %>% dplyr::filter(Experiment=="Rouge"&Prop_time_lost<0.2)

### Prop time moving 
ggplot(data_rouge_inf20)+
  geom_boxplot(aes(x=Traitement, y=Prop_time_moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Prop_time_moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modPTM <- glmmTMB(Prop_time_moving~Traitement*sexe+(1|Replicat), 
                  data=data_rouge_inf20, family = binomial)

Anova(modPTM)


modPTM <- glmmTMB(Prop_time_moving~Traitement+(1|Replicat), 
                  data=data_rouge_inf20, family = binomial)
summary(modPTM)

emmeans(modPTM, pairwise~Traitement, infer=TRUE, type="response")


res <- simulateResiduals(modPTM)
plot(res)
model_performance(modPTM)

modPTM <- glm(Prop_time_moving~Traitement, 
                  data=data_rouge_inf20, family = quasibinomial)


summary(modPTM)

emmeans(modPTM, pairwise~Traitement, infer=TRUE, type="response")


res <- simulateResiduals(modPTM)
plot(res)
model_performance(modPTM)
plot(residuals(modPTM))

#AVG SM
ggplot(data_rouge_inf20)+
  geom_boxplot(aes(x=Traitement, y=Average_Speed_Moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Average_Speed_Moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modAVM <- glmmTMB(Average_Speed_Moving~Traitement*sexe+(1|Replicat), 
                  data=data_rouge_inf20)

Anova(modAVM)
summary(modAVM)

res <- simulateResiduals(modAVM)
plot(res)

emmeans(modAVM, pairwise~Traitement+sexe, infer=TRUE, type="response")
# 
### Traveled distance  

ggplot(data_rouge_inf20)+
  geom_boxplot(aes(x=Traitement, y=Traveled_Dist_Moving, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Traveled_Dist_Moving, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modTVM <- glmmTMB(Traveled_Dist_Moving~Traitement*sexe+(1|Replicat), 
                  data=data_rouge_inf20)
Anova(modTVM)
modTVM <- glmmTMB(Traveled_Dist_Moving~Traitement+(1|Replicat), 
                  data=data_rouge_inf20)
summary(modTVM)
res <- simulateResiduals(modTVM)
plot(res)

# pas ok 

check_outliers(modTVM)
# transformation 
modTVM <- glmmTMB(sqrt(Traveled_Dist_Moving)~Traitement+sexe+(1|Replicat), 
                  data=data_rouge_inf20)
res <- simulateResiduals(modTVM)
plot(res)
Anova(modTVM)# sors pas mais modèle meilleur avec variable sexe 

emmeans(modTVM, pairwise~Traitement+sexe, infer=TRUE, type="response")



### Max speed
ggplot(data_rouge_inf20)+
  geom_boxplot(aes(x=Traitement, y=Max_speed, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Max_speed, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


modMS <- glmmTMB(Max_speed~Traitement*sexe+(1|Replicat), 
                 data=data_rouge_inf20)
Anova(modMS)

modMS <- glmmTMB(Max_speed~Traitement+sexe+(1|Replicat), 
                 data=data_orange_inf20)
summary(modMS)

res <- simulateResiduals(modMS)
plot(res)
emmeans(modMS, pairwise~Traitement+sexe, infer=TRUE, type="response")

### Max bout inactiv 

ggplot(data_orange_inf20_sub)+
  geom_boxplot(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  geom_jitter(aes(x=Traitement, y=Max_bout_inactiv, colour=Traitement))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

hist(data_orange_inf20_sub$Max_bout_inactiv)
# retrait des +1500 
data_orange_inf20_sub <- data_orange_inf20 %>% dplyr::filter(Max_bout_inactiv<1500)


modMI <- glmmTMB(Max_bout_inactiv~Traitement*sexe+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2())

Anova(modMI)
modMI <- glmmTMB(Max_bout_inactiv~Traitement+(1|Replicat), 
                 data=data_orange_inf20_sub, family=nbinom2(), zi=~1)

summary(modMI)

emmeans(modMI, pairwise~Traitement+sexe, infer=TRUE, type="response")


res <- simulateResiduals(modMI)
plot(res)#OK 

## Comparaison for Spatial data 




