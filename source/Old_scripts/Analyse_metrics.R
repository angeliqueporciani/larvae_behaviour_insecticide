# Visualisation and analyses of metrics 

# load package----

library(tidyverse)
library(patchwork)

# load data ---- 

res_by_ind_spatial <- read.delim(
  "./Results_data_ATA/Results/Spatial/Element_All_borders_1.csv",
                                 ,
                                 sep = ";") %>% 
  separate(Video, c("Camera", "FPS", "Replicat"), remove=FALSE)

res_by_ind <- read.delim("./Results_data_ATA/Results/Results_by_ind.csv",
                                 ,
                                 sep = ";") %>% 
  separate(Video, c("Camera", "FPS", "Replicat"), remove=FALSE) %>% 
  select(!c(25:29))


corres_ID <- read_csv2("./Corr_coord_data/corres_Arena_ID.csv") %>% 
  select(c(1,2))

corres_ttmt <- read_csv2("./corr_traitement_plaque.csv")%>% 
  select(c(1:4))

## ID attribution 
corres_ID <- corres_ID %>% 
  mutate(Arena=as.numeric(str_extract(Arena, "\\d.*")))

res_by_ind <- res_by_ind %>%
  inner_join(corres_ID) 

res_by_ind <- res_by_ind %>% 
  mutate(Plaque=as.numeric(str_extract(ID, "\\d+"))) %>% 
  inner_join(corres_ttmt) %>% mutate(End_seq_min=as.numeric(End_seq)/60)
                                    

str(res_by_ind)

res_by_ind_spatial <- res_by_ind_spatial %>%
  inner_join(corres_ID) 

res_by_ind_spatial <- res_by_ind_spatial %>% 
  mutate(Plaque=as.numeric(str_extract(ID, "\\d+"))) %>% 
  inner_join(corres_ttmt)

# Visualisation ---- 
## general sequence (complete recording almost 1hour)----
res_by_ind_sub <- res_by_ind %>% filter(Sequence=="General" & Prop_time_lost < 0.20)


## Prop time moving 
ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Prop_time_moving, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Prop_time_moving, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Prop_time_moving, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Proportion of time moving (%)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

## Average Speed 
ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Average_Speed, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Average_Speed, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Average_Speed, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Average speed (cm/s)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

## Average Speed Moving
ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Average_Speed_Moving, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Average_Speed_Moving, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Average_Speed_Moving, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Average Speed Moving (cm/s)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

## Traveled_Dist
ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Traveled_Dist, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Traveled_Dist, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Traveled_Dist, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Traveled_Dist in one hour (cm2)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))


## Traveled_Dist_moving
ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Traveled_Dist_Moving, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Traveled_Dist_Moving, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Traveled_Dist_Moving, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Traveled_Dist when moving in one hour (cm2)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

## Meander

ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Meander, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Meander, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Meander, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Meander")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

## Meander moving

ggplot(data=res_by_ind_sub)+
  geom_jitter(aes(x=Traitement, y=Meander_moving, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Meander_moving, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Meander_moving, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Meander_moving")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))


# Loop

data_temp=subset(res_by_ind, Sequence=="General")
loop.list <- names(data_temp[, c(14:20)])
plot_list <- list()

for (n in loop.list)
  {
  plot_list[[n]] <- ggplot(data=data_temp)+
    geom_jitter(aes_string(x="Traitement", y=n, color="Traitement")) +
    geom_boxplot(aes_string(x="Traitement", y=n, color="Traitement"), width = 0.15, position = position_dodge(0.9)) +
     geom_violin(aes_string(x="Traitement", y=n,
                    fill = "Traitement",
                    colour="Traitement"),alpha=0.3, linewidth = 0) +
    theme_light()+
    theme(legend.position = "none")+
    ylab(paste(n))+
    scale_color_manual(
      values = c("#648FFF", "#FE6100"))+
    scale_fill_manual(
      values = c("#648FFF", "#FE6100"))
  #print(plot_list[[n]])
}

wrap_plots(plot_list, nrow=2)

## sequence 90% of explored surface ----
res_by_ind_sub2 <- res_by_ind %>% 
  filter(Prop_time_lost < 0.20 & Sequence=="Seq_0")

res_by_ind_spatial_sub <- res_by_ind_spatial %>% 
  filter(Prop_time_lost < 0.20 & Sequence=="Seq_0")
str(res_by_ind_spatial_sub)

## Loop plot 
res_by_ind_spatial <- res_by_ind_spatial %>% mutate(End_seq=res_by_ind$End_seq)
data_temp2 <- res_by_ind_spatial %>% filter(Prop_time_lost < 0.20 &
                                Sequence == "Seq_0") 
loop.list <- names(data_temp2[, c(9:12,14:17,18,20,21,29)])
plot_list <- list()

for (n in loop.list)
{
  plot_list[[n]] <- res_by_ind_spatial %>% filter(Prop_time_lost < 0.20 &
                                                    Sequence == "Seq_0") %>%
    ggplot()+
    geom_jitter(aes_string(x="Traitement", y=n, color="Traitement")) +
    geom_boxplot(aes_string(x="Traitement", y=n, color="Traitement"), width = 0.15, position = position_dodge(0.9)) +
    geom_violin(aes_string(x="Traitement", y=n,
                           fill = "Traitement",
                           colour="Traitement"),alpha=0.3, linewidth = 0) +
    theme_light()+
    theme(legend.position = "none")+
    ylab(paste(n))+
    scale_color_manual(
      values = c("#648FFF", "#FE6100"))+
    scale_fill_manual(
      values = c("#648FFF", "#FE6100"))
  #print(plot_list[[n]])
}

wrap_plots(plot_list, nrow=2)

# Time length for exploring 90% of the surface (ET90)
ggplot(data=res_by_ind_sub2)+
  geom_jitter(aes(x=Traitement, y=End_seq, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=End_seq, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=End_seq, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Time to explored 90% of arena (sec)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

# Time length for exploring 90% of the surface (ET90) without 2 outliers 
ggplot(data=subset(res_by_ind_sub2, End_seq<3000))+
  geom_jitter(aes(x=Traitement, y=End_seq, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=End_seq, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=End_seq, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Time to explored 90% of arena (sec)")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))

# Proportion of time along border (3mm) for 1 one hour of recording
res_by_ind_spatial %>% filter(Prop_time_lost < 0.20 & Sequence=="General") %>% 
ggplot()+
  geom_jitter(aes(x=Traitement, y=Prop_time_inside, color=Traitement)) +
  geom_boxplot(aes(x=Traitement, y=Prop_time_inside, color=Traitement), width = 0.15, position = position_dodge(0.9)) +
  geom_violin(aes(x=Traitement, y=Prop_time_inside, 
                  fill = Traitement, 
                  colour=Traitement),alpha=0.3) +
  theme_light()+
  ylab("Proportion of time close to border (1mm around) during 1 hour")+
  scale_color_manual(
    values = c("#648FFF", "#FE6100"))+
  scale_fill_manual(
    values = c("#648FFF", "#FE6100"))


# correlation between time to reach 90% of explored area and traveled distance 

res_by_ind_spatial %>% filter(Prop_time_lost < 0.20 &
                                Sequence == "Seq_0") %>%
  ggplot() +
  geom_point(aes(x = Traveled_Dist_Moving, y = End_seq, colour = Traitement)) +
  # geom_smooth(
  #   aes(x = Traveled_Dist_Moving, y = End_seq, colour = Traitement),
  #   method = loess,
  #   se = FALSE,
  #   linewidth = 0.5,
  #   linetype = "dashed"
  # ) +
  geom_smooth(
    aes(x = Traveled_Dist_Moving, y = End_seq, colour = Traitement, fill=Traitement),
    method = lm,
    se = TRUE,
    linewidth = 0.5
  ) +
  theme_light() + scale_color_manual(values = c("#648FFF", "#FE6100")) +
  scale_fill_manual(values = c("#648FFF", "#FE6100"))
                                     
## test GLMM 
library(glmmTMB)
data_glm <- res_by_ind%>% filter(Prop_time_lost < 0.20 &
                                            Sequence == "General") %>% 
  mutate_if(is.character, as.factor)
str(data_glm)

PTM <- glmmTMB(Prop_time_moving~Traitement, data=data_glm, family=binomial(link = "logit"))
summary(PTM)

hist(data_glm$Traveled_Dist)
TD <- glmmTMB(Traveled_Dist~Traitement, data=data_glm, family=gaussian(link = "identity"))
summary(TD)

hist(data_glm$Traveled_Dist_Moving)
TDM <- glmmTMB(Traveled_Dist_Moving~Traitement, data=data_glm, family=gaussian(link = "identity"))
summary(TDM)


hist(data_glm$Traveled_Dist_Moving)
AS <- glmmTMB(Average_Speed_Moving~Traitement, data=data_glm, family=gaussian(link = "identity"))
summary(AS)
tidy(AS)

# boucle glm gaussien 
res_glm <- list()
for (i in loop.list[-1]){
  mod <- glmmTMB(i~Traitement, data=data_glm, family=gaussian(link = "identity"))
  res_glm[[i]] <- summary(mod)
}


library(tidystringdist) # Works since v0.1.2
library(broom.mixed)

comb <- tidy_comb_all(names(data_glm))

mod <- glmmTMB(Traveled_Dist_Moving~Traitement, data=data_glm, family=gaussian(link = "identity"))
summary(mod)

RLS_table <- data_glm %>% 
  dplyr::select(loop.list[-1]) %>%  # enleve la var réponse pour ne conserver que les var explicatives
  map(~glmmTMB(.x ~ Traitement, data = data_glm))%>% 
  map_dfr(~broom::tidy(., effects = "fixed"), .id = 'source')%>% 
  dplyr::select(-term) %>% 
  arrange(p.value)

RLS_table

class(RLS_table[[1]])
RLS_table[[1]]$coefficients

RLS_table
## Test SVM ---
library(kernlab)

### prep data
data <- res_by_ind %>% filter(Sequence=="General") %>% select(c(9:10,14:21,27)) %>% mutate_if(is.character, as.factor)
str(data)

####3
resksvm=ksvm(Traitement~., data=data, type="nu-svc", cross=2)
names(resksvm)

###4
##noyau gaussien par defaut et un seul paramètre= sigma
class(resksvm)####objet de classe S2 sous R on accède a ce qu'il ya  dedans via slotNames et faut utiliser @
slotNames(resksvm)
resksvm@kernelf####donne les paramètres du noyaux 
resksvm@param###donne nu, par défaut 0.2
resksvm###donne tous les paramètres d'un coup
resksvm@obj ###approche 1 contre 1 par défaut

###Q7 taux d'erreur dans l'echantillon d'apprentissage 
resksvm@error###taux d'erreur d'apprentissage ==0 ici

###Q8 erreur en validation croisée
erreur4=NULL
for (i in 1:100){
  resksvm=ksvm(Traitement~., data=data, type="nu-svc", cross=2)
  erreur4=c(erreur4, resksvm@cross)
}
mean(erreur4)###erreur 0.46


seqnu=seq(0.1,0.5,by=0.1)
seqsig=10^((-2):2)

enrres=matrix(NA,length(seqnu)*length(seqsig),3)
colnames(enrres)=c("nu","sigma","errmoy")
compt=1
for (i in seqnu)
{
  print(i)
  for (j in seqsig)
  {
    print(j)
    enrloc=NULL
    for (k in 1:10)
    {
      ressvmloc=ksvm(Traitement~., data=data, type="nu-svc", cross=2,nu=i,kpar = list(sigma = j))
      enrloc=c(enrloc,ressvmloc@cross)
    }
    enrres[compt,]=c(i,j,mean(enrloc))
    compt=compt+1
  }
}

# which set of values allow the minimum mean error of classification 

min(enrres[,3])

             