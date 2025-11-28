# HMM video parameters extraction 

library(momentuHMM)
library(dplyr)
library(MoveR)
library(zoo)
library(here)
library(purrr)
library(tidyverse)
library(furrr)
library(gtsummary)

# load/import data in a list -----
metadata <- read_csv("./Data_larvae/20240807_meta_data.csv")
metadata <- metadata %>% separate(ID, c("Camera", "ID2")) 
metadata <- metadata %>%  mutate(Camera=str_to_lower(Camera), fullID=paste0(Camera,ID2))
metadata$date_adulte <- as.Date(metadata$date_adulte, format="%d-%m")
metadata$date_mort <- as.Date(metadata$date_mort, format="%d-%m")
metadata_sub <-  metadata %>% drop_na(position_LAM) %>% filter(replicat!=5) %>% 
  mutate(Time=as.numeric(difftime(date_mort,date_adulte, units="days")))

# tri des id qui sont en LAM 
# suppresion des ID qui sont pas dans les LAM et virer le replicat 05
metadata_sub <-  metadata %>% drop_na(position_LAM) %>% filter(replicat!=5) %>% 
  mutate(Time=as.numeric(difftime(date_mort,date_adulte, units="days")), ID2 = gsub("[^0-9]", "", fullID))


ID_selected <- read.csv("./output/ID_selected_MoveR.csv")# ID avec moins de 20% de perte de tracking 

# 

corres_ttmt <- read_csv2("./data_larvae/corr_traitement_plaque.csv")%>% 
  select(c(1:4))

## import files of HMM parameters estimated on other scripts 

files_names <- list.files(here("./output/HMM_res/"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".rds")
}

list_file <- vector("list", length(files_names))
for(i in 1: length(list_file)) { 
  # x <- read.csv2(here::here("data",files_list[i]))
  list_file[[i]] <-  readRDS(paste(here("./output/HMM_res/", files_names[i])))
  #assign(data_names[[i]], x)
}
names(list_file) <- data_names

# test on 1 Rplicate 
## function for param estimate extraction 
fun_extract_step <- function(res_HMM){
  res_l <- tryCatch(res_HMM$CIreal$step$est %>% data.frame(), 
                    error = function(e) NA)
  return(res_l)
  
}

fun_extract_angle <- function(res_HMM){
  res_l <- tryCatch(res_HMM$CIreal$angle$est %>% data.frame(),
                    error = function(e) NA)
  return(res_l)
}

fun_TiS_extract <- function(res_HMM){
  res_l <- tryCatch(timeInStates(res_HMM) %>% data.frame(), 
                    error = function(e) NA)
  return(res_l)
}

# Application to all individual 
list_res_step <- list()
list_res_angle <- list()

  for ( i in 1:length(list_file)){
    print(i)
  temp <- list_file[[i]] %>% mutate(step_est=map(HMM_fit, fun_extract_step),
                                  angle_est=map(HMM_fit, fun_extract_angle),
                                  TinState=map(HMM_fit, fun_TiS_extract))
  #step df 
  list_res_step[[i]] <- temp %>% #filter(HMM_fit != "NA") %>% 
    select(rpi,ID,step_est) %>% 
    unnest(step_est) %>% mutate(origin=rep(names(list_file[i])))
  
  list_res_step[[i]] <- list_res_step[[i]] %>% drop_na() %>% 
    mutate(metrics=rep(c("mean", "sd","zeromass"), 
                       times = nrow(na.omit(list_res_step[[i]]))/3))
  
  # angle df 
  list_res_angle[[i]] <- temp %>% #filter(HMM_fit != "NA") %>% 
    select(rpi,ID,angle_est) %>% 
    unnest(angle_est) %>% mutate(origin=rep(names(list_file[i])))
  
  list_res_angle[[i]] <-   list_res_angle[[i]] %>% drop_na() %>% 
    mutate(metrics=rep(c("mean", "concentration"), 
                       times=nrow(na.omit(list_res_angle[[i]]))/2))
}


### extraction des params en DF pour visualisation and analysis----- 

angle_df <- do.call(rbind, list_res_angle) %>%
  mutate(replicat=str_sub(origin, -2,-1))

step_df <- do.call(rbind, list_res_step) %>% 
  mutate(replicat=str_sub(origin, -2,-1))


### pivot to bind both parameters

df_plot_angle <- angle_df %>% 
  pivot_longer(cols=c(3,4,5), names_to = "State") %>% mutate(param="angle")

df_plot_step <- step_df %>% 
  pivot_longer(cols=c(3,4,5), names_to = "State") %>% mutate(param="step")

# bind both for plot (and maybe some analysis)
df_plot_both <- rbind(df_plot_step, df_plot_angle) %>% data.frame

# join with metadata ----- 
df_plot_both <- df_plot_both %>% 
  mutate(ID2=str_sub(ID, -6, -1)) %>% inner_join(metadata_sub, by ="ID2")
  
## Plot param ----- 


  
## For methods of clustering with concentration and mu only ----- 
angle_df2 <- angle_df %>% 
  filter(metrics=="concentration") %>% 
  rename(c_state.1=state.1, c_state.2=state.2, c_state.3=state.3)

step_df2 <- step_df %>% 
  filter(metrics=="mean") %>% 
  rename(mu_state.1=state.1, mu_state.2=state.2, mu_state.3=state.3)

# bind both by columns 

df_tot_HMMvid_cl <-angle_df2 %>% select(!metrics) %>% 
  inner_join(step_df2)

# plot univariate ----- 
df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_jitter(aes(y=value, 
                  x=param, 
                  colour=ID, 
                  group=ID))+
  facet_grid(~State)

df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_jitter(aes(y=value, 
                  x=param, 
                  colour=traitement, 
                  group=traitement))+
  facet_grid(~State)

df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_point(aes(y=value, 
                 x=State, 
                 colour=ID, 
                 group=ID))+
  facet_wrap(~param, scales="free")

df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  dplyr::filter(sexe!="NA") %>% 
  ggplot()+
  geom_boxplot(aes(y=value, 
                   x=traitement, colour=traitement))+
  # geom_jitter(aes(y=value, 
  #                 x=traitement, colour=sexe))+
   facet_grid(cols=vars(State), rows=vars(param), scales="free")+theme_light()

ggsave("./Data_larvae/HMM_larvae_inter.jpeg", width = 30, height = 20, units = "cm")


df_plot_both %>%
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  dplyr::filter(param=="step"& sexe!="NA") %>% 
  ggplot()+
  #geom_jitter(aes(x=traitement, y=value, colour = sexe))+
  geom_boxplot(aes(x=traitement, y=value, colour = sexe),width = 0.15, position = position_dodge(0.9))+
  geom_violin(aes(x=traitement, y=value,
                  fill = sexe,
                  colour=sexe),alpha=0.3, linewidth = 0)+
  facet_grid(cols = vars(State), scales="free")+theme_light()+
  #ylab("Lambda values")+
  theme(legend.position = "left")+
  scale_color_manual(
    values = c("deeppink4", "deepskyblue4"))+
  scale_fill_manual(
    values = c("deeppink4", "deepskyblue4"))

ggsave("./Data_larvae/HMM_larvae.jpeg", width = 30, height = 20, units = "cm")

df_plot_both %>%
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  dplyr::filter(param=="step"& sexe!="NA") %>% 
  ggplot()+
  #geom_jitter(aes(x=traitement, y=value, colour = sexe))+
  geom_boxplot(aes(x=traitement, y=value/38, colour = traitement),width = 0.15, position = position_dodge(0.9))+
  geom_violin(aes(x=traitement, y=value/38,
                  fill = traitement,
                  colour=traitement), alpha=0.3, linewidth = 0)+
  facet_grid(cols = vars(State), scales="free")+theme_light()+
  ylab("Step values (in cm/s)")+
  theme(legend.position = "left")
  # scale_color_manual(
  #   values = c("deeppink4", "deepskyblue4"))+
  # scale_fill_manual(
  #   values = c("deeppink4", "deepskyblue4"))
ggsave("./img/HMM_larvae3.jpeg", width = 30, height = 20, units = "cm")

# ACP ------
library(FactoMineR)
library(corrplot)
library(factoextra)

pca1<-PCA(df_tot_HMMvid_cl[,-c(1,2,6,7,11)], graph=FALSE)

fviz_eig(pca1, addlabels = TRUE) 

res <- get_pca_var(pca1)
corrplot(res$cos2)

fviz_pca_var(pca1, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_ind(pca1,
             #label = "none", # hide individual labels
            # habillage =  df_ACP$Traitement, # color by groups
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


# Test en individuel ---- 

tib_rpi1_R09 <- list_file[[3]]

tib_rpi1_R09 <- tib_rpi1_R09 %>% mutate(step_est=map(HMM_fit, fun_extract_step),
                                  angle_est=map(HMM_fit, fun_extract_angle),
                                  TinState=map(HMM_fit, fun_TiS_extract))

## step 
vid_step_df <- tib_rpi1_R09 %>% #filter(HMM_fit != "NA") %>% 
  select(rpi,ID,step_est) %>% 
  unnest(step_est) %>% mutate(replicat=rep("R09"))

vid_step_df <- vid_step_df %>% drop_na() %>% 
  mutate(metrics=rep(c("mean", "sd","zeromass"), 
                     times =nrow(na.omit(vid_step_df))/3))


## angle 
vid_angle_df <- tib_rpi1_R09%>% #filter(HMM_fit != "NA") %>% 
  select(rpi,ID,angle_est) %>% 
  unnest(angle_est) %>% mutate(replicat=rep("R09"))

vid_angle_df <- vid_angle_df %>% drop_na() %>% 
  mutate(metrics=rep(c("mean", "concentration"), 
  times=nrow(na.omit(vid_angle_df))/2))


### plot 
vid_step_df %>% 
  dplyr::filter(metrics=="mean") %>% 
  ggplot()+
  geom_jitter(aes(y=state.1, 
                  x=metrics, 
                  colour=ID))

# length step by second 

vid_step_df %>% 
  dplyr::filter(metrics=="mean" & state.1<100) %>% 
  ggplot()+
  geom_jitter(aes(y=state.1, 
                  x=metrics, 
                  colour=ID))

vid_step_df %>% 
  dplyr::filter(metrics=="mean" & state.1<100) %>% 
  ggplot()+
  geom_jitter(aes(y=state.1, 
                  x=metrics, 
                  colour=ID))

vid_step_df %>% 
  dplyr::filter(metrics=="mean" & state.3<100) %>% 
  ggplot()+
  geom_jitter(aes(y=state.3, 
                  x=metrics, 
                  colour=ID))

fun_plot_angle <- function(ste){
  ggplot()+
    geom_jitter(aes(y=ste, 
                    x=metrics, 
                    colour=ID))
}

df_plot_angle <- vid_angle_df %>% 
  pivot_longer(cols=c(3,4,5), names_to = "State") %>% mutate(param="angle")

df_plot_angle %>% dplyr::filter(metrics=="concentration") %>% 
  ggplot()+
  geom_jitter(aes(y=value, 
                  x=metrics, 
                  colour=ID))+
  facet_grid(~State)

# in line by ID 
df_plot_angle %>% dplyr::filter(metrics=="concentration"& value<100) %>% 
  ggplot()+
  geom_line(aes(y=value, 
                  x=State, 
                  colour=ID, group=ID))



df_plot_step <- vid_step_df %>% 
  pivot_longer(cols=c(3,4,5), names_to = "State") %>% mutate(param="step")

# plot 
df_plot_step %>% dplyr::filter(metrics=="mean"& value<100) %>% 
  ggplot()+
  geom_jitter(aes(y=value, 
                  x=metrics, 
                  colour=ID))+
  facet_grid(~State)

# link both df to see the association between angle and step_length (speed)

df_plot_both <- rbind(df_plot_step, df_plot_angle) %>% data.frame

### tbl for data summary 
tbl <- 
  mtcars %>%
  select(am, cyl, mpg, hp) %>%
  dplyr::mutate(
    cyl = paste(cyl, "Cylinder"),
    am = factor(am, labels = c("Automatic", "Manual"))
  ) %>%
  tbl_strata(
    strata = cyl,
    ~.x %>%
      tbl_summary(
        by = am,
        type = where(is.numeric) ~ "continuous"
      ) %>%
      modify_header(all_stat_cols() ~ "**{level}**")
  )


tbl_sum <- df_plot_both %>%
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>% 
  dplyr::select(State, param, traitement, value) %>% dplyr::mutate_if(is.character, as.factor) %>% 
  group_by(State, param,traitement) %>% summarise(MeanP=mean(value))
tbl_sum

## step mean 
df_plot_both %>%  
  dplyr::filter( value<100 & metrics=="mean"& param=='step' ) %>% 
  tbl_summary(include = value,
    by=State,
    type = ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    digits = list(all_continuous2() ~ 2))

## conentration value 
df_plot_both %>%  
  dplyr::filter( value<100 & metrics=="concentration"& param=='angle' ) %>% 
  tbl_summary(include = value,
              by=State,
              type = ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})"),
              digits = list(all_continuous2() ~ 2))

df_plot_both %>% 
  tbl_summary(
    include = loop.list,
    by=sexe,
    type = loop.list ~ "continuous2",
    statistic = all_continuous2() ~ c("{median} ({p25} - {p75}", "{mean} ({sd})"),
    digits = list(
      all_continuous2() ~ 2)
  )


df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>% 
  ggplot()+
  geom_jitter(aes(y=value, 
                  x=param, 
                  colour=ID))+
  facet_grid(~State)

df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_boxplot(aes(y=value, 
                  x=param))+
  facet_grid(~State)
             
df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_jitter(aes(y=value, 
                x=param, 
                colour=ID, 
                group=ID))+
  facet_grid(~State)

df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_point(aes(y=value, 
                  x=State, 
                  colour=ID, 
                  group=ID))+
  facet_wrap(~param, scales="free")


df_plot_both %>% 
  dplyr::filter( value<100 & metrics=="mean"| metrics=="concentration") %>%
  ggplot()+
  geom_boxplot(aes(y=value, 
                 x=State, colour=State))+
  facet_wrap(~param, scales="free")

# Description des trajectoires : ACP pour les valeurs des états pour step and angle. 
angle_concentration <- df_plot_angle %>% 
  filter(metrics=="concentration") %>% 
  select(value)

step_mean <- df_plot_step %>%
  filter(metrics=="mean") %>%
  select(value) 

plot(angle_concentration$value,step_mean$value, ylim = c(0,10))

cor.test(angle_concentration$value,step_mean$value)

t_HMM <- readRDS("./output/HMM_res/VID_HMM_rpi106.rds")
plot(t_HMM$dataHMM[[1]]$step, type="l")

plot(t_HMM$dataHMM[[1]]$step/38, type="l", ylab="Step length by second (cm/s)", xlab="Time (seconds)")
plotStates(t_HMM$HMM_fit[[1]], ask=F)

