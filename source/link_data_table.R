# link between both database : those with video parameter and other with LAM 

LAM_cured <- readRDS("./LAM_data/Output/LAM_Pestim_cured.rds") %>% 
  select(!c(5,6)) %>% 
  unnest(cols = c(MaxAct, Period))

Video_data <- readRDS("./Data_larvae/data_Larve.rds") %>% data.frame()

all_param <- inner_join(Video_data, LAM_cured, by=c("replicat", "camera","plaque","sexe"))

plot(all_param_general$Average_Speed_Moving,all_param_general$MaxAct)
#plot(all_param$Max_Speed,all_param_general$MaxAct)

cor(all_param_general$Average_Speed_Moving,all_param_general$MaxAct)

all_param_general <- all_param %>% filter(Sequence=="General")
cor(all_param_general$Max_Speed,all_param_general$MaxAct)

all_param_general %>% filter(Max_Speed>20) 

scaled_Max_Speed <- scale(all_param_general$Max_Speed)
scaled_MaxAct <- scale(all_param_general$MaxAct)

plot(scaled_Max_Speed,scaled_MaxAct)
