# test du package trajR 
# load packages 

library(tidyverse)
library(trajr)
library(stringr)

# Extraction des paramètres 

datazoom <- read.csv2("./Test_trajectoire_larves_HC/corrected_coordinates/00001_Corrected.csv") %>%
  mutate_if(is.character, as.numeric)
dataHQ_ID1 <- read.csv2("./Test_trajectoire_larves_HC/Arena_0Ind0.csv") %>%
  mutate_if(is.character, as.numeric)
dataHQ_ID2 <- read.csv2("./Test_trajectoire_larves_HC/Arena_1Ind0.csv") %>%
  mutate_if(is.character, as.numeric)
summary(dataHQ_ID1)

df.0001 <- datazoom %>% pivot_longer(names_to= "ID",cols = c(3:14)) %>% 
  mutate_if(is.character, as.factor)
df.0001 <- df.0001 %>% separate(ID, c("coord", "Arena", "ID"))
summary(df.0001)

df.0001 <- df.0001 %>% pivot_wider(names_from = coord)

ID1 <- df.0001 %>% filter(Arena=="Arena1")

IDsub <- ID1 %>% filter(Frame < 137)

trj <- TrajFromCoords(
  dataHQ_ID1,
  xCol = 2,
  yCol = 3,
  timeCol = "Time",
  fps = 25,
  spatialUnits = "cm",
  timeUnits = "s"
)

trj2 <- TrajFromCoords(
  IDsub,
  xCol = 5,
  yCol = 6,
  timeCol = "Time",
  fps = 25,
  spatialUnits = "cm",
  timeUnits = "s"
)

# Plot it
plot(trj2)
plot(trj)
derivs <- TrajDerivatives(trj)
derivs

plot(derivs$acceleration ~ derivs$accelerationTimes, type = 'l', col = 'red', 
     yaxt = 'n',
     xlab = 'Time (s)',
     ylab = expression(paste('Change in speed (', cm/s^2, ')')))

axis(side = 2, col = "red")
lines(derivs$speed ~ derivs$speedTimes, col = 'blue')
axis(side = 4, col = "blue")
mtext('Speed (cm/s)', side = 4, line = 3)
abline(h = 0, col = 'lightGrey')

plot(dataHQ_ID2$Speed~dataHQ_ID2$Time, type="l")
plot(dataHQ_ID2$Moving~dataHQ_ID2$Time, type="l")

# Step of coordinates analysis ---- 
# (For one individual) 
## 1. Visualisation des data extraites du logiciel animalTA en fonction du temps
## 2. Parameters extractions (avec TrajR et handmade fonction) 
## 3. visualisation de paramètres extraits en fonction du temps et par individu. 
## 4. Statistical analysis : ACP dans un premier temps pour voir des groupes et 
## GLMM and Co pour variabilité phenotypique, répétabilité et prévisionnabilité. 
## 5. HMM pour identifier des états comportementaux et leur rythmicité.
## 6. Approches decompositions séries temporelles => SVM, analyses non supervisée/supervisée(insecticides vs non, males/femelles...). 

#. HMM : test sur une petite serie temporelle 
library(momentuHMM)



