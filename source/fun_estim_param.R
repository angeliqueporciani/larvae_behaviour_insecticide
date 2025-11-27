#Function for script 05_estim_paramind

# CRCW 
fun_CRCW <- function(df){
  temp_df <- df %>% mutate(ID=rep("1"), x=X, y=Y)
  crwOut1 <- crawlWrap(obsData=temp_df, Time.name = "Time", time.scale = "seconds",attempts=1)
  clean_CRCW <- data.frame(crwOut1$crwPredict) # vitesse estimée par la function crawlwrap pas euclidienne ou bizare, donc recalcul
  return(clean_CRCW)
}

poss.CRCW = possibly(.f = fun_CRCW, otherwise = NA)


# Step length per seconde estimation  

## function speed (step length)

# step_fun <- function(df, x_col = "x", y_col = "y") {
#   # vérification des colonnes
#   if (!all(c(x_col, y_col) %in% names(df))) {
#     stop("Les colonnes spécifiées n'existent pas dans le data.frame.")
#   }
#   
#   x <- df[[x_col]]
#   y <- df[[y_col]]
#   
#   # calcul des distances entre chaque point consécutif
#   step_dist <- c(NA, sqrt(diff(x)^2 + diff(y)^2))
#   
#   # ajout au dataframe
#   df$step_distance <- step_dist
#   return(df)
# }

# simplifcation comme les noms de colonne ne change pas 
step_fun <- function(df) {
  
  x <- df$mu.x
  y <- df$mu.y
  
  # calcul des distances entre chaque point consécutif
  step_dist <- c(NA, sqrt(diff(x)^2 + diff(y)^2))
  
  # ajout au dataframe
  df$step_distance <- step_dist
  
  # estimation of move 1-0 
  df$Move <- ifelse(df$step_distance>0.3, 1, 0)
  return(df)
}

poss.step_fun <-  possibly(.f = step_fun, otherwise = NA )

## function max speed and bout length estimation 

fun_max_speed <- function(df){max(df$step_distance, na.rm = TRUE)}
poss.max_speed <-  possibly(.f = fun_max_speed, otherwise = NA )


fun_Lbout_activ <- function(df) {
  vec <- df$Move
  if (sum(is.na(vec))>0){
    vec <- na.locf(vec)
  }
  if (sum(is.na(vec))==0){
    vec <- vec}
  # Identify the indices where consecutive numbers are broken or a repetition changes
  breaks <- c(0, which(diff(vec) != 0), length(vec))
  
  # Use those break points to split the vector into sublists
  result <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
    vec[(breaks[i] + 1):breaks[i + 1]]
  })
  
  # now keep only ones corresponding to 1 
  filtered_active <- Filter(function(x) all(x == 1), result)
  
  # same for 0 
  filtered_inactive <- Filter(function(x) all(x == 0), result)
  
  res_Lb1<-max(unlist(lapply(filtered_active, length)))
  #res_Lb_in<-max(unlist(lapply(filtered_inactive, length)))
  return(res_Lb1)
}

fun_Lbout_inactiv <- function(df) {
  vec <- df$Move
  if (sum(is.na(vec))>0){
    vec <- na.locf(vec)
  }
  if (sum(is.na(vec))==0){
    df$vec <- df$vec}
  # Identify the indices where consecutive numbers are broken or a repetition changes
  breaks <- c(0, which(diff(vec) != 0), length(vec))
  
  # Use those break points to split the vector into sublists
  result <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
    vec[(breaks[i] + 1):breaks[i + 1]]
  })
  
  # now keep only ones corresponding to 1 
  filtered_active <- Filter(function(x) all(x == 1), result)
  
  # same for 0 
  filtered_inactive <- Filter(function(x) all(x == 0), result)
  
  #res_Lb1<-max(unlist(lapply(filtered_active, length)))
  res_Lb_in<-max(unlist(lapply(filtered_inactive, length)))
  return(res_Lb_in)
}

poss.fun_Lbout_activ <-  possibly(.f = fun_Lbout_activ, otherwise = NA )
poss.fun_Lbout_inactiv <-  possibly(.f = fun_Lbout_inactiv, otherwise = NA )
