# function split bout and max length ----
split_by_repetition <- function(df) {
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
  res_Lb_in<-max(unlist(lapply(filtered_inactive, length)))
  return(c(res_Lb1, res_Lb_in))
}
#### 

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
