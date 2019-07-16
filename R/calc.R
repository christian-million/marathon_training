library(lubridate)

#https://davidrroberts.wordpress.com/2016/04/04/running-pace-and-time-calculator-function-for-r/

seconds_to_time <- function(sec){
  #Need to vectorize
  hours <- floor(sec/60/60)
  mins <- floor(sec/60 - (hours*60))
  sec_pretty <- round((sec) - (mins*60) - (hours*60*60))
  out <- rep(NA, length(sec_pretty))
  
  for(i in seq_along(sec_pretty)){
    if(hours[i]<1){
      out[i] <- paste(sprintf("%02d", mins[i]), sprintf("%02d", sec_pretty[i]), sep = ':')
    } else {
      out[i] <- paste(sprintf("%02d", hours[i]), sprintf("%02d", mins[i]), sprintf("%02d", sec_pretty[i]), sep = ':')
    }
  }
   return(out)
}

calc_pace <- function(distance, time){
  
  secs <- lubridate::period_to_seconds(lubridate::hms(time))
  pace <- secs / distance
  seconds_to_time(pace)
  
}

# calc_pace(26.2, "03:30:00")


calc_splits <- function(distance, time){
  
  splits <- data.frame(distance = c(1:floor(distance), distance))
  
  secs <- lubridate::period_to_seconds(lubridate::hms(time))
  
  pace <- secs / distance
  
  splits$sec <- splits$distance * pace
  
  splits$splits <- seconds_to_time(splits$sec)
  
  splits[c('distance', 'splits')]
}

calc_splits(26.2, "03:30:00")
