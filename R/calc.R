library(lubridate)

#https://davidrroberts.wordpress.com/2016/04/04/running-pace-and-time-calculator-function-for-r/

calc_pace <- function(distance, time){
  
  secs <- lubridate::period_to_seconds(lubridate::hms(time))
  pace <- secs / distance
  hours <- floor(pace/60/60)
  mins <- floor(pace/60 - (hours*60))
  sec_pretty <- round((pace) - (mins*60) - (hours*60*60))
  paste(sprintf("%02d", mins), sprintf("%02d", sec_pretty), sep = ':')
  
}

calc_pace(26.2, "03:30:00")



calc_splits <- function(){
  
  
}

calc_splits(26.2, "03:30:00")