library(ggplot2)
library(dplyr)
library(tidyr)
#http://www.roymfrancis.com/calendar-plot-with-ggplot2/
#### Printable Plan
printable_plan <- tidy_plan %>%
  group_by(Week)%>%
  mutate(weekstart = paste0(min(date)),
         month = lubridate::month(date,label = TRUE)) %>%
  group_by(month, Week, weekstart, weekday) %>%
  summarise(summary_info = paste0("Miles: ", miles,
                                  "\n Pace: ", trgt_pace,
                                  "\n Date: ", date))%>%
  ungroup() %>%
  mutate(summary_info = case_when(weekday == "Fri" ~ "REST DAY",
                                  weekday == "Mon" ~ "CROSS TRAIN",
                                  TRUE ~ summary_info),
         weekday = factor(weekday, levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')))%>%
  select(month, Week, weekday, summary_info)




ggplot(printable_plan, aes(x = weekday, y = Week))+
  geom_tile(color = "black", aes(fill = month))+
  geom_text(aes(label = summary_info))+
  theme_minimal()+
  scale_x_discrete(position = "top")+
  scale_y_reverse(breaks = c(18:1)) 
