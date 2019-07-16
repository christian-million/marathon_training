# References: http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html

# install.packages('rvest')
# install.packages('robotstxt')

# Load Necessary Packages
library(robotstxt)
library(rvest)
library(dplyr)
library(tidyr)

# URL of Website to scrape
url <- "https://www.halhigdon.com/training-programs/marathon-training/intermediate-2-marathon/"

# An Xpath (identifier?)
xpath <- '//*[@id="miles"]/div/table'

# Is is acceptable to scrape this webpage?
robotstxt::paths_allowed(url)

# Read webpage and convert all tables to a dataframe
hals_intermediate2 <- url %>%
  xml2::read_html() %>% # Received a Warning message: \n'rvest::html' is deprecated.\nUse 'xml2::read_html' instead.
  rvest::html_nodes("table") %>% # or rvest::html_nodes(xpath = "xpath")
  rvest::html_table()

# Select the dataframe that uses Miles, not Kilometer
plan <- hals_intermediate2[[1]]

# Add the dates
tidy_plan <- plan %>%
  gather(key = "weekday", value = "miles", Mon:Sun)

date_lu <- data.frame(weekday = rep(unique(tidy_plan$weekday), times = 18),
                      Week = rep(1:18, each = 7),
                      date = seq(as.Date("2019-08-05"),as.Date("2019-12-08"), by = "day"),
                      stringsAsFactors = FALSE)

tidy_plan <- tidy_plan %>%
  left_join(date_lu, by=c('weekday', 'Week')) %>%
  mutate(race_pace = grepl('pace|Marathon', miles),
         miles = case_when(grepl(' mi', miles) ~ as.numeric(gsub(' mi.*',"", miles)),
                           miles %in% c('Cross', 'Rest') ~ 0,
                           miles == "Half Marathon" ~ 13.1,
                           miles == "Marathon" ~ 26.2,
                           TRUE ~ as.numeric(miles)))
tidy_plan %>%
  mutate(three_one = ifelse(weekday == "Sun" & Week%%3 == 2, TRUE, FALSE),
         trgt_pace = case_when(race_pace ~ "08:01",
                              weekday == "Sun" ~ "08:31",
                              TRUE ~ "08:15"))

lubridate::ms("08:01") - lubridate::ms("08:10:01")
