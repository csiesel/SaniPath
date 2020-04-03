library(readxl)
library(tidyverse)
library(anytime)


##### Child Observation #####

childobs <- read_excel("~/Downloads/1-5 SO - Children - Obs - all versions - False - 2020-04-02-12-56-29.xlsx")
childobs$startdate <- as.Date(childobs$`so_startgr/so_time_start`)
childobs$`so_endgr/so_time_end`[which(childobs$`so_endgr/so_time_end`=="2019-08-14 09:58:49")] <- "2019-08-13 14:50:49"
child <-childobs %>%
  group_by(deviceid, startdate) %>%
  summarise(., min=min(`so_startgr/so_time_start`, na.rm=TRUE), max=max(`so_endgr/so_time_end`, na.rm=TRUE)) %>%
  mutate(max=anytime(max), min=anytime(min), duration=difftime(max, min, units="hours"))

#This is removing Jamie's observations
child <- filter(child, !deviceid %in% c("ee.kobotoolbox.org:4sj639LhGFXbnASb",
                                      "ee.kobotoolbox.org:uk0Tf91i44cc9x9s",
                                      "ee.kobotoolbox.org:4sj639LhGFXbnASb",
                                      "ee.kobotoolbox.org:6b7NqDTLxgxecLgc"))
#This is removing practice observatins
child <- filter(child,  startdate >=as.Date("2019-08-22"))
sum(child$duration)




##### Food Prep #####
foodprep <- read_excel("~/Downloads/1.8. SO - Food Prep - Obs - all versions - False - 2020-04-03-14-49-09.xlsx")
foodprep$startdate <- as.Date(foodprep$fo_start_dt)
food <- foodprep %>%
  group_by(startdate) %>%
  summarise(., min=min(fo_start, na.rm=TRUE), max=max(fo_end_dt_001, na.rm=TRUE)) %>%
  mutate(max=anytime(max), min=anytime(min), duration=difftime(max, min, units="hours"))
sum(food$duration)
