# gps average calculation for approximation of neighborhoods
# plug each gps result into meta_neighborhood file

library(tidyverse)
source("helper_dataload_all.R")

df.gps <- df.col %>% group_by(neighb_UID, city, citylabel) %>%
        summarize(lat = mean(X_col_location_latitude, na.rm = T),
                  long = mean(X_col_location_longitude, na.rm = T))

df.gps

write.csv(df.gps, paste0(getwd(), "/data/meta_data/", "meta_gps_neighb.csv"), row.names = F, na="")
