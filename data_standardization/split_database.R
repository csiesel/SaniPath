# **********************************************************************************
# **********************************************************************************
# seperate clean data for each deployment into separate files -
# by deployment

# **********************************************************************************
library(tidyverse)

# Import data:
source("helper_dataload_all.R")


df.col <- df.col %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.lab <- df.lab %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.h <- df.h %>% left_join(., meta_dply[c("id", "citylabel", "city")], by = c("dply_num" = "id"))
df.c <- df.c %>% left_join(., meta_dply[c("id", "citylabel", "city")], by = c("dply_num" = "id"))
df.s <- df.s %>% left_join(., meta_dply[c("id", "citylabel", "city")], by = c("dply_num" = "id"))
df.ecdata <- df.ecdata %>% 
        left_join(., meta_sampleID, by = c("sample_type" = "id")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("dply_num" = "id"))

#*************************************************************************************************************
# 1 ----
deployment = 1

df.col %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_col", ".csv"), row.names = F, na="")
df.lab %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_lab", ".csv"), row.names = F, na="")
df.h %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_h", ".csv"), row.names = F, na="")

#*************************************************************************************************************
# 8 ---- 
deployment = 8
df.ecdata %>% filter(dply_num == deployment) -> df
df.col %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_col", ".csv"), row.names = F, na="")
df.lab %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_lab", ".csv"), row.names = F, na="")
df.h %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_h", ".csv"), row.names = F, na="")
# df.c %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                # sprintf("%02d", deployment), "_c", ".csv"), row.names = F, na="")
# df.s %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                # sprintf("%02d", deployment), "_s", ".csv"), row.names = F, na="")

#*************************************************************************************************************
# 9 ----
deployment <- 9

df.col %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_col", ".csv"), row.names = F, na="")
df.lab %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_lab", ".csv"), row.names = F, na="")
df.h %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_h", ".csv"), row.names = F, na="")
df.c %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_c", ".csv"), row.names = F, na="")
df.s %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_s", ".csv"), row.names = F, na="")

df.ecdata %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_ecdata", ".csv"), row.names = F, na="")

meta_neighb[meta_neighb$deployment_id == deployment, ] %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                          sprintf("%02d", deployment), "_meta", ".csv"), row.names = F, na="")
#*************************************************************************************************************
#*************************************************************************************************************
# 12 Senegal ----
deployment <- 12

df.col %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_col", ".csv"), row.names = F, na="")
df.lab %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_lab", ".csv"), row.names = F, na="")
df.h %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_h", ".csv"), row.names = F, na="")
df.c %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_c", ".csv"), row.names = F, na="")
df.s %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_s", ".csv"), row.names = F, na="")

df.ecdata %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                     sprintf("%02d", deployment), "_ecdata", ".csv"), row.names = F, na="")

meta_neighb[meta_neighb$deployment_id == deployment, ] %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                               sprintf("%02d", deployment), "_meta", ".csv"), row.names = F, na="")
#*************************************************************************************************************










# 13 ----
deployment = 13

df.col %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_col", ".csv"), row.names = F, na="")
df.lab %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                  sprintf("%02d", deployment), "_lab", ".csv"), row.names = F, na="")
df.h %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_h", ".csv"), row.names = F, na="")
df.c %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_c", ".csv"), row.names = F, na="")
df.s %>% filter(dply_num == deployment) %>% write.csv(., paste0(getwd(), "/data/deployments/99_allclean/", 
                                                                sprintf("%02d", deployment), "_s", ".csv"), row.names = F, na="")









