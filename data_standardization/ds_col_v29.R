# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# Form: Sample collection (col)
# **********************************************************************************
# Deployments: 1-13
# **********************************************************************************
# Last modified: February 12, 2020
# By: Wolfgang
# Version: v29
# **********************************************************************************


#---- SET-UP ----
source("helper_dataload_meta.R")

# read master form
df.col.0 <- read.table(paste0(getwd(), "/data/deployments/00_master/", "SAMPLE_09_20_17_WM - xml - 2017-10-31-18-23", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)
# read cambodia form
df.col.1 <- read.table(paste0(getwd(), "/data/deployments/01_cambodia/", "SP CAMBO DATASET - sample", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)
# read bangladesh form
df.col.2 <- read.table(paste0(getwd(), "/data/deployments/02_bangladesh/", "sample_clean_092217_wm", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T, na.strings="n/a")
# read ghana form
df.col.3 <- read.csv(paste0(getwd(), "/data/deployments/03_ghana/", "ghana col", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))
# read zambia form
df.col.4 <- read.csv(paste0(getwd(), "/data/deployments/04_zambia/", "col_lusaka", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.col.5 <- read.csv(paste0(getwd(), "/data/deployments/05_mozambique/", "compiled_env1", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana accra form
df.col.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "col_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.col.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "col_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique2 form
df.col.8 <- read.csv(paste0(getwd(), "/data/deployments/08_mozambique2/", "compiled_env_2016", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read uganda form
df.col.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/", "col_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read atl form
df.col.10 <- read.csv(paste0(getwd(), "/data/deployments/10_usa-atl/", "Atlanta_Sample_FINAL0", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read vellore form
df.col.11 <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/", "Samples", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))

# read senegal form
df.col.12 <- read.csv(paste0(getwd(), "/data/deployments/12_senegal/", 
                             "Sample_Survey-xlsform_waspa_final_2020_02_05_17_51_51", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))


# read lusaka2 form
df.col.13 <- read.csv(paste0(getwd(), "/data/deployments/13_zambia2/", "Sample_Survey-xlsform_lusaka2019_final_2019_11_04_18_49_22", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# **********************************************************************************
#---- 0 MASTER -----
##### create new variables #####
df.col.0$col_extra_data <- NA
df.col.0$dply_num <-NA
df.col.0$col_UID <- NA
df.col.0$col_UID <- as.character(df.col.0$col_UID)

df.col.0$col_date <- NA
df.col.0$col_date <- as.Date(df.col.0$col_date, format = "%Y-%m-%d")

df.col.0$col_sample_type_alt <- NA
##### merge datasets #####
# Clean up DFO
# delete rows and override the original table
df.col.0 <- df.col.0[-c(1:2),]

# **********************************************************************************

# **********************************************************************************
#---- 1 CAMBODIA -----
##### rename variables #####
colnames(df.col.1)[colnames(df.col.1) == 'start_dt'] <- 'col_start_dt'
colnames(df.col.1)[colnames(df.col.1) == 'end_dt'] <- 'col_end_dt'
colnames(df.col.1)[colnames(df.col.1) == 'col_d_businesses'] <- 'col_d_shops'

##### recode #####
df.col.1$col_extra_data <- NA

df.col.1$col_extra_data <- ifelse(df.col.1$col_sample_type == 20, 1,
                                  ifelse(df.col.1$col_sample_type == 21, 1,
                                         ifelse(df.col.1$col_sample_type == 22, 1,
                                                ifelse(df.col.1$col_sample_type == 23, 1, 
                                                       ifelse(df.col.1$col_bw_type ==  5, 1, 0)))))

# alternative sample types (e.g. bottled water -> drinking water)
df.col.1$col_sample_type_alt <- case_when(df.col.1$col_sample_type ==  20 ~ "Ice",
                                          df.col.1$col_sample_type ==  21 ~ "Drinking Water", 
                                          df.col.1$col_sample_type ==  22 ~ "Bottled Water",
                                          df.col.1$col_sample_type ==  23 ~ "Well Water")
# recode col_sample_type:
# Mark as having extra data (above)
# Ice, Drinking Water: Water Supply, Drinking Water: Bottled Water, and Drinking Water: Well Water
# classified as Drinking Water
df.col.1$col_sample_type[df.col.1$col_sample_type ==  20] <- 33
df.col.1$col_sample_type[df.col.1$col_sample_type ==  21] <- 3
df.col.1$col_sample_type[df.col.1$col_sample_type ==  22] <- 33
df.col.1$col_sample_type[df.col.1$col_sample_type ==  23] <- 33


# recode col_pa_type
df.col.1$col_pa_type[df.col.1$col_pa_type ==  3] <- 88

# Create new column to retain original values
df.col.1$col_p_type_ch <- df.col.1$col_p_type

# Then recode with numeric values
df.col.1$col_p_type[df.col.1$col_p_type ==  "Cabbages"] <- 1
df.col.1$col_p_type[df.col.1$col_p_type ==  "Cabbage"] <- 1
df.col.1$col_p_type[df.col.1$col_p_type ==  "Cucumber"] <- 2
df.col.1$col_p_type[df.col.1$col_p_type ==  "Egg plan"] <- 3
df.col.1$col_p_type[df.col.1$col_p_type ==  "Egg plant"] <- 3
df.col.1$col_p_type[df.col.1$col_p_type ==  "Lettuce"] <- 4
df.col.1$col_p_type[df.col.1$col_p_type ==  "Long bean"] <- 5
df.col.1$col_p_type[df.col.1$col_p_type ==  "Long plant"] <- 6
df.col.1$col_p_type[df.col.1$col_p_type ==  "Salad"] <- 7
df.col.1$col_p_type[df.col.1$col_p_type ==  "Tomatoes"] <- 8
df.col.1$col_p_type[df.col.1$col_p_type ==  "Water mimosa"] <- 9
df.col.1$col_p_type[df.col.1$col_p_type ==  "Wing bean"] <- 10

# Change object type
df.col.1$col_p_type <- as.numeric(df.col.1$col_p_type)

# Mark as having extra data (above) and group rain water with "Other."
df.col.1$col_bw_type[df.col.1$col_bw_type ==  4] <- 88
df.col.1$col_bw_type[df.col.1$col_bw_type ==  5] <- 88

df.col.1$col_s_type[df.col.1$col_s_type ==  3] <- 88

df.col.1$col_d_water_level <- df.col.1$col_d_water_level-1

table(df.col.1$col_sample_type)
##### create new variables #####
# Create country or deployment code
df.col.1$dply_num <- 1

# Create unique ID variable
df.col.1$col_UID <- NA

#For Cambodia, we have changed some of the sample types above.

# get string from positon 4 to 6, last three characters
# substr(df.col.1$col_id, 4, 6)



# Create date variable
df.col.1$col_date <- NA
df.col.1$col_date <- substr(df.col.1$col_start_dt, 1, 10)
df.col.1$col_date <- as.Date(df.col.1$col_date, format = "%Y-%m-%d")

# Note: there are some samples that do not have lab data available
# deleting these datapoints here.
df.col.1 <- df.col.1 %>% 
        filter(!col_id %in% c("Dwb403", "Dwc301", "Dwc302", "Dwc303", "Dwc304", "Dwc305"))

df.col.1$col_id <- (1000:(1000+nrow(df.col.1)-1))
df.col.1$col_id_val <- df.col.1$col_id

# paste number together
df.col.1$col_UID <- paste0(sprintf("%02d", df.col.1$dply_num),
                           "_",
                           sprintf("%02d", df.col.1$col_sample_type),
                           "_",
                           sprintf("%04d", as.numeric(df.col.1$col_id)))

##### deployment-specific info #####
# find columns that are in df1 but not in df0
extra1 <- which(colnames(df.col.1) %in% setdiff(colnames(df.col.1), colnames(df.col.0)))

# remove "extra" columns from df1
df.col.1.clean <- df.col.1[, -extra1]

# export extra data
df.col.1.extra <- df.col.1[, extra1]
df.col.1.extra$col_UID <- df.col.1$col_UID

##### merge datasets #####
df.col.0$subscriberid <- as.numeric(df.col.0$subscriberid)
df.col.0$simserial <- as.numeric(df.col.0$simserial)
df.col.0$deviceid <- as.numeric(df.col.0$deviceid)
# df.col.0$col_id <- as.character(df.col.0$col_id)
# df.col.0$col_id_val <- as.character(df.col.0$col_id_val)

# check that dataframes have same dimensions
setdiff(colnames(df.col.1.clean), colnames(df.col.0))

# merge clean datasets
df.col <- bind_rows(df.col.0, df.col.1.clean)

# check that merged dataset has same dimensions as master
setdiff(colnames(df.col), colnames(df.col.0))
# **********************************************************************************

# **********************************************************************************
#---- 2 BANGLADESH -----
##### rename variables #####
colnames(df.col.2)[colnames(df.col.2) == 'start_dt'] <- 'col_start_dt'
colnames(df.col.2)[colnames(df.col.2) == 'end_dt'] <- 'col_end_dt'

##### recode #####
df.col.2$col_extra_data <- NA

df.col.2$col_extra_data <- ifelse(df.col.2$col_sample_type == 3, 1,
                                  ifelse(df.col.2$col_sample_type == 10, 1, 
                                         ifelse(df.col.2$col_bw_type == 4, 1, 0)))

df.col.2$col_sample_type_alt <- case_when(df.col.2$col_sample_type ==  10 ~ "Other Drinking Water")

# recode col_sample_type: 
# Mark as having extra data (above)
# Reclassify Other Drinking Water, Street Food
df.col.2$col_sample_type[df.col.2$col_sample_type ==  10] <- 33
df.col.2$col_sample_type[df.col.2$col_sample_type ==  11] <- 10

# recode col_pa_type
# table(df.col.2$col_pa_type)
df.col.2$col_pa_type[df.col.2$col_pa_type ==  4] <- 88


# recode col_bw_type
# table(df.col.2$col_bw_type)
# Mark as having extra data (above)
df.col.2$col_bw_type[df.col.2$col_bw_type == 5] <- 88
df.col.2$col_bw_type[df.col.2$col_bw_type == 4] <- 88

# recode col_p_type (produce)
# table(df.col.2$col_p_type_other) 
#all NA
df.col.2$col_p_type[df.col.2$col_p_type == 1] <- 8      # recodes tomatoes as 8
df.col.2$col_p_type[df.col.2$col_p_type == 3] <- 16     # recodes corriander as 11
df.col.2$col_p_type[df.col.2$col_p_type == 4] <- 88     # recodes other as 88

# Recode col_dw_type
df.col.2$col_dw_type[df.col.2$col_dw_type == 6] <- 88

# Recode col_l_toilet_type
# table(df.col.2$col_l_toilet_type)

# NOTE: Use of operators would require changing object type, but could run this way instead:
# df.col.2$col_l_toilet_type <- as.numeric(df.col.2$col_l_toilet_type)
# df.col.2$col_l_toilet_type[which(df.col.2$col_l_toilet_type>=4)] <- df.col.2$col_l_toilet_type[which(df.col.2$col_l_toilet_type>=4)]+1
# df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 10] <- 88

# to keep as character
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 9] <- 88
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 8] <- 9
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 7] <- 8
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 6] <- 7
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 5] <- 6
df.col.2$col_l_toilet_type[df.col.2$col_l_toilet_type == 4] <- 5

# Recode col_sf_type (street food)
# table(df.col.2$col_sf_type)

#Recode col_neighborhood
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 11] <- 1
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 22] <- 2
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 33] <- 3
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 44] <- 4
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 55] <- 5
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 66] <- 6
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 77] <- 7
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 88] <- 8
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 99] <- 9
df.col.2$col_neighborhood[df.col.2$col_neighborhood == 0] <- 10

##### create new variables #####
# Create country or deployment code
df.col.2$dply_num <- 2

#Create unique ID variable
df.col.2$col_UID <- NA

# paste number together
df.col.2$col_UID <- paste0(sprintf("%02d", df.col.2$dply_num),
                           "_",
                           sprintf("%02d", df.col.2$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.2$col_id))

# Create date variable
df.col.2$col_date <- NA
df.col.2$col_date <- substr(df.col.2$col_start_dt, 1, 10)
df.col.2$col_date <- as.Date(df.col.2$col_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# Find columns that are in df2 but not in df.col
extra2 <- which(colnames(df.col.2) %in% setdiff(colnames(df.col.2), colnames(df.col.0)))

# Remove "extra" columns from df2
df.col.2.clean <- df.col.2[, -extra2]

# export extra data
df.col.2.extra <- df.col.2[, extra2]
df.col.2.extra$col_UID <- df.col.2$col_UID

##### merge datasets #####
# check that dataframes have same dimensions
setdiff(colnames(df.col.2.clean), colnames(df.col.0))

# merge clean datasets
df.col <- bind_rows(df.col, df.col.2.clean)
# **********************************************************************************

# **********************************************************************************
#---- 3 GHANA -----
##### rename variables #####
colnames(df.col.3)[colnames(df.col.3) == 'started_time'] <- 'col_start_dt'
# rename form.neighbor when recoding below
colnames(df.col.3)[colnames(df.col.3) == 'form.sample_type'] <- 'col_sample_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.sampleid'] <- 'col_id'

colnames(df.col.3)[colnames(df.col.3) == 'form.ev_lat_lon'] <- 'col_location'

colnames(df.col.3)[colnames(df.col.3) == 'form.currently_raining'] <- 'col_weather'
colnames(df.col.3)[colnames(df.col.3) == 'form.rain_last_day'] <- 'col_rainyest'

colnames(df.col.3)[colnames(df.col.3) == 'form.source_dist'] <- 'col_pa_prox'
colnames(df.col.3)[colnames(df.col.3) == 'form.particle_sample_type'] <- 'col_pa_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.particle_sample_type_other'] <- 'col_pa_type_other'
colnames(df.col.3)[colnames(df.col.3) == 'form.dis_lat'] <- 'col_pa_latrine_distance'

colnames(df.col.3)[colnames(df.col.3) == 'form.produce_type'] <- 'col_p_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.produce_quantity'] <- 'col_p_num'
colnames(df.col.3)[colnames(df.col.3) == 'form.source_ty'] <- 'col_dw_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.other_source'] <- 'col_dw_type_other'
colnames(df.col.3)[colnames(df.col.3) == 'form.containr'] <- 'col_dw_container'
colnames(df.col.3)[colnames(df.col.3) == 'form.covered'] <- 'col_dw_container_lid'
colnames(df.col.3)[colnames(df.col.3) == 'form.free_cl'] <- 'col_dw_cl_free'
colnames(df.col.3)[colnames(df.col.3) == 'form.total_cl'] <- 'col_dw_cl_total'

colnames(df.col.3)[colnames(df.col.3) == 'form.source_type'] <- 'col_l_toilet_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.source_type_other'] <- 'col_l_toilet_type_other'
colnames(df.col.3)[colnames(df.col.3) == 'form.n_stall'] <- 'col_l_toilet_stalls'
colnames(df.col.3)[colnames(df.col.3) == 'form.vis_fec'] <- 'col_l_toilet_feces'
colnames(df.col.3)[colnames(df.col.3) == 'form.latrine_user_num'] <- 'col_l_toilet_users'
colnames(df.col.3)[colnames(df.col.3) == 'form.hw_stat'] <- 'col_l_toilet_hw'

colnames(df.col.3)[colnames(df.col.3) == 'form.surface_source_ty'] <- 'col_s_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.surface_source_ty_other'] <- 'col_s_type_other'

colnames(df.col.3)[colnames(df.col.3) == 'form.water_level'] <- 'col_d_water_level'
colnames(df.col.3)[colnames(df.col.3) == 'form.water_flow'] <- 'col_d_water_flow'
colnames(df.col.3)[colnames(df.col.3) == 'form.drain_size'] <- 'col_d_size'
colnames(df.col.3)[colnames(df.col.3) == 'form.drain_lining'] <- 'col_d_lining'
colnames(df.col.3)[colnames(df.col.3) == 'form.feces_near'] <- 'col_d_feces'
colnames(df.col.3)[colnames(df.col.3) == 'form.animals_near'] <- 'col_d_animals'
colnames(df.col.3)[colnames(df.col.3) == 'form.schools_present'] <- 'col_d_schools'
colnames(df.col.3)[colnames(df.col.3) == 'form.orn_ag_present'] <- 'col_d_urban_ag'
colnames(df.col.3)[colnames(df.col.3) == 'form.pub_latrines_present'] <- 'col_d_public_latrines'
colnames(df.col.3)[colnames(df.col.3) == 'form.hh_latrines_present'] <- 'col_d_hh_latrines'
colnames(df.col.3)[colnames(df.col.3) == 'form.businesses_present'] <- 'col_d_shops'
colnames(df.col.3)[colnames(df.col.3) == 'form.housolds_present'] <- 'col_d_hh'
colnames(df.col.3)[colnames(df.col.3) == 'form.num_households_present'] <- 'col_d_hh_num'

colnames(df.col.3)[colnames(df.col.3) == 'form.source_ty_ocean'] <- 'col_o_type'
colnames(df.col.3)[colnames(df.col.3) == 'form.other_source_ocean'] <- 'col_o_type_other'

colnames(df.col.3)[colnames(df.col.3) == 'form.notes'] <- 'col_notes'
colnames(df.col.3)[colnames(df.col.3) == 'form.collection_enumerator_names'] <- 'col_enum'

##### recode #####
# Create another variable to indicate when we are collapsing categories and extra data is available (code 0/1)
df.col.3$col_extra_data <- NA

df.col.3$col_extra_data <- ifelse(df.col.3$col_dw_type == 4, 1, 
                                  ifelse(df.col.3$col_d_water_level == 0, 1, 
                                         ifelse(df.col.3$col_l_toilet_type == 1, 1, 
                                                ifelse(df.col.3$col_l_toilet_type == 3, 1,
                                                       ifelse(df.col.3$col_l_toilet_type == 4, 1,
                                                              ifelse(df.col.3$col_l_toilet_type == 5, 1,
                                                                     ifelse(df.col.3$col_l_toilet_type == 6, 1, 0)))))))

# Code col_neighborhood as numeric
neigh <- meta_neighb[meta_neighb$deployment_id == 3 ,][,4:6]
colnames(neigh) <- c("col_neighborhood", "col_neigh", "neighborhood")

df.col.3 <- left_join(df.col.3, neigh, by=c("form.neighbor" = "neighborhood"))

# Recode missing values for col_location
df.col.3$col_location[df.col.3$col_location == ""] <- NA

# Code sunny/cloudy weather as missing 
# NOTE: is there a better way to do this?
df.col.3$col_weather[df.col.3$col_weather ==  1] <- 3
df.col.3$col_weather[df.col.3$col_weather ==  0] <- NA

# Recode col_pa_prox
df.col.3$col_pa_prox[df.col.3$col_pa_prox ==  0] <- 4

# Recode col_p_type (produce)
# NOTE: This was free answer in Ghana survey. Master/other deployments gave 3 options. 
# Should we collapse categories? 

# Create new column to retain original values
df.col.3$col_p_type_ch <- df.col.3$col_p_type

# Then recode with numeric values
df.col.3$col_p_type[df.col.3$col_p_type ==  "Cabbage"] <- 1

df.col.3$col_p_type[df.col.3$col_p_type ==  "cucumber"] <- 2

df.col.3$col_p_type[df.col.3$col_p_type ==  "Lettuce"] <- 4
df.col.3$col_p_type[df.col.3$col_p_type ==  "lettuce"] <- 4

df.col.3$col_p_type[df.col.3$col_p_type ==  "Tomatoes"] <- 8
df.col.3$col_p_type[df.col.3$col_p_type ==  "tomatoes"] <- 8
df.col.3$col_p_type[df.col.3$col_p_type ==  "Tomato"] <- 8
df.col.3$col_p_type[df.col.3$col_p_type ==  "tomato"] <- 8

df.col.3$col_p_type[df.col.3$col_p_type ==  "Apple"] <- 11

df.col.3$col_p_type[df.col.3$col_p_type ==  "Carrot"] <- 12
df.col.3$col_p_type[df.col.3$col_p_type ==  "carrot"] <- 12
df.col.3$col_p_type[df.col.3$col_p_type ==  "carrots"] <- 12

df.col.3$col_p_type[df.col.3$col_p_type ==  "green pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "Green pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "Green Pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "pepper (green)"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "pepper(green)"] <- 13

df.col.3$col_p_type[df.col.3$col_p_type ==  "red pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "Red pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "pepper(red)"] <- 13

df.col.3$col_p_type[df.col.3$col_p_type ==  "pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "Pepper"] <- 13
df.col.3$col_p_type[df.col.3$col_p_type ==  "Peper"] <- 13

df.col.3$col_p_type[df.col.3$col_p_type ==  "spring onion"] <- 14
df.col.3$col_p_type[df.col.3$col_p_type ==  "Spring onions"] <- 14


# Change object type
df.col.3$col_p_type <- as.numeric(df.col.3$col_p_type)

# Recode col_l_toilet_type
# Mark as having extra info (above)
# CHECK: do any other options match up?
table(df.col.3$col_l_toilet_type)
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 1] <- 5
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 2] <- 6
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 3] <- 88
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 4] <- 2
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 5] <- 2
df.col.3$col_l_toilet_type[df.col.3$col_l_toilet_type == 6] <- 6


# Recode col_dw_type
# Mark as having extra data (above)
df.col.3$col_dw_type[df.col.3$col_dw_type == 4] <- 88

# Recode col_d_water_level
# Mark as having extra data (above)
# treat 'dry' as missing
df.col.3$col_d_water_level[df.col.3$col_d_water_level == 0] <- NA

# Recode col_d_lining
df.col.3$col_d_lining[df.col.3$col_d_lining ==  3] <- 22 # placeholder for dirt
df.col.3$col_d_lining[df.col.3$col_d_lining ==  2] <- 3 # recode stones as 3
df.col.3$col_d_lining[df.col.3$col_d_lining ==  22] <- 2 # recode dirt as 2

##### create new variables #####

# Create country or deployment code
df.col.3$dply_num <- 3

#Create unique ID variable
df.col.3$col_UID <- NA

# paste number together
df.col.3$col_UID <- paste0(sprintf("%02d", df.col.3$dply_num),
                           "_",
                           sprintf("%02d", df.col.3$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.3$col_id))

# Create date variable
colnames(df.col.3)[colnames(df.col.3) == 'form.ec_dd'] <- 'col_date'
df.col.3$col_date <- as.Date(df.col.3$col_date, format = "%Y-%m-%d")

# Create separate location variables using form.ev_lat_lon (now col_location)
df.col.3 <- separate(df.col.3, col_location, c("X_col_location_latitude","X_col_location_longitude",
                                               "X_col_location_altitude","X_col_location_precision"), 
                     sep=" ", remove = FALSE)

# delete duplicates
df.col.3 <- df.col.3[!duplicated(df.col.3$col_id), ]

# delete samples with no lab data 
del <- c(230, 861, 725, 368, 321, 779, 881, 383, 863,842, 642, 864, 862, 511, 849, 485, 297, 492, 517)
df.col.3 <- df.col.3 %>% 
        filter(!col_id %in% del)

##### deployment-specific info #####

# Find columns that are in df3 but not in df.col
extra3 <- which(colnames(df.col.3) %in% setdiff(colnames(df.col.3), colnames(df.col.0)))

# Remove "extra" columns from df2
df.col.3.clean <- df.col.3[, -extra3]

# export extra data
df.col.3.extra <- df.col.3[, extra3]
df.col.3.extra$col_UID <- df.col.3$col_UID

##### merge datasets #####
# Adjust column type
# df.col.3.clean$col_id <- as.character(df.col.3.clean$col_id)
df.col.3.clean$X_col_location_latitude <- as.numeric(df.col.3.clean$X_col_location_latitude)
df.col.3.clean$X_col_location_longitude <- as.numeric(df.col.3.clean$X_col_location_longitude)
df.col.3.clean$X_col_location_altitude <- as.numeric(df.col.3.clean$X_col_location_altitude)
df.col.3.clean$X_col_location_precision <- as.numeric(df.col.3.clean$X_col_location_precision)

# check that dataframes have same dimensions
setdiff(colnames(df.col.3.clean), colnames(df.col.0))

# merge clean datasets
df.col <- bind_rows(df.col, df.col.3.clean)

# **********************************************************************************

# **********************************************************************************
#---- 4 ZAMBIA -----
##### rename variables #####
colnames(df.col.4)[colnames(df.col.4) == 'col_start'] <- 'col_start_dt'

##### recode #####
df.col.4$col_p_type[df.col.4$col_p_type ==  88] <- 15 #guava
df.col.4$col_p_type[df.col.4$col_p_type ==  2] <- 11 #apples
df.col.4$col_p_type[df.col.4$col_p_type ==  1] <- 2 #cucumber

# recode SF type
# table(df.col.4$col_sf_type)
df.col.4$col_sf_type[df.col.4$col_sf_type ==  1] <- 4 #roasted Maize
df.col.4$col_sf_type[df.col.4$col_sf_type ==  3] <- 5 #fritters
df.col.4$col_sf_type[df.col.4$col_sf_type ==  88] <- 6 #scones


# alternative sample types (e.g. bottled water -> drinking water)
df.col.4$col_sample_type_alt <- case_when(df.col.4$col_odw_type ==  1 ~ "Borehole",
                                          df.col.4$col_odw_type ==  2 ~ "Shallow Well")
# recode col_sample_type:

df.col.4$col_sample_type[df.col.4$col_sample_type ==  11] <- 33 #other drinking water

#indicate extra data
df.col.4$col_extra_data <- NA

df.col.4$col_extra_data <- case_when(
        df.col.4$col_f_type >= 0 ~ 1,
        df.col.4$col_p_wash >= 0 ~ 1,
        df.col.4$col_odw_type >= 0 ~ 1,
        df.col.4$col_odw_container >= 0 ~ 1,
        df.col.4$col_odw_container_lid >= 0 ~ 1,
        df.col.4$col_odw_id >= 0 ~ 1,
        df.col.4$col_odw_cl_free >= 0 ~ 1,
        df.col.4$col_odw_cl_total >= 0 ~ 1,
        df.col.4$col_l_toilet_pubshare >= 0 ~ 1,
        df.col.4$col_l_toilet_hhs >= 0 ~ 1
)

##### create new variables #####
# Create country or deployment code
df.col.4$dply_num <- 4

#Create unique ID variable
df.col.4$col_UID <- NA

# paste number together
df.col.4$col_UID <- paste0(sprintf("%02d", df.col.4$dply_num),
                           "_",
                           sprintf("%02d", df.col.4$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.4$col_id))

# Create date variable
df.col.4$col_date <- substr(df.col.4$col_start_dt, 1, 10)
df.col.4$col_date <- as.Date(df.col.4$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
# Find columns that are in df3 but not in df.col
extra4 <- which(colnames(df.col.4) %in% setdiff(colnames(df.col.4), colnames(df.col.0)))

# Remove "extra" columns from df2
df.col.4.clean <- df.col.4[, -extra4]

# export extra data
df.col.4.extra <- df.col.4[, extra4]
df.col.4.extra$col_UID <- df.col.4$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.4.clean)
# **********************************************************************************

# **********************************************************************************
#---- 5 MOZAMBIQUE -----
setdiff(colnames(df.col.5), colnames(df.col.0))

##### rename variables #####

colnames(df.col.5)[colnames(df.col.5) == 'sp_type'] <- 'col_sample_type'
colnames(df.col.5)[colnames(df.col.5) == 'neighbor'] <- 'col_neighborhood'
colnames(df.col.5)[colnames(df.col.5) == 'ev_lat'] <- 'X_col_location_latitude'
colnames(df.col.5)[colnames(df.col.5) == 'ev_long'] <- 'X_col_location_longitude'
colnames(df.col.5)[colnames(df.col.5) == 'feces_3m'] <- 'col_pa_prox'
colnames(df.col.5)[colnames(df.col.5) == 'dis_lat'] <- 'col_pa_latrine_distance'

colnames(df.col.5)[colnames(df.col.5) == 'source_bw'] <- 'col_bw_type'
colnames(df.col.5)[colnames(df.col.5) == 'source_other'] <- 'col_bw_type_other'
colnames(df.col.5)[colnames(df.col.5) == 'stored'] <- 'col_bw_container'
colnames(df.col.5)[colnames(df.col.5) == 'covered'] <- 'col_bw_container_lid'

colnames(df.col.5)[colnames(df.col.5) == 'lat_type'] <- 'col_l_toilet_type'
colnames(df.col.5)[colnames(df.col.5) == 'n_stall'] <- 'col_l_toilet_stalls'
colnames(df.col.5)[colnames(df.col.5) == 'Vis_fec'] <- 'col_l_toilet_feces'
colnames(df.col.5)[colnames(df.col.5) == 'HW_stat'] <- 'col_l_toilet_hw'


# date
df.col.5$ec_sd <- as.Date(df.col.5$ec_sd, "%m/%d/%Y")
df.col.5$ec_isd <- as.Date(df.col.5$ec_isd, "%m/%d/%Y")
df.col.5$ec_ied <- as.Date(df.col.5$ec_ied, "%m/%d/%Y")
df.col.5$ec_st <- format(strptime(df.col.5$ec_st, "%I:%M:%S %p"), "%H:%M:%S") 
df.col.5$ec_ist <- format(strptime(df.col.5$ec_ist, "%I:%M:%S %p"), "%H:%M:%S") 
df.col.5$ec_iet <- format(strptime(df.col.5$ec_iet, "%I:%M:%S %p"), "%H:%M:%S") 

# join date 
df.col.5$col_start_dt <- paste0(df.col.5$ec_sd,
                                  "T",
                                  df.col.5$ec_st)


##### recode #####
df.col.5$col_sample_type <- case_when(
        df.col.5$col_sample_type == 1 ~ 9,
        df.col.5$col_sample_type == 2 ~ 1,
        df.col.5$col_sample_type == 3 ~ 6,
        df.col.5$col_sample_type == 4 ~ 8,
        df.col.5$col_sample_type == 5 ~ 7)

df.col.5$col_bw_type[df.col.5$col_bw_type ==  4] <- 88

df.col.5$col_l_toilet_type_other <- ifelse(df.col.5$col_l_toilet_type == 3, "Bucket/ Pan", NA)

df.col.5$col_l_toilet_type <- case_when(
        df.col.5$col_l_toilet_type == 1 ~ 5,
        df.col.5$col_l_toilet_type == 2 ~ 6,
        df.col.5$col_l_toilet_type == 3 ~ 88,
        df.col.5$col_l_toilet_type == 4 ~ 1,
        df.col.5$col_l_toilet_type == 5 ~ 1,
        df.col.5$col_l_toilet_type == 6 ~ 88)

df.col.5$col_pa_prox <- ifelse(df.col.5$col_sample_type == 8, 2, NA)

df.col.5$X_col_location_latitude <- df.col.5$X_col_location_latitude * -1

##### create new variables #####
# Create country or deployment code
df.col.5$dply_num <- 5

# Create unique ID variable
df.col.5$col_UID <- NA

df.col.5$col_id <- 1000:(1000+nrow(df.col.5)-1)
df.col.5$col_id_val <- df.col.5$col_id

# paste number together
df.col.5$col_UID <- paste0(sprintf("%02d", df.col.5$dply_num),
                           "_",
                           sprintf("%02d", df.col.5$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.5$col_id))

# Create date variable
df.col.5$col_date <- df.col.5$ec_sd
df.col.5$col_date <- as.Date(df.col.5$col_date, format = "%Y-%m-%d")



##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra5 <- which(colnames(df.col.5) %in% setdiff(colnames(df.col.5), colnames(df.col.0)))

# remove "extra" columns from df4
df.col.5.clean <- df.col.5[, -extra5]

# export extra data
df.col.5.extra <- df.col.5[, extra5]
df.col.5.extra$col_UID <- df.col.5$col_UID

##### merge datasets #####
setdiff(colnames(df.col.5.clean), colnames(df.col.0))


df.col <- bind_rows(df.col, df.col.5.clean)
# **********************************************************************************

# **********************************************************************************
#---- 6 GHANA Accra -----
# setdiff(colnames(df.col.6), colnames(df.col.0))

##### rename variables #####
colnames(df.col.6)[colnames(df.col.6) == 'col_start'] <- 'col_start_dt'

##### recode #####
df.col.6$col_p_type[df.col.6$col_p_type ==  1] <- 8 #Tomatoe
df.col.6$col_p_type[df.col.6$col_p_type ==  2] <- 13 #pepper
df.col.6$col_p_type[df.col.6$col_p_type ==  3] <- 4 #lettuce
df.col.6$col_p_type[df.col.6$col_p_type ==  88] <- 4 #lettuce

# recode SF type
# table(df.col.6$col_sf_type)
df.col.6$col_sf_type[df.col.6$col_sf_type ==  1] <- 7 #Waakye
df.col.6$col_sf_type[df.col.6$col_sf_type ==  2] <- 8 #Kenkey
df.col.6$col_sf_type[df.col.6$col_sf_type ==  3] <- 9 #beans

#indicate extra data
df.col.6$col_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.col.6$dply_num <- 6

#Create unique ID variable
df.col.6$col_UID <- NA

# paste number together
df.col.6$col_UID <- paste0(sprintf("%02d", df.col.6$dply_num),
                           "_",
                           sprintf("%02d", df.col.6$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.6$col_id))

# Create date variable
df.col.6$col_date <- substr(df.col.6$col_start_dt, 1, 10)
df.col.6$col_date <- as.Date(df.col.6$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
df.col.6$col_neighborhood[df.col.6$col_neighborhood == 268] <- 1
df.col.6$col_neighborhood[df.col.6$col_neighborhood == 269] <- 2

# Find columns that are in df but not in df.col
extra6 <- which(colnames(df.col.6) %in% setdiff(colnames(df.col.6), colnames(df.col.0)))

# Remove "extra" columns from df
df.col.6.clean <- df.col.6[, -extra6]

# export extra data
df.col.6.extra <- df.col.6[, extra6]
df.col.6.extra$col_UID <- df.col.6$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.6.clean)
# **********************************************************************************

# **********************************************************************************
#---- 7 GHANA Kumasi -----
# setdiff(colnames(df.col.7), colnames(df.col.0))

##### rename variables #####
colnames(df.col.7)[colnames(df.col.7) == 'col_start'] <- 'col_start_dt'

##### recode #####
df.col.7$col_p_type[df.col.7$col_p_type ==  1] <- 8 #tomato
df.col.7$col_p_type[df.col.7$col_p_type ==  2] <- 1 #cabbage
df.col.7$col_p_type[df.col.7$col_p_type ==  3] <- 13 #pepper (including green pepper)
df.col.7$col_p_type[df.col.7$col_p_type ==  4] <- 4 #lettuce

# recode SF type
# table(df.col.7$col_sf_type)
df.col.7$col_sf_type[df.col.7$col_sf_type ==  1] <- 10 #fried yam
df.col.7$col_sf_type[df.col.7$col_sf_type ==  2] <- 8 #kenkey
df.col.7$col_sf_type[df.col.7$col_sf_type ==  3] <- 7 #waakye

#indicate extra data
df.col.7$col_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.col.7$dply_num <- 7

#Create unique ID variable
df.col.7$col_UID <- NA
# paste number together
df.col.7$col_UID <- paste0(sprintf("%02d", df.col.7$dply_num),
                           "_",
                           sprintf("%02d", df.col.7$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.7$col_id))

# Create date variable
df.col.7$col_date <- substr(df.col.7$col_start_dt, 1, 10)
df.col.7$col_date <- as.Date(df.col.7$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
df.col.7$col_neighborhood[df.col.7$col_neighborhood == 280] <- 1
df.col.7$col_neighborhood[df.col.7$col_neighborhood == 281] <- 2
df.col.7$col_neighborhood[df.col.7$col_neighborhood == 282] <- 3
df.col.7$col_neighborhood[df.col.7$col_neighborhood == 283] <- 4

# Find columns that are in df but not in df.col
extra7 <- which(colnames(df.col.7) %in% setdiff(colnames(df.col.7), colnames(df.col.0)))

# Remove "extra" columns from df
df.col.7.clean <- df.col.7[, -extra7]

# export extra data
df.col.7.extra <- df.col.7[, extra7]
df.col.7.extra$col_UID <- df.col.7$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.7.clean)
# **********************************************************************************

# **********************************************************************************
#---- 8 MOZAMBIQUE -----
setdiff(colnames(df.col.8), colnames(df.col.0))

##### rename variables #####

colnames(df.col.8)[colnames(df.col.8) == 'sp_type'] <- 'col_sample_type'
colnames(df.col.8)[colnames(df.col.8) == 'neighbor'] <- 'col_neighborhood'
colnames(df.col.8)[colnames(df.col.8) == 'ev_lat'] <- 'X_col_location_latitude'
colnames(df.col.8)[colnames(df.col.8) == 'ev_long'] <- 'X_col_location_longitude'
colnames(df.col.8)[colnames(df.col.8) == 'feces_3m'] <- 'col_pa_prox'
colnames(df.col.8)[colnames(df.col.8) == 'dis_lat'] <- 'col_pa_latrine_distance'

colnames(df.col.8)[colnames(df.col.8) == 'source_bw'] <- 'col_bw_type'
colnames(df.col.8)[colnames(df.col.8) == 'source_other'] <- 'col_bw_type_other'
colnames(df.col.8)[colnames(df.col.8) == 'stored'] <- 'col_bw_container'
colnames(df.col.8)[colnames(df.col.8) == 'covered'] <- 'col_bw_container_lid'

colnames(df.col.8)[colnames(df.col.8) == 'lat_type'] <- 'col_l_toilet_type'
colnames(df.col.8)[colnames(df.col.8) == 'n_stall'] <- 'col_l_toilet_stalls'
colnames(df.col.8)[colnames(df.col.8) == 'Vis_fec'] <- 'col_l_toilet_feces'
colnames(df.col.8)[colnames(df.col.8) == 'HW_stat'] <- 'col_l_toilet_hw'

colnames(df.col.8)[colnames(df.col.8) == 'fd_type'] <- 'col_p_type'
colnames(df.col.8)[colnames(df.col.8) == 'fd_num'] <- 'col_p_num'


# date
df.col.8$ec_sd <- as.Date(df.col.8$ec_sd, "%m/%d/%Y")
df.col.8$ec_isd <- as.Date(df.col.8$ec_isd, "%m/%d/%Y")
df.col.8$ec_ied <- as.Date(df.col.8$ec_ied, "%m/%d/%Y")
df.col.8$ec_st <- format(strptime(df.col.8$ec_st, "%I:%M:%S %p"), "%H:%M:%S") 
df.col.8$ec_ist <- format(strptime(df.col.8$ec_ist, "%I:%M:%S %p"), "%H:%M:%S") 
df.col.8$ec_iet <- format(strptime(df.col.8$ec_iet, "%I:%M:%S %p"), "%H:%M:%S") 

# join date 
df.col.8$col_start_dt <- paste0(df.col.8$ec_sd,
                                "T",
                                df.col.8$ec_st)


##### recode #####
df.col.8$col_sample_type <- case_when(
        df.col.8$col_sample_type == 1 ~ 9, #bathing water
        df.col.8$col_sample_type == 2 ~ 1, #drains
        df.col.8$col_sample_type == 3 ~ 6, #flood water
        df.col.8$col_sample_type == 4 ~ 8, #soil
        df.col.8$col_sample_type == 5 ~ 7, #swabs
        df.col.8$col_sample_type == 6 ~ 2, #produce
        df.col.8$col_sample_type == 7 ~ 3) #drinking water

# produce
df.col.8$col_p_type[df.col.8$col_p_type ==  "Pepino"] <- 2 #cucumber
df.col.8$col_p_type[df.col.8$col_p_type ==  "Alface"] <- 4 #lettuce
df.col.8$col_p_type[df.col.8$col_p_type ==  "Tomate"] <- 8 #tomato

df.col.8$col_p_type <- as.numeric(df.col.8$col_p_type)
table(df.col.8$col_l_toilet_type)


df.col.8$col_l_toilet_type <- case_when(
        df.col.8$col_l_toilet_type == 1 ~ 5,
        df.col.8$col_l_toilet_type == 2 ~ 6,
        df.col.8$col_l_toilet_type == 3 ~ 88,
        df.col.8$col_l_toilet_type == 4 ~ 1,
        df.col.8$col_l_toilet_type == 5 ~ 1,
        df.col.8$col_l_toilet_type == 6 ~ 88)


df.col.8$X_col_location_latitude <- df.col.8$X_col_location_latitude * -1

##### create new variables #####
# Create country or deployment code
df.col.8$dply_num <- 8

# Create unique ID variable
df.col.8$col_UID <- NA

df.col.8$col_id <- 1000:(1000+nrow(df.col.8)-1)
df.col.8$col_id_val <- df.col.8$col_id

# paste number together
df.col.8$col_UID <- paste0(sprintf("%02d", df.col.8$dply_num),
                           "_",
                           sprintf("%02d", df.col.8$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.8$col_id))

# Create date variable
df.col.8$col_date <- df.col.8$ec_sd
df.col.8$col_date <- as.Date(df.col.8$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra8 <- which(colnames(df.col.8) %in% setdiff(colnames(df.col.8), colnames(df.col.0)))

# remove "extra" columns from df4
df.col.8.clean <- df.col.8[, -extra8]

# export extra data
df.col.8.extra <- df.col.8[, extra8]
df.col.8.extra$col_UID <- df.col.8$col_UID

##### merge datasets #####
setdiff(colnames(df.col.8.clean), colnames(df.col.0))


df.col <- bind_rows(df.col, df.col.8.clean)
# **********************************************************************************

# **********************************************************************************
#---- 9 Uganda Kampala -----
# setdiff(colnames(df.col.9), colnames(df.col.0))

##### rename variables #####
colnames(df.col.9)[colnames(df.col.9) == 'col_start'] <- 'col_start_dt'

##### recode #####
# table(df.col.9$col_p_type)
# df.col.9$col_p_type[df.col.9$col_p_type ==  1] <- 1 #cabbage
df.col.9$col_p_type[df.col.9$col_p_type ==  2] <- 8 #tomato

table(df.col.9$col_sample_type)

df.col.9$col_sample_type[df.col.9$col_sample_type ==  9] <- 33 #other drinking water

df.col.9$col_sample_type_alt[df.col.9$col_sample_type ==  33] <- "Spring Water"

# recode SF type
# table(df.col.9$col_sf_type)
df.col.9$col_sf_type[df.col.9$col_sf_type ==  1] <- 11 #rolex
df.col.9$col_sf_type[df.col.9$col_sf_type ==  3] <- 12 #samosas
df.col.9$col_sf_type[df.col.9$col_sf_type ==  88] <- 13#Chapati with beans

#indicate extra data
df.col.9$col_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.col.9$dply_num <- 9

#Create unique ID variable
df.col.9$col_UID <- NA
# paste number together
df.col.9$col_UID <- paste0(sprintf("%02d", df.col.9$dply_num),
                           "_",
                           sprintf("%02d", df.col.9$col_sample_type),
                           "_",
                           sprintf("%04d", df.col.9$col_id))

# Create date variable
df.col.9$col_date <- substr(df.col.9$col_start_dt, 1, 10)
df.col.9$col_date <- as.Date(df.col.9$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
# df.col.9$col_neighborhood[df.col.9$col_neighborhood == 309] <- 1
# df.col.9$col_neighborhood[df.col.9$col_neighborhood == 310] <- 2
# df.col.9$col_neighborhood[df.col.9$col_neighborhood == 311] <- 3
# df.col.9$col_neighborhood[df.col.9$col_neighborhood == 312] <- 4
# df.col.9$col_neighborhood[df.col.9$col_neighborhood == 313] <- 5

# Find columns that are in df but not in df.col
extra9 <- which(colnames(df.col.9) %in% setdiff(colnames(df.col.9), colnames(df.col.0)))

# Remove "extra" columns from df
df.col.9.clean <- df.col.9[, -extra9]

# export extra data
df.col.9.extra <- df.col.9[, extra9]
df.col.9.extra$col_UID <- df.col.9$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.9.clean)
# **********************************************************************************

# **********************************************************************************
#---- 10 ATL -----
# setdiff(colnames(df.col.10), colnames(df.col.0))

##### rename variables #####
colnames(df.col.10)[colnames(df.col.10) == 'start_dt'] <- 'col_start_dt'
colnames(df.col.10)[colnames(df.col.10) == 'end_dt'] <- 'col_end_dt'


##### recode #####
# table(df.col.10$col_sample_type)
# df.col.10$col_p_type[df.col.10$col_p_type ==  1] <- 1 #cabbage
df.col.10$col_sample_type[df.col.10$col_sample_type ==  1] <- 8 #Particulate
df.col.10$col_sample_type[df.col.10$col_sample_type ==  2] <- 2 #Produce
df.col.10$col_sample_type[df.col.10$col_sample_type ==  3] <- 7 #Swab
df.col.10$col_sample_type[df.col.10$col_sample_type ==  4] <- 3 #Drinking Water

df.col.10$col_pa_prox[df.col.10$col_pa_prox ==  3] <- 88 #other


df.col.10$col_p_type[df.col.10$col_p_type ==  "Pepper"] <- 13 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Tomatoes"] <- 8 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Lettuce"] <- 4 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Roma tomato"] <- 8 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Roma tomatoes"] <- 8
df.col.10$col_p_type[df.col.10$col_p_type ==  "Green bell pepper"] <- 13 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Green leaf lettuce"] <- 4 
df.col.10$col_p_type[df.col.10$col_p_type ==  "JalapeÌ±o"] <- 99 
df.col.10$col_p_type[df.col.10$col_p_type ==  "Cucumber "] <- 2 

df.col.10$col_p_type <- as.numeric(as.character(df.col.10$col_p_type))


#indicate extra data
df.col.10$col_extra_data <- 1

df.col.10 <- df.col.10 %>% group_by(col_sample_type) %>% mutate(col_id = row_number(col_sample_type)) %>% ungroup()
df.col.10$col_id <- paste0(sprintf("%01d", df.col.10$col_sample_type),
                           sprintf("%03d", df.col.10$col_id))
df.col.10$col_id <- as.numeric(as.character(df.col.10$col_id))

df.col.10$col_id_val <- df.col.10$col_id

df.col.10$X_col_location_latitude <- as.numeric(df.col.10$X_col_location_latitude)
df.col.10$X_col_location_longitude <- as.numeric(df.col.10$X_col_location_longitude)
df.col.10$X_col_location_altitude <- as.numeric(df.col.10$X_col_location_altitude)
df.col.10$X_col_location_precision <- as.numeric(df.col.10$X_col_location_precision)

df.col.10.1 <- read.csv(paste0(getwd(), "/data/deployments/10_usa-atl/", "atl_flood_col", ".csv"), 
                        sep=",", stringsAsFactors=F, header=T, 
                        na.strings = c("n/a", "NA", "N/A", "---"))


df.col.10 <- bind_rows(df.col.10, df.col.10.1)


##### create new variables #####
# Create country or deployment code
df.col.10$dply_num <- 10




#Create unique ID variable
df.col.10$col_UID <- NA

# paste number together
df.col.10$col_UID <- paste0(sprintf("%02d", df.col.10$dply_num),
                            "_",
                            sprintf("%02d", df.col.10$col_sample_type),
                            "_",
                            sprintf("%04d", df.col.10$col_id))

# Create date variable
df.col.10$col_date <- substr(df.col.10$col_start_dt, 1, 10)
df.col.10$col_date <- as.Date(df.col.10$col_date, format = "%Y-%m-%d")

df.col.10 <- separate(df.col.10, col_location, c("X_col_location_latitude","X_col_location_longitude",
                                               "X_col_location_altitude","X_col_location_precision"), 
                     sep=" ", remove = FALSE)

df.col.10$X_col_location_latitude <- as.numeric(df.col.10$X_col_location_latitude)
df.col.10$X_col_location_longitude <- as.numeric(df.col.10$X_col_location_longitude)
df.col.10$X_col_location_altitude <- as.numeric(df.col.10$X_col_location_altitude)
df.col.10$X_col_location_precision <- as.numeric(df.col.10$X_col_location_precision)



##### deployment-specific info #####

# Find columns that are in df but not in df.col
extra10 <- which(colnames(df.col.10) %in% setdiff(colnames(df.col.10), colnames(df.col.0)))

# Remove "extra" columns from df
df.col.10.clean <- df.col.10[, -extra10]

# export extra data
df.col.10.extra <- df.col.10[, extra10]
df.col.10.extra$col_UID <- df.col.10$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.10.clean)
# **********************************************************************************

# **********************************************************************************
#---- 11 Vellore -----
# setdiff(colnames(df.col.11), colnames(df.col.0))

##### rename variables #####
colnames(df.col.11)[colnames(df.col.11) == 'sample_type'] <- 'col_sample_type'
colnames(df.col.11)[colnames(df.col.11) == 'sample_id'] <- 'col_id'
colnames(df.col.11)[colnames(df.col.11) == 'collection_datetime'] <- 'col_start_dt'
colnames(df.col.11)[colnames(df.col.11) == 'neighborhood'] <- 'col_neighborhood'

colnames(df.col.11)[colnames(df.col.11) == 'GPS_latitude'] <- 'X_col_location_latitude'
colnames(df.col.11)[colnames(df.col.11) == 'GPS_longitude'] <- 'X_col_location_longitude'

##### recode #####
# table(df.col.11$col_sample_type)
# df.col.11$col_sample_type[df.col.11$col_sample_type ==  1] <- 8 #soil/particulate
# df.col.11$col_sample_type[df.col.11$col_sample_type ==  2] <- 7 #public latrine swabs
# df.col.11$col_sample_type[df.col.11$col_sample_type ==  3] <- 2 #Produce
# df.col.11$col_sample_type[df.col.11$col_sample_type ==  4] <- 3 #piped water
# df.col.11$col_sample_type[df.col.11$col_sample_type ==  7] <- 9 #bathing water

df.col.11$col_sample_type <- case_when(df.col.11$col_sample_type ==  1 ~ 8,
                                       df.col.11$col_sample_type ==  2 ~ 7,
                                       df.col.11$col_sample_type ==  3 ~ 2,
                                       df.col.11$col_sample_type ==  4 ~ 3,
                                       df.col.11$col_sample_type ==  7 ~ 9)

df.col.11$col_p_type <- case_when(df.col.11$sample_name ==  "PVG001" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG002" ~ 18, #lady finger/ okra
                                  df.col.11$sample_name ==  "PVG003" ~ 16, #coriander
                                  df.col.11$sample_name ==  "PVG004" ~ 17, #green chilly
                                  df.col.11$sample_name ==  "PVG005" ~ 18, #okra
                                  df.col.11$sample_name ==  "PVG006" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG007" ~ 17, #green chilly
                                  df.col.11$sample_name ==  "PVG008" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG009" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG010" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG101" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG102" ~ 17, #green chilly
                                  df.col.11$sample_name ==  "PVG103" ~ 16, #coriander
                                  df.col.11$sample_name ==  "PVG104" ~ 17, #green chilly
                                  df.col.11$sample_name ==  "PVG105" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG106" ~ 16, #coriander
                                  df.col.11$sample_name ==  "PVG107" ~  8, #tomato
                                  df.col.11$sample_name ==  "PVG108" ~ 18, #lady finger/ okra
                                  df.col.11$sample_name ==  "PVG109" ~ 17, #green chilly
                                  df.col.11$sample_name ==  "PVG110" ~ 16) #coriander

# Create date variable
df.col.11$col_date <- as.Date(df.col.11$col_start_dt, format = "%m/%d/%Y")

df.col.11$col_start_dt <- paste0(as.Date(df.col.11$col_start_dt, "%m/%d/%Y"), "T00:00:00.000+00")


#indicate extra data
df.col.11$col_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.col.11$dply_num <- 11


df.col.11$col_id <- 1000+(df.col.11$col_id)
df.col.11$col_id_val <- df.col.11$col_id

#Create unique ID variable
df.col.11$col_UID <- NA

# paste number together
df.col.11$col_UID <- paste0(sprintf("%02d", df.col.11$dply_num),
                            "_",
                            sprintf("%02d", df.col.11$col_sample_type),
                            "_",
                            sprintf("%04d", df.col.11$col_id))



##### deployment-specific info #####

# Find columns that are in df but not in df.col
extra11 <- which(colnames(df.col.11) %in% setdiff(colnames(df.col.11), colnames(df.col.0)))

# Remove "extra" columns from df
df.col.11.clean <- df.col.11[, -extra11]

# export extra data
df.col.11.extra <- df.col.11[, extra11]
df.col.11.extra$col_UID <- df.col.11$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.11.clean)
# **********************************************************************************


# **********************************************************************************
#---- 12 SENEGAL Dakar -----
setdiff(colnames(df.col.12), colnames(df.col.0))

##### rename variables #####
colnames(df.col.12)[colnames(df.col.12) == 'col_start'] <- 'col_start_dt'

##### recode #####
table(df.col.12$col_sample_type)
# sample type 11 = food swab

# neighborhood
df.col.12$col_neighborhood[df.col.12$col_neighborhood == 409] <- 1
df.col.12$col_neighborhood[df.col.12$col_neighborhood == 410] <- 2
df.col.12$col_neighborhood[df.col.12$col_neighborhood == 411] <- 3
df.col.12$col_neighborhood[df.col.12$col_neighborhood == 412] <- 4
df.col.12$col_neighborhood[df.col.12$col_neighborhood == 413] <- 5

#2001 - 2010 - is ok
df.col.12$col_id[df.col.12$col_neighborhood == 1 & df.col.12$col_sample_type == 10]

#2011 - 2020
df.col.12$col_id[df.col.12$col_neighborhood == 2 & df.col.12$col_sample_type == 10]

df.col.12$col_id[df.col.12$col_id == 2021 & df.col.12$col_neighborhood == 2] <- 2011
df.col.12$col_id[df.col.12$col_id == 2022 & df.col.12$col_neighborhood == 2] <- 2012
df.col.12$col_id[df.col.12$col_id == 2023 & df.col.12$col_neighborhood == 2] <- 2013
df.col.12$col_id[df.col.12$col_id == 2024 & df.col.12$col_neighborhood == 2] <- 2014
df.col.12$col_id[df.col.12$col_id == 2025 & df.col.12$col_neighborhood == 2] <- 2015
df.col.12$col_id[df.col.12$col_id == 2026 & df.col.12$col_neighborhood == 2] <- 2016
df.col.12$col_id[df.col.12$col_id == 2027 & df.col.12$col_neighborhood == 2] <- 2017
df.col.12$col_id[df.col.12$col_id == 2028 & df.col.12$col_neighborhood == 2] <- 2018
df.col.12$col_id[df.col.12$col_id == 2029 & df.col.12$col_neighborhood == 2] <- 2019
df.col.12$col_id[df.col.12$col_id == 2030 & df.col.12$col_neighborhood == 2] <- 2020

#2021 - 2030 - is ok
df.col.12$col_id[df.col.12$col_neighborhood == 3 & df.col.12$col_sample_type == 10]


#2031 - 2040
df.col.12$col_id[df.col.12$col_neighborhood == 4 & df.col.12$col_sample_type == 10]


#2041 - 2050
df.col.12$col_id[df.col.12$col_neighborhood == 5 & df.col.12$col_sample_type == 10]

df.col.12$col_id[df.col.12$col_id == 2051 & df.col.12$col_neighborhood == 5] <- 2041
df.col.12$col_id[df.col.12$col_id == 2052 & df.col.12$col_neighborhood == 5] <- 2042
df.col.12$col_id[df.col.12$col_id == 2053 & df.col.12$col_neighborhood == 5] <- 2043
df.col.12$col_id[df.col.12$col_id == 2054 & df.col.12$col_neighborhood == 5] <- 2044
df.col.12$col_id[df.col.12$col_id == 2055 & df.col.12$col_neighborhood == 5] <- 2045
df.col.12$col_id[df.col.12$col_id == 2056 & df.col.12$col_neighborhood == 5] <- 2046
df.col.12$col_id[df.col.12$col_id == 2057 & df.col.12$col_neighborhood == 5] <- 2047
df.col.12$col_id[df.col.12$col_id == 2058 & df.col.12$col_neighborhood == 5] <- 2048
df.col.12$col_id[df.col.12$col_id == 2059 & df.col.12$col_neighborhood == 5] <- 2049
df.col.12$col_id[df.col.12$col_id == 2060 & df.col.12$col_neighborhood == 5] <- 2050


df.col.12$col_id[df.col.12$col_start_dt == "2019-11-14T11:13:00.000Z"] <- 3015

df.col.12$col_id_val <- df.col.12$col_id


# duplicated(df.col.12$col_id)
# # 3005 is duplicated
# n_occur <- data.frame(table(df.col.12$col_id))

# fix produce and streetfood entries
table(df.col.12$col_p_type_other)
table(df.col.12$col_p_type)

# 1 Salade -- 7
# 2 Tomate -- 8
# 3 Concombre -- 2
# 4 Poivron vert -- 13
# 5 Piment rouge -- 17
# 88 Carotte -- 12

df.col.12$col_p_type <- case_when(df.col.12$col_p_type ==  1 ~  7, #salad
                                  df.col.12$col_p_type ==  2 ~  8,
                                  df.col.12$col_p_type ==  3 ~  2,
                                  df.col.12$col_p_type ==  4 ~  13,
                                  df.col.12$col_p_type ==  5 ~  17,
                                  df.col.12$col_p_type ==  88 ~  12)

df.col.12$col_p_type_other[df.col.12$col_p_type_other == "Carotte "] <- "carrot"

table(df.col.12$col_sf_type_other)
table(df.col.12$col_sf_type)

# Ndambé
# Petit pois
# Pain thon
# Autre

df.col.12$col_sf_type <- case_when(df.col.12$col_sf_type ==  1 ~  15, #stew
                                   df.col.12$col_sf_type ==  2 ~  16, #peas
                                   df.col.12$col_sf_type ==  3 ~  17, # tuna sandwich
                                   df.col.12$col_sf_type ==  88 ~  18) # pasta

table(df.col.12$col_sf_type)

# recode col_sample_type:
df.col.12$col_sample_type[df.col.12$col_sample_type ==  11] <- 99 #food swabs
table(df.col.12$col_sample_type)

# filter out food swabs
df.col.12.swabs <- df.col.12 %>% filter(col_sample_type == 99)
df.col.12 <- df.col.12 %>% filter(col_sample_type != 99)

##### create new variables #####
#indicate extra data
df.col.12$col_extra_data <- 1

# Create country or deployment code
df.col.12$dply_num <- 12
df.col.12.swabs$dply_num <- 12

#Create unique ID variable
df.col.12$col_UID <- NA

# paste number together
df.col.12$col_UID <- paste0(sprintf("%02d", df.col.12$dply_num),
                            "_",
                            sprintf("%02d", df.col.12$col_sample_type),
                            "_",
                            sprintf("%04d", df.col.12$col_id))

df.col.12.swabs$col_UID <- paste0(sprintf("%02d", df.col.12.swabs$dply_num),
                                  "_",
                                  sprintf("%02d", df.col.12.swabs$col_sample_type),
                                  "_",
                                  sprintf("%04d", df.col.12.swabs$col_id))
# Create date variable
df.col.12$col_date <- substr(df.col.12$col_start_dt, 1, 10)
df.col.12$col_date <- as.Date(df.col.12$col_date, format = "%Y-%m-%d")

df.col.12.swabs$col_date <- substr(df.col.12.swabs$col_start_dt, 1, 10)
df.col.12.swabs$col_date <- as.Date(df.col.12.swabs$col_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# Find columns that are in df3 but not in df.col
extra12 <- which(colnames(df.col.12) %in% setdiff(colnames(df.col.12), colnames(df.col.0)))

# Remove "extra" columns from df2
df.col.12.clean <- df.col.12[, -extra12]

# export extra data
df.col.12.extra <- df.col.12[, extra12]
df.col.12.extra$col_UID <- df.col.12$col_UID


##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.12.clean)


# **********************************************************************************
#---- 13 ZAMBIA 2 -----
setdiff(colnames(df.col.13), colnames(df.col.0))

##### rename variables #####
colnames(df.col.13)[colnames(df.col.13) == 'col_start'] <- 'col_start_dt'

##### recode #####
table(df.col.13$col_sample_type)

# neighborhood
df.col.13$col_neighborhood[df.col.13$col_neighborhood == 381] <- 1
df.col.13$col_neighborhood[df.col.13$col_neighborhood == 382] <- 2
df.col.13$col_neighborhood[df.col.13$col_neighborhood == 380] <- 3

# fix produce and streetfood entries
table(df.col.13$col_p_type_other)

df.col.13$col_p_type_other[df.col.13$col_p_type_other == "Fritters"] <- "Tomato"
df.col.13$col_p_type_other[df.col.13$col_p_type_other == "Tonato"] <- "Tomato"

df.col.13$col_p_type <- ifelse(df.col.13$col_p_type_other == "Tomato", 8, NA)
df.col.13$col_p_type <- ifelse(df.col.13$col_p_type_other == "Watermelon", 19, df.col.13$col_p_type)
df.col.13$col_p_type <- ifelse(df.col.13$col_p_type_other == "Cucumber", 2, df.col.13$col_p_type)
table(df.col.13$col_p_type)


table(df.col.13$col_sf_type_other)
# SF1=Fritters
# SF2=Samoosa
# SF3=donuts/cassava/japati (egg wrap)

df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "3803"] <- "Cassava"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Pie"] <- "Egg Wrap"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Scones"] <- "Egg Wrap"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Samoosa"] <- "Samoosas"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Fritter"] <- "Fritters"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Doughnut"] <- "Egg Wrap"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Roasted Cassava"] <- "Egg Wrap"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Chikanda"] <- "Egg Wrap"
df.col.13$col_sf_type_other[df.col.13$col_sf_type_other == "Japati"] <- "Egg Wrap"

df.col.13$col_sf_type <- ifelse(df.col.13$col_sf_type_other == "Egg Wrap", 14, NA)
df.col.13$col_sf_type <- ifelse(df.col.13$col_sf_type_other == "Samoosas", 12, df.col.13$col_sf_type)
df.col.13$col_sf_type <- ifelse(df.col.13$col_sf_type_other == "Fritters", 5, df.col.13$col_sf_type)

table(df.col.13$col_sf_type)

# recode col_sample_type:
df.col.13$col_sample_type[df.col.13$col_sample_type ==  11] <- 33 #other drinking water
table(df.col.13$col_sample_type)

# Shallow Well
df.col.13$col_odw_type[df.col.13$col_sample_type ==  9] <- 2

# alternative sample types (e.g. bottled water -> drinking water)
df.col.13$col_sample_type_alt <- case_when(df.col.13$col_odw_type ==  1 ~ "Borehole",
                                           df.col.13$col_odw_type ==  2 ~ "Shallow Well")

df.col.13$col_sample_type[df.col.13$col_sample_type ==  9] <- 33 #other drinking water


##### create new variables #####
df.col.13$col_extra_data <- 1

# Create country or deployment code
df.col.13$dply_num <- 13

#Create unique ID variable
df.col.13$col_UID <- NA

# paste number together
df.col.13$col_UID <- paste0(sprintf("%02d", df.col.13$dply_num),
                            "_",
                            sprintf("%02d", df.col.13$col_sample_type),
                            "_",
                            sprintf("%04d", df.col.13$col_id))

# Create date variable
df.col.13$col_date <- substr(df.col.13$col_start_dt, 1, 10)
df.col.13$col_date <- as.Date(df.col.13$col_date, format = "%Y-%m-%d")


##### deployment-specific info #####
# Find columns that are in df3 but not in df.col
extra13 <- which(colnames(df.col.13) %in% setdiff(colnames(df.col.13), colnames(df.col.0)))

# Remove "extra" columns from df2
df.col.13.clean <- df.col.13[, -extra13]

# export extra data
df.col.13.extra <- df.col.13[, extra13]
df.col.13.extra$col_UID <- df.col.13$col_UID

##### merge datasets #####
# merge clean datasets
df.col <- bind_rows(df.col, df.col.13.clean)







# **********************************************************************************
# add unique neighborhood numbers (over all projects)
df.col$neighb_UID <- paste0(sprintf("%01d", df.col$dply_num),
                            sprintf("%02d", df.col$col_neighborhood))

df.col$neighb_UID <- as.numeric(df.col$neighb_UID)

df.col <- left_join(df.col, meta_dply[,c(1,4)], by = c("dply_num" = "id"))


df.col$col_sample_type_alt[df.col$col_sample_type == 3] <- "Drinking Water"


# table(df.col$col_sample_type_alt)

table(df.col$col_sample_type)



# **********************************************************************************
##### 99 WRITE DATA #####
write.csv(df.col, paste0(getwd(), "/data/", "col_merged_", Sys.Date(), ".csv"), row.names = F, na="")

write.csv(df.col.1.extra, paste0(getwd(), "/data/extra_data/", "col_extra_01.csv"), row.names = F, na="")
write.csv(df.col.2.extra, paste0(getwd(), "/data/extra_data/", "col_extra_02.csv"), row.names = F, na="")
write.csv(df.col.3.extra, paste0(getwd(), "/data/extra_data/", "col_extra_03.csv"), row.names = F, na="")
write.csv(df.col.4.extra, paste0(getwd(), "/data/extra_data/", "col_extra_04.csv"), row.names = F, na="")
write.csv(df.col.5.extra, paste0(getwd(), "/data/extra_data/", "col_extra_05.csv"), row.names = F, na="")
write.csv(df.col.8.extra, paste0(getwd(), "/data/extra_data/", "col_extra_08.csv"), row.names = F, na="")
write.csv(df.col.9.extra, paste0(getwd(), "/data/extra_data/", "col_extra_09.csv"), row.names = F, na="")
write.csv(df.col.11.extra, paste0(getwd(), "/data/extra_data/", "col_extra_11.csv"), row.names = F, na="")
write.csv(df.col.12.swabs, paste0(getwd(), "/data/extra_data/", "col_extra_12_swabs.csv"), row.names = F, na="")
write.csv(df.col.13.extra, paste0(getwd(), "/data/extra_data/", "col_extra_13.csv"), row.names = F, na="")


