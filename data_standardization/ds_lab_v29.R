# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# Form: laboratory testing (lab)
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
df.lab.0 <- read.table(paste0(getwd(), "/data/deployments/00_master/", "LAB_09_19_17_WM - xml - 2017-10-31-18-47", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)

# read cambodia form
df.lab.1 <- read.table(paste0(getwd(), "/data/deployments/01_cambodia/", "SP CAMBO DATASET - lab", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)

# read bangladesh form
df.lab.2 <- read.table(paste0(getwd(), "/data/deployments/02_bangladesh/", "lab_data_011018_wm", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA"))

# read ghana form
df.lab.3 <- read.csv(paste0(getwd(), "/data/deployments/03_ghana/", "ghana lab", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

# read zambia form
df.lab.4 <- read.csv(paste0(getwd(), "/data/deployments/04_zambia/", "lab_lusaka", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.lab.5 <- read.csv(paste0(getwd(), "/data/deployments/05_mozambique/", "compiled_env1", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana accra form
df.lab.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "lab_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.lab.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "lab_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.lab.8 <- read.csv(paste0(getwd(), "/data/deployments/08_mozambique2/", "compiled_env_2016", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.lab.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/", "lab_clean", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read atl form
df.lab.10 <- read.csv(paste0(getwd(), "/data/deployments/10_usa-atl/", "Atlanta_Lab_FINAL0", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))

# read vellore form
df.lab.11 <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/", "Samples", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))

# read senegal form
df.lab.12 <- read.csv(paste0(getwd(), "/data/deployments/12_senegal/", 
                             "Membrane_Filtration_Lab-xlsform_waspa_final_2020_02_05_17_51_34", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))

# read lusaka2 form
df.lab.13 <- read.csv(paste0(getwd(), "/data/deployments/13_zambia2/", "IDEXX_Lab-xlsform_lusaka2019_final_2019_11_04_18_49_17", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))
#*************************************************************************************************

#---- 0 Master -----
##### create new variables #####
df.lab.0$lab_extra_data <- NA
df.lab.0$dply_num <- NA

df.lab.0$lab_UID <- NA
df.lab.0$lab_UID <- as.character(df.lab.0$lab_UID)

# df.lab.0$Cube.or.Hygiene.Ice <- NA 

df.lab.0$lab_date <- NA
df.lab.0$lab_date <- as.Date(df.lab.0$lab_date, format = "%Y-%m-%d")

# df.lab.0$lab_1_ecoli <- NA
# df.lab.0$lab_2_ecoli <- NA
# df.lab.0$lab_3_ecoli <- NA

##### merge datasets #####
# delete rows and override the original table
df.lab.0 <- df.lab.0[-c(1:1),]

df.lab.0$subscriberid <- as.numeric(df.lab.0$subscriberid)
df.lab.0$simserial <- as.numeric(df.lab.0$simserial)
df.lab.0$deviceid <- as.numeric(df.lab.0$deviceid)
df.lab.0$lab_id <- as.numeric(df.lab.0$lab_id) 
df.lab.0$lab_id_val <- as.numeric(df.lab.0$lab_id_val)
# **********************************************************************************

# **********************************************************************************
#---- 1 CAMBODIA -----
##### rename variables #####
colnames(df.lab.1)[colnames(df.lab.1) == 'start_dt'] <- 'lab_start_dt'
colnames(df.lab.1)[colnames(df.lab.1) == 'end_dt'] <- 'lab_end_dt'

colnames(df.lab.1)[colnames(df.lab.1) == 'lab_1_ecoli_reading'] <- 'lab_1_ecoli_reading_membrane'
colnames(df.lab.1)[colnames(df.lab.1) == 'lab_1_ecoli'] <- 'lab_1_ecoli_membrane'

colnames(df.lab.1)[colnames(df.lab.1) == 'lab_2_ecoli_reading'] <- 'lab_2_ecoli_reading_membrane'
colnames(df.lab.1)[colnames(df.lab.1) == 'lab_2_ecoli'] <- 'lab_2_ecoli_membrane'

colnames(df.lab.1)[colnames(df.lab.1) == 'lab_3_ecoli_reading'] <- 'lab_3_ecoli_reading_membrane'
colnames(df.lab.1)[colnames(df.lab.1) == 'lab_3_ecoli'] <- 'lab_3_ecoli_membrane'

##### recode #####
# Create another variable to indicate when we are collapsing categories and extra data is available (code 0/1)
df.lab.1$lab_extra_data <- NA

df.lab.1$lab_extra_data <- ifelse(df.lab.1$lab_sample_type == 20, 1,
                                  ifelse(df.lab.1$lab_sample_type == 21, 1,
                                         ifelse(df.lab.1$lab_sample_type == 22, 1,
                                                ifelse(df.lab.1$lab_sample_type == 23, 1, 0))))

# Recode lab_sample_type
# Mark as having extra data (above)
# Ice, Drinking Water: Water Supply, Drinking Water: Bottled Water, and Drinking Water: Well Water
# reclassified as Drinking Water
df.lab.1$lab_sample_type[df.lab.1$lab_sample_type ==  20] <- 33 #odw
df.lab.1$lab_sample_type[df.lab.1$lab_sample_type ==  21] <- 3 #dw
df.lab.1$lab_sample_type[df.lab.1$lab_sample_type ==  22] <- 33 #odw
df.lab.1$lab_sample_type[df.lab.1$lab_sample_type ==  23] <- 33 #odw

##### create new variables #####
# Create country or deployment code
df.lab.1$dply_num <- 1


# lab analysis
df.lab.1$lab_analysis <- 2

# Create date variable
df.lab.1$lab_date <- NA
df.lab.1$lab_date <- substr(df.lab.1$lab_processing, 1, 10)
df.lab.1$lab_date <- as.Date(df.lab.1$lab_date, format = "%Y-%m-%d")

# do not change unique id here, -> already created UID variable.
# Note: there are some samples that do not have lab data available
df.lab.1$lab_id <- (1000:(1000+nrow(df.lab.1)-1))
df.lab.1$lab_id_val <- df.lab.1$lab_id

# Create unique ID variable
df.lab.1$lab_UID <- NA

# paste number together
df.lab.1$lab_UID <- paste0(sprintf("%02d", df.lab.1$dply_num),
                           "_",
                           sprintf("%02d", df.lab.1$lab_sample_type),
                           "_",
                           sprintf("%04d", as.numeric(df.lab.1$lab_id)))

##### deployment-specific info #####
# find columns that are in df1 but not in df0
extra1 <- which(colnames(df.lab.1) %in% setdiff(colnames(df.lab.1), colnames(df.lab.0)))

# remove "extra" columns from df1
df.lab.1.clean <- df.lab.1[, -extra1]

# export extra data
df.lab.1.extra <- df.lab.1[, extra1]
df.lab.1.extra$lab_UID <- df.lab.1$lab_UID

##### merge datasets #####
# check that dataframes have same dimensions
setdiff(colnames(df.lab.1.clean), colnames(df.lab.0))

# merge clean datasets
df.lab <- bind_rows(df.lab.0, df.lab.1.clean)
# **********************************************************************************

# **********************************************************************************
#---- 2 BANGLADESH -----
##### rename variables #####
colnames(df.lab.2)[colnames(df.lab.2) == 'start_dt'] <- 'lab_start_dt'
colnames(df.lab.2)[colnames(df.lab.2) == 'end_dt'] <- 'lab_end_dt'

colnames(df.lab.2)[colnames(df.lab.2) == 'lab_1_ecoli_reading'] <- 'lab_1_ecoli_reading_idexx'
colnames(df.lab.2)[colnames(df.lab.2) == 'lab_1_ecoli_big'] <- 'lab_1_ecoli_big_idexx'
colnames(df.lab.2)[colnames(df.lab.2) == 'lab_1_ecoli_small'] <- 'lab_1_ecoli_small_idexx'

colnames(df.lab.2)[colnames(df.lab.2) == 'lab_2_ecoli_reading'] <- 'lab_2_ecoli_reading_idexx'
colnames(df.lab.2)[colnames(df.lab.2) == 'lab_2_ecoli_big'] <- 'lab_2_ecoli_big_idexx'

colnames(df.lab.2)[colnames(df.lab.2) == 'lab_2_ecoli_small'] <- 'lab_2_ecoli_small_idexx'

colnames(df.lab.2)[colnames(df.lab.2) == 'lab_3_ecoli_reading'] <- 'lab_3_ecoli_reading_idexx'
colnames(df.lab.2)[colnames(df.lab.2) == 'lab_3_ecoli_big'] <- 'lab_3_ecoli_big_idexx'
colnames(df.lab.2)[colnames(df.lab.2) == 'lab_3_ecoli_small'] <- 'lab_3_ecoli_small_idexx'
##### recode #####

# Create another variable to indicate when we are collapsing categories and extra data is available (code 0/1)
df.lab.2$lab_extra_data <- NA

df.lab.2$lab_extra_data <- ifelse(df.lab.2$lab_sample_type == 10, 1,
                                  ifelse(df.lab.2$lab_sample_type == 3, 1,
                                         ifelse(df.lab.2$lab_1_ecoli_reading_idexx == 1, 1,
                                                ifelse(df.lab.2$lab_1_ecoli_reading_idexx == 2, 1, 
                                                       ifelse(df.lab.2$lab_2_ecoli_reading_idexx == 1, 1,
                                                              ifelse(df.lab.2$lab_2_ecoli_reading_idexx == 2, 1, 
                                                                     ifelse(df.lab.2$lab_3_ecoli_reading_idexx == 1, 1,
                                                                            ifelse(df.lab.2$lab_3_ecoli_reading_idexx == 2, 1, 0))))))))

# Recode lab_sample_type
# Mark as having extra data (above)
# Other Drinking Water categorized as Drinking Water
df.lab.2$lab_sample_type[df.lab.2$lab_sample_type ==  10] <- 33 #odw

# Recode Street Food
df.lab.2$lab_sample_type[df.lab.2$lab_sample_type ==  11] <- 10

# Recode lab_1_ecoli_reading_idexx
# Mark as having extra data (above)
df.lab.2$lab_1_ecoli_reading_idexx <- case_when(
        df.lab.2$lab_1_ecoli_reading_idexx == 1 ~ 2,
        df.lab.2$lab_1_ecoli_reading_idexx == 2 ~ 1,
        df.lab.2$lab_1_ecoli_reading_idexx == 3 ~ 2)

# df.lab.2$lab_1_ecoli_reading_idexx[df.lab.2$lab_1_ecoli_reading_idexx ==  1] <- 99
# df.lab.2$lab_1_ecoli_reading_idexx[df.lab.2$lab_1_ecoli_reading_idexx ==  2] <- 1
# df.lab.2$lab_1_ecoli_reading_idexx[df.lab.2$lab_1_ecoli_reading_idexx ==  3] <- 2
# df.lab.2$lab_1_ecoli_reading_idexx[df.lab.2$lab_1_ecoli_reading_idexx ==  99] <- 2

# Recode lab_2_ecoli_reading_idexx
# Mark as having extra data (above)
df.lab.2$lab_2_ecoli_reading_idexx <- case_when(
        df.lab.2$lab_2_ecoli_reading_idexx == 1 ~ 2,
        df.lab.2$lab_2_ecoli_reading_idexx == 2 ~ 1,
        df.lab.2$lab_2_ecoli_reading_idexx == 3 ~ 2)

# df.lab.2$lab_2_ecoli_reading_idexx[df.lab.2$lab_2_ecoli_reading_idexx ==  2] <- 1
# df.lab.2$lab_2_ecoli_reading_idexx[df.lab.2$lab_2_ecoli_reading_idexx ==  3] <- 2

# Recode lab_3_ecoli_reading_idexx
# Mark as having extra data (above)
df.lab.2$lab_3_ecoli_reading_idexx <- case_when(
        df.lab.2$lab_3_ecoli_reading_idexx == 1 ~ 2,
        df.lab.2$lab_3_ecoli_reading_idexx == 2 ~ 1,
        df.lab.2$lab_3_ecoli_reading_idexx == 3 ~ 2)

# df.lab.2$lab_3_ecoli_reading_idexx[df.lab.2$lab_3_ecoli_reading_idexx ==  2] <- 1
# df.lab.2$lab_3_ecoli_reading_idexx[df.lab.2$lab_3_ecoli_reading_idexx ==  3] <- 2

##### create new variables #####
# Create country or deployment code
df.lab.2$dply_num <- 2

# Create unique ID variable
df.lab.2$lab_UID <- NA

# paste number together
df.lab.2$lab_UID <- paste0(sprintf("%02d", df.lab.2$dply_num),
                           "_",
                           sprintf("%02d", df.lab.2$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.2$lab_id))

# lab analysis
df.lab.2$lab_analysis <- 1

# Create date variable
df.lab.2$lab_date <- NA
df.lab.2$lab_date <- substr(df.lab.2$lab_processing, 1, 10)
df.lab.2$lab_date <- as.Date(df.lab.2$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df2 but not in df0
extra2 <- which(colnames(df.lab.2) %in% setdiff(colnames(df.lab.2), colnames(df.lab.0)))

# remove "extra" columns from df1
df.lab.2.clean <- df.lab.2[, -extra2]

# export extra data
df.lab.2.extra <- df.lab.2[, extra2]
df.lab.2.extra$lab_UID <- df.lab.2$lab_UID

##### merge datasets #####
# check that dataframes have same dimensions
setdiff(colnames(df.lab.2.clean), colnames(df.lab.0))

# merge clean datasets
df.lab <- bind_rows(df.lab, df.lab.2.clean)
# **********************************************************************************

# **********************************************************************************
#---- 3 GHANA -----
##### rename variables #####
colnames(df.lab.3)[colnames(df.lab.3) == 'started_time'] <- 'lab_start_dt'
colnames(df.lab.3)[colnames(df.lab.3) == 'completed_time'] <- 'lab_end_dt'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.sample_type'] <- 'lab_sample_type'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.sampleid'] <- 'lab_id'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.sampleid_validate'] <- 'lab_id_val'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.produce_weight'] <- 'lab_p_weight'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_dil1'] <- 'lab_1_dil_tested'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_ecnt1'] <- 'lab_1_ecoli_membrane'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_dil2'] <- 'lab_2_dil_tested'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_ecnt2'] <- 'lab_2_ecoli_membrane'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.third_dilution'] <- 'lab_3_dilution_performed'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_dil3'] <- 'lab_3_dil_tested'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_ecnt3'] <- 'lab_3_ecoli_membrane'

colnames(df.lab.3)[colnames(df.lab.3) == 'form.ec_notes'] <- 'lab_notes'
colnames(df.lab.3)[colnames(df.lab.3) == 'form.operator'] <- 'lab_enum'

# join date when processed
df.lab.3$lab_processing <- paste0(df.lab.3$form.ec_sd,
                                  "T",
                                  df.lab.3$form.ec_st)

# join date when placed in incubator
df.lab.3$lab_incubator_placed <- paste0(df.lab.3$form.ec_isd,
                                        "T",
                                        df.lab.3$form.ec_ist)

# join date when removed from incubator
df.lab.3$lab_incubator_removed <- paste0(df.lab.3$form.ec_ied,
                                         "T",
                                         df.lab.3$form.ec_iet)
##### recode #####
# Create another variable to indicate when we are collapsing categories and extra data is available (code 0/1)
df.lab.3$lab_extra_data <- NA

# Recode lab_1_dil_tested
table(df.lab.3$lab_1_dil_tested)

# open drain (1)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_1_dil_tested == 0.1] <- 6 
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_1_dil_tested == 0.01] <- 5
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_1_dil_tested == 0.001] <- 4
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_1_dil_tested == 0.0001] <- 3
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_1_dil_tested == 0.00001] <- 2

# produce (2)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_1_dil_tested == 10] <- 7
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_1_dil_tested == 1] <- 6

# drinking water (3)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_1_dil_tested == 100] <- 7
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_1_dil_tested == 10] <- 7

# ocean water (4)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_1_dil_tested == 10] <- 6
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_1_dil_tested == 1] <- 5

# surface water (5)
# no samples

# floodwater (6)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_1_dil_tested == 10] <- 6
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_1_dil_tested == 1] <- 5

# swabs (7)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_1_dil_tested == 10] <- 7
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_1_dil_tested == 1] <- 6

# particulate (8)
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_1_dil_tested == 10] <- 7
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_1_dil_tested == 1] <- 6
df.lab.3$lab_1_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_1_dil_tested == 0.1] <- 5 

# bathing water (9)
# no samples

# Recode lab_2_dil_tested
table(df.lab.3$lab_2_dil_tested)

# open drain (1)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.1] <- 6 
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.01] <- 5
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.001] <- 4
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.0001] <- 3
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.00001] <- 2
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.000001] <- 1
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_2_dil_tested == 0.0000001] <- NA

# produce (2)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_2_dil_tested == 10] <- 7
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_2_dil_tested == 1] <- 6

# drinking water (3)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_2_dil_tested == 100] <- 7
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_2_dil_tested == 10] <- 7

# ocean water (4)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_2_dil_tested == 10] <- 6
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_2_dil_tested == 1] <- 5

# surface water (5)
# no samples

# floodwater (6)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_2_dil_tested == 10] <- 6
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_2_dil_tested == 1] <- 5

# swabs (7)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_2_dil_tested == 10] <- 7
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_2_dil_tested == 1] <- 6

# particulate (8)
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_2_dil_tested == 10] <- 7
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_2_dil_tested == 1] <- 6
df.lab.3$lab_2_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_2_dil_tested == 0.1] <- 5 

# bathing water (9)
# no samples

# Recode lab_3_dil_tested
table(df.lab.3$lab_3_dil_tested) #CHECK

# open drain (1)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.1] <- 6 
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.01] <- 5
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.001] <- 4
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.0001] <- 3
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.00001] <- 2
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.000001] <- 1
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 1 & df.lab.3$lab_3_dil_tested == 0.0000001] <- NA

# produce (2)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_3_dil_tested == 10] <- 7
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 2 & df.lab.3$lab_3_dil_tested == 1] <- 6

# drinking water (3)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_3_dil_tested == 100] <- 7
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 3 & df.lab.3$lab_3_dil_tested == 10] <- 7

# ocean water (4)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_3_dil_tested == 10] <- 6
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 4 & df.lab.3$lab_3_dil_tested == 1] <- 5

# surface water (5)
# no samples

# floodwater (6)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_3_dil_tested == 10] <- 6
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 6 & df.lab.3$lab_3_dil_tested == 1] <- 5

# swabs (7)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_3_dil_tested == 10] <- 7
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 7 & df.lab.3$lab_3_dil_tested == 1] <- 6

# particulate (8)
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_3_dil_tested == 10] <- 7
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_3_dil_tested == 1] <- 6
df.lab.3$lab_3_dil_tested[df.lab.3$lab_sample_type == 8 & df.lab.3$lab_3_dil_tested == 0.1] <- 5 

# bathing water (9)
# no samples


# Recode lab_3_dilution_performed
df.lab.3$lab_3_dilution_performed[df.lab.3$lab_3_dilution_performed ==  "Yes"] <- 1
df.lab.3$lab_3_dilution_performed[df.lab.3$lab_3_dilution_performed ==  "No"] <- 0

df.lab.3$lab_3_dilution_performed <- as.numeric(df.lab.3$lab_3_dilution_performed)

#volume
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 1 ] <- 1
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 2 ] <- 10
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 3 ] <- 100
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 4 ] <- 1 # not 100% sure about this volume
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 6 ] <- 1
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 7 ] <- 10
df.lab.3$lab_1_volume[df.lab.3$lab_sample_type == 8 ] <- 1

df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 1 ] <- 1
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 2 ] <- 1
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 3 ] <- 10
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 4 ] <- 1 
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 6 ] <- 1
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 7 ] <- 1
df.lab.3$lab_2_volume[df.lab.3$lab_sample_type == 8 ] <- 1

df.lab.3$lab_3_volume <- case_when(df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 1 ~ 1,
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 2 ~ 1,
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 3 ~ as.numeric(NA),
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 4 ~ 1,
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 6 ~ 1,
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 7 ~ as.numeric(NA),
                                   df.lab.3$lab_3_dilution_performed == 1 | df.lab.3$lab_sample_type == 8 ~ 1
                                   )

# valid reading
df.lab.3$lab_1_ecoli_reading_membrane <- case_when(df.lab.3$lab_1_ecoli_membrane == 999 ~ 1,
                                                   df.lab.3$lab_1_ecoli_membrane == 998 ~ 2,
                                                   df.lab.3$lab_1_ecoli_membrane < 998 ~ 3)

df.lab.3$lab_2_ecoli_reading_membrane <- case_when(df.lab.3$lab_2_ecoli_membrane == 999 ~ 1,
                                                   df.lab.3$lab_2_ecoli_membrane == 998 ~ 2,
                                                   df.lab.3$lab_2_ecoli_membrane < 998 ~ 3)

df.lab.3$lab_3_ecoli_reading_membrane <- case_when(df.lab.3$lab_3_ecoli_membrane == 999 ~ 1,
                                                   df.lab.3$lab_3_ecoli_membrane == 998 ~ 2,
                                                   df.lab.3$lab_3_ecoli_membrane < 998 ~ 3)
##### create new variables #####
# Create country or deployment code
df.lab.3$dply_num <- 3

# Create unique ID variable
df.lab.3$lab_UID <- NA

# paste number together
df.lab.3$lab_UID <- paste0(sprintf("%02d", df.lab.3$dply_num),
                           "_",
                           sprintf("%02d", df.lab.3$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.3$lab_id))

# lab analysis
df.lab.3$lab_analysis <- 2

# Create date variable
df.lab.3$lab_date <- NA
df.lab.3$lab_date <- substr(df.lab.3$lab_processing, 1, 10)
df.lab.3$lab_date <- as.Date(df.lab.3$lab_date, format = "%Y-%m-%d")

# delete duplicates
df.lab.3 <- df.lab.3[!duplicated(df.lab.3$lab_id), ]

# delete samples with no matching sample data 
del <- c(517, 759, 456, 842, 455, 624, 460, 454, 316, 459, 452, 739, 461, 453, 458, 457)
df.lab.3 <- df.lab.3 %>% 
        filter(!lab_id %in% del)
##### deployment-specific info #####
# find columns that are in df3 but not in df0
extra3 <- which(colnames(df.lab.3) %in% setdiff(colnames(df.lab.3), colnames(df.lab.0)))

# remove "extra" columns from df1
df.lab.3.clean <- df.lab.3[, -extra3]

# export extra data
df.lab.3.extra <- df.lab.3[, extra3]
df.lab.3.extra$lab_UID <- df.lab.3$lab_UID

##### merge datasets #####
# check that dataframes have same dimensions
setdiff(colnames(df.lab.3.clean), colnames(df.lab.0))

# merge clean datasets
df.lab <- bind_rows(df.lab, df.lab.3.clean)



#---- 4 ZAMBIA -----
setdiff(colnames(df.lab.4), colnames(df.lab.0))

##### rename variables #####
df.lab.4$col_start_dt <- NULL
df.lab.4$col_end_dt <- NULL

colnames(df.lab.4)[colnames(df.lab.4) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.4)[colnames(df.lab.4) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.4)[colnames(df.lab.4) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.4)[colnames(df.lab.4) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.4)[colnames(df.lab.4) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.4)[colnames(df.lab.4) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####
df.lab.4$lab_sample_type[df.lab.4$lab_sample_type ==  11] <- 33 #other drinking water


##### create new variables #####
# Create country or deployment code
df.lab.4$dply_num <- 4

# Create unique ID variable
df.lab.4$lab_UID <- NA

# paste number together
df.lab.4$lab_UID <- paste0(sprintf("%02d", df.lab.4$dply_num),
                           "_",
                           sprintf("%02d", df.lab.4$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.4$lab_id))

# Create date variable
df.lab.4$lab_date <- NA
df.lab.4$lab_date <- substr(df.lab.4$lab_processing, 1, 10)
df.lab.4$lab_date <- as.Date(df.lab.4$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra4 <- which(colnames(df.lab.4) %in% setdiff(colnames(df.lab.4), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.4.clean <- df.lab.4[, -extra4]

# export extra data
df.lab.4.extra <- df.lab.4[, extra4]
df.lab.4.extra$lab_UID <- df.lab.4$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.4.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.4.clean)
# **********************************************************************************

# **********************************************************************************
#---- 5 MOZAMBIQUE -----
setdiff(colnames(df.lab.5), colnames(df.lab.0))

##### rename variables #####

colnames(df.lab.5)[colnames(df.lab.5) == 'sp_type'] <- 'lab_sample_type'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_dil1'] <- 'lab_1_dil_tested'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_dil2'] <- 'lab_2_dil_tested'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_dil3'] <- 'lab_3_dil_tested'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_ecnt1'] <- 'lab_1_ecoli_membrane'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_ecnt2'] <- 'lab_2_ecoli_membrane'
colnames(df.lab.5)[colnames(df.lab.5) == 'ec_ecnt3'] <- 'lab_3_ecoli_membrane'


# date
df.lab.5$ec_sd <- as.Date(df.lab.5$ec_sd, "%m/%d/%Y")
df.lab.5$ec_isd <- as.Date(df.lab.5$ec_isd, "%m/%d/%Y")
df.lab.5$ec_ied <- as.Date(df.lab.5$ec_ied, "%m/%d/%Y")
df.lab.5$ec_st <- format(strptime(df.lab.5$ec_st, "%I:%M:%S %p"), "%H:%M:%S") 
df.lab.5$ec_ist <- format(strptime(df.lab.5$ec_ist, "%I:%M:%S %p"), "%H:%M:%S") 
df.lab.5$ec_iet <- format(strptime(df.lab.5$ec_iet, "%I:%M:%S %p"), "%H:%M:%S") 

# join date when processed
df.lab.5$lab_processing <- paste0(df.lab.5$ec_sd,
                                  "T",
                                  df.lab.5$ec_st)

# join date when placed in incubator
df.lab.5$lab_incubator_placed <- paste0(df.lab.5$ec_isd,
                                        "T",
                                        df.lab.5$ec_ist)

# join date when removed from incubator
df.lab.5$lab_incubator_removed <- paste0(df.lab.5$ec_ied,
                                         "T",
                                         df.lab.5$ec_iet)



##### recode #####

df.lab.5$lab_start_dt <- df.lab.5$lab_processing

df.lab.5$lab_1_dil_tested <- case_when(
        df.lab.5$lab_1_dil_tested == 1 ~ 7,
        df.lab.5$lab_1_dil_tested == 2 ~ 6,
        df.lab.5$lab_1_dil_tested == 3 ~ 5,
        df.lab.5$lab_1_dil_tested == 4 ~ 4,
        df.lab.5$lab_1_dil_tested == 5 ~ 3,
        df.lab.5$lab_1_dil_tested == 6 ~ 2)

df.lab.5$lab_2_dil_tested <- case_when(
        df.lab.5$lab_2_dil_tested == 1 ~ 7,
        df.lab.5$lab_2_dil_tested == 2 ~ 6,
        df.lab.5$lab_2_dil_tested == 3 ~ 5,
        df.lab.5$lab_2_dil_tested == 4 ~ 4,
        df.lab.5$lab_2_dil_tested == 5 ~ 3,
        df.lab.5$lab_2_dil_tested == 6 ~ 2)

df.lab.5$lab_3_dil_tested <- case_when(
        df.lab.5$lab_3_dil_tested == 1 ~ 7,
        df.lab.5$lab_3_dil_tested == 2 ~ 6,
        df.lab.5$lab_3_dil_tested == 3 ~ 5,
        df.lab.5$lab_3_dil_tested == 4 ~ 4,
        df.lab.5$lab_3_dil_tested == 5 ~ 3,
        df.lab.5$lab_3_dil_tested == 6 ~ 2)

df.lab.5$lab_sample_type <- case_when(
        df.lab.5$lab_sample_type == 1 ~ 9,
        df.lab.5$lab_sample_type == 2 ~ 1,
        df.lab.5$lab_sample_type == 3 ~ 6,
        df.lab.5$lab_sample_type == 4 ~ 8,
        df.lab.5$lab_sample_type == 5 ~ 7)


# valid reading
df.lab.5$lab_1_ecoli_reading_membrane <- case_when(
        df.lab.5$lab_1_ecoli_membrane == 999 ~ 1,
        df.lab.5$lab_1_ecoli_membrane == 998 ~ 2,
        df.lab.5$lab_1_ecoli_membrane < 998 ~ 3)

df.lab.5$lab_2_ecoli_reading_membrane <- case_when(
        df.lab.5$lab_2_ecoli_membrane == 999 ~ 1,
        df.lab.5$lab_2_ecoli_membrane == 998 ~ 2,
        df.lab.5$lab_2_ecoli_membrane < 998 ~ 3)

df.lab.5$lab_3_ecoli_reading_membrane <- case_when(
        df.lab.5$lab_3_ecoli_membrane == 999 ~ 1,
        df.lab.5$lab_3_ecoli_membrane == 998 ~ 2,
        df.lab.5$lab_3_ecoli_membrane < 998 ~ 3)

# 3rd dil
df.lab.5$lab_3_dilution_performed <- ifelse(df.lab.5$lab_3_ecoli_membrane >= 0, 1, 0)
df.lab.5$lab_3_dilution_performed[is.na(df.lab.5$lab_3_dilution_performed)] <- 0

# volume
df.lab.5$lab_1_volume <- 100
df.lab.5$lab_2_volume <- 100
df.lab.5$lab_3_volume <- ifelse(df.lab.5$lab_3_dilution_performed > 0, 100, NA)


##### create new variables #####
# Create country or deployment code
df.lab.5$dply_num <- 5

df.lab.5$lab_analysis <- 2

# Create unique ID variable
df.lab.5$lab_UID <- NA

df.lab.5$lab_id <- 1000:(1000+nrow(df.lab.5)-1)
df.lab.5$lab_id_val <- df.lab.5$lab_id

df.lab.5$lab_extra_data <- 1

# paste number together
df.lab.5$lab_UID <- paste0(sprintf("%02d", df.lab.5$dply_num),
                           "_",
                           sprintf("%02d", df.lab.5$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.5$lab_id))

# Create date variable
df.lab.5$lab_date <- df.lab.5$ec_sd
df.lab.5$lab_date <- as.Date(df.lab.5$lab_date, format = "%Y-%m-%d")

#weight
df.lab.5$lab_pa_weight[df.lab.5$lab_sample_type == 8] <- df.lab.5$weight[df.lab.5$lab_sample_type == 8]


##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra5 <- which(colnames(df.lab.5) %in% setdiff(colnames(df.lab.5), colnames(df.lab.0)))

# remove "extra" columns from df5
df.lab.5.clean <- df.lab.5[, -extra5]

# export extra data
df.lab.5.extra <- df.lab.5[, extra5]
df.lab.5.extra$lab_UID <- df.lab.5$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.5.clean), colnames(df.lab.0))


df.lab <- bind_rows(df.lab, df.lab.5.clean)

# df.lab %>% group_by(dply_num, lab_sample_type) %>% summarise(n = n())
# **********************************************************************************

# **********************************************************************************
#---- 6 GHANA Accra -----
setdiff(colnames(df.lab.6), colnames(df.lab.0))

##### rename variables #####
df.lab.6$col_start_dt <- NULL
df.lab.6$col_end_dt <- NULL

colnames(df.lab.6)[colnames(df.lab.6) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.6)[colnames(df.lab.6) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.6)[colnames(df.lab.6) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.6)[colnames(df.lab.6) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.6)[colnames(df.lab.6) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.6)[colnames(df.lab.6) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####
# add weight to deployment 6, from deployment 7 weights, assumed beans 200grams
# table(df.col.6$col_sf_type)
# id.kenkey6 <- df.col.6$col_id[df.col.6$col_sample_type == 10 & df.col.6$col_sf_type == 8]
# id.waakye6 <- df.col.6$col_id[df.col.6$col_sample_type == 10 & df.col.6$col_sf_type == 7]
# id.beans6 <- df.col.6$col_id[df.col.6$col_sample_type == 10 & df.col.6$col_sf_type == 9]

# kenkey
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5001] <- 387.24
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5003] <- 387.24
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5010] <- 387.24
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5101] <- 387.24
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5102] <- 387.24
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5106] <- 387.24

# waakye
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5002] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5005] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5007] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5009] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5104] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5105] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5108] <- 178.41
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5110] <- 178.41

# beans
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5004] <- 200
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5006] <- 200
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5008] <- 200
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5103] <- 200
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5107] <- 200
df.lab.6$lab_sf_weight[df.lab.6$lab_id == 5109] <- 200

##### create new variables #####
df.lab.6$lab_analysis <- 2

# Create country or deployment code
df.lab.6$dply_num <- 6

# Create unique ID variable
df.lab.6$lab_UID <- NA

# paste number together
df.lab.6$lab_UID <- paste0(sprintf("%02d", df.lab.6$dply_num),
                           "_",
                           sprintf("%02d", df.lab.6$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.6$lab_id))

# Create date variable
df.lab.6$lab_date <- NA
df.lab.6$lab_date <- substr(df.lab.6$lab_processing, 1, 10)
df.lab.6$lab_date <- as.Date(df.lab.6$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra6 <- which(colnames(df.lab.6) %in% setdiff(colnames(df.lab.6), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.6.clean <- df.lab.6[, -extra6]

# export extra data
df.lab.6.extra <- df.lab.6[, extra6]
df.lab.6.extra$lab_UID <- df.lab.6$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.6.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.6.clean)
# **********************************************************************************

# **********************************************************************************
#---- 7 GHANA Kumasi -----
setdiff(colnames(df.lab.7), colnames(df.lab.0))

##### rename variables #####
df.lab.7$col_start_dt <- NULL
df.lab.7$col_end_dt <- NULL

colnames(df.lab.7)[colnames(df.lab.7) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.7)[colnames(df.lab.7) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.7)[colnames(df.lab.7) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.7)[colnames(df.lab.7) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.7)[colnames(df.lab.7) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.7)[colnames(df.lab.7) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####


##### create new variables #####
df.lab.7$lab_analysis <- 2

# Create country or deployment code
df.lab.7$dply_num <- 7

# Create unique ID variable
df.lab.7$lab_UID <- NA

# paste number together
df.lab.7$lab_UID <- paste0(sprintf("%02d", df.lab.7$dply_num),
                           "_",
                           sprintf("%02d", df.lab.7$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.7$lab_id))

# Create date variable
df.lab.7$lab_date <- NA
df.lab.7$lab_date <- substr(df.lab.7$lab_processing, 1, 10)
df.lab.7$lab_date <- as.Date(df.lab.7$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra7 <- which(colnames(df.lab.7) %in% setdiff(colnames(df.lab.7), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.7.clean <- df.lab.7[, -extra7]

# export extra data
df.lab.7.extra <- df.lab.7[, extra7]
df.lab.7.extra$lab_UID <- df.lab.7$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.7.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.7.clean)
# **********************************************************************************

# **********************************************************************************
#---- 8 MOZAMBIQUE -----
setdiff(colnames(df.lab.8), colnames(df.lab.0))

##### rename variables #####

colnames(df.lab.8)[colnames(df.lab.8) == 'sp_type'] <- 'lab_sample_type'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_dil1'] <- 'lab_1_dil_tested'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_dil2'] <- 'lab_2_dil_tested'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_dil3'] <- 'lab_3_dil_tested'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_ecnt1'] <- 'lab_1_ecoli_membrane'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_ecnt2'] <- 'lab_2_ecoli_membrane'
colnames(df.lab.8)[colnames(df.lab.8) == 'ec_ecnt3'] <- 'lab_3_ecoli_membrane'


# date
df.lab.8$ec_sd <- as.Date(df.lab.8$ec_sd, "%m/%d/%Y")
df.lab.8$ec_isd <- as.Date(df.lab.8$ec_isd, "%m/%d/%Y")
df.lab.8$ec_ied <- as.Date(df.lab.8$ec_ied, "%m/%d/%Y")
df.lab.8$ec_st <- format(strptime(df.lab.8$ec_st, "%I:%M:%S %p"), "%H:%M:%S") 
df.lab.8$ec_ist <- format(strptime(df.lab.8$ec_ist, "%I:%M:%S %p"), "%H:%M:%S") 
df.lab.8$ec_iet <- format(strptime(df.lab.8$ec_iet, "%I:%M:%S %p"), "%H:%M:%S") 

# join date when processed
df.lab.8$lab_processing <- paste0(df.lab.8$ec_sd,
                                  "T",
                                  df.lab.8$ec_st)

# join date when placed in incubator
df.lab.8$lab_incubator_placed <- paste0(df.lab.8$ec_isd,
                                        "T",
                                        df.lab.8$ec_ist)

# join date when removed from incubator
df.lab.8$lab_incubator_removed <- paste0(df.lab.8$ec_ied,
                                         "T",
                                         df.lab.8$ec_iet)



##### recode #####

df.lab.8$lab_start_dt <- df.lab.8$lab_processing

df.lab.8$lab_1_dil_tested <- case_when(
        df.lab.8$lab_1_dil_tested == 1 ~ 7,
        df.lab.8$lab_1_dil_tested == 2 ~ 6,
        df.lab.8$lab_1_dil_tested == 3 ~ 5,
        df.lab.8$lab_1_dil_tested == 4 ~ 4,
        df.lab.8$lab_1_dil_tested == 5 ~ 3,
        df.lab.8$lab_1_dil_tested == 6 ~ 2)

df.lab.8$lab_2_dil_tested <- case_when(
        df.lab.8$lab_2_dil_tested == 1 ~ 7,
        df.lab.8$lab_2_dil_tested == 2 ~ 6,
        df.lab.8$lab_2_dil_tested == 3 ~ 5,
        df.lab.8$lab_2_dil_tested == 4 ~ 4,
        df.lab.8$lab_2_dil_tested == 5 ~ 3,
        df.lab.8$lab_2_dil_tested == 6 ~ 2)

df.lab.8$lab_3_dil_tested <- case_when(
        df.lab.8$lab_3_dil_tested == 1 ~ 7,
        df.lab.8$lab_3_dil_tested == 2 ~ 6,
        df.lab.8$lab_3_dil_tested == 3 ~ 5,
        df.lab.8$lab_3_dil_tested == 4 ~ 4,
        df.lab.8$lab_3_dil_tested == 5 ~ 3,
        df.lab.8$lab_3_dil_tested == 6 ~ 2)

df.lab.8$lab_sample_type <- case_when(
        df.lab.8$lab_sample_type == 1 ~ 9, #bathing water
        df.lab.8$lab_sample_type == 2 ~ 1, #drains
        df.lab.8$lab_sample_type == 3 ~ 6, #flood water
        df.lab.8$lab_sample_type == 4 ~ 8, #soil
        df.lab.8$lab_sample_type == 5 ~ 7, #swabs
        df.lab.8$lab_sample_type == 6 ~ 2, #produce
        df.lab.8$lab_sample_type == 7 ~ 3) #drinking water


# valid reading
df.lab.8$lab_1_ecoli_reading_membrane <- case_when(
        df.lab.8$lab_1_ecoli_membrane == 999 ~ 1,
        df.lab.8$lab_1_ecoli_membrane == 998 ~ 2,
        df.lab.8$lab_1_ecoli_membrane < 998 ~ 3)

df.lab.8$lab_2_ecoli_reading_membrane <- case_when(
        df.lab.8$lab_2_ecoli_membrane == 999 ~ 1,
        df.lab.8$lab_2_ecoli_membrane == 998 ~ 2,
        df.lab.8$lab_2_ecoli_membrane < 998 ~ 3)

df.lab.8$lab_3_ecoli_reading_membrane <- case_when(
        df.lab.8$lab_3_ecoli_membrane == 999 ~ 1,
        df.lab.8$lab_3_ecoli_membrane == 998 ~ 2,
        df.lab.8$lab_3_ecoli_membrane < 998 ~ 3)

# 3rd dil
df.lab.8$lab_3_dilution_performed <- ifelse(df.lab.8$lab_3_ecoli_membrane >= 0, 1, 0)
df.lab.8$lab_3_dilution_performed[is.na(df.lab.8$lab_3_dilution_performed)] <- 0

# volume
df.lab.8$lab_1_volume <- 100
df.lab.8$lab_2_volume <- 100
df.lab.8$lab_3_volume <- ifelse(df.lab.8$lab_3_dilution_performed > 0, 100, NA)


##### create new variables #####
# Create country or deployment code
df.lab.8$dply_num <- 8

df.lab.8$lab_analysis <- 2

# Create unique ID variable
df.lab.8$lab_UID <- NA

df.lab.8$lab_id <- 1000:(1000+nrow(df.lab.8)-1)
df.lab.8$lab_id_val <- df.lab.8$lab_id

df.lab.8$lab_extra_data <- 1

# paste number together
df.lab.8$lab_UID <- paste0(sprintf("%02d", df.lab.8$dply_num),
                           "_",
                           sprintf("%02d", df.lab.8$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.8$lab_id))

# Create date variable
df.lab.8$lab_date <- df.lab.8$ec_sd
df.lab.8$lab_date <- as.Date(df.lab.8$lab_date, format = "%Y-%m-%d")

#weight
df.lab.8$lab_pa_weight[df.lab.8$lab_sample_type == 8] <- df.lab.8$weight[df.lab.8$lab_sample_type == 8]


##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra8 <- which(colnames(df.lab.8) %in% setdiff(colnames(df.lab.8), colnames(df.lab.0)))

# remove "extra" columns from df5
df.lab.8.clean <- df.lab.8[, -extra8]

# export extra data
df.lab.8.extra <- df.lab.8[, extra8]
df.lab.8.extra$lab_UID <- df.lab.8$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.8.clean), colnames(df.lab.0))


df.lab <- bind_rows(df.lab, df.lab.8.clean)
# **********************************************************************************

# **********************************************************************************
#---- 9 UGANDA Kampala -----
setdiff(colnames(df.lab.9), colnames(df.lab.0))

##### rename variables #####
df.lab.9$col_start_dt <- NULL
df.lab.9$col_end_dt <- NULL

colnames(df.lab.9)[colnames(df.lab.9) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.9)[colnames(df.lab.9) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.9)[colnames(df.lab.9) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.9)[colnames(df.lab.9) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.9)[colnames(df.lab.9) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.9)[colnames(df.lab.9) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####
df.lab.9$lab_sample_type[df.lab.9$lab_sample_type ==  9] <- 33 #other drinking water

##### create new variables #####
df.lab.9$lab_analysis <- 2 # 2=membrane filtration

# Create country or deployment code
df.lab.9$dply_num <- 9

# Create unique ID variable
df.lab.9$lab_UID <- NA

# paste number together
df.lab.9$lab_UID <- paste0(sprintf("%02d", df.lab.9$dply_num),
                           "_",
                           sprintf("%02d", df.lab.9$lab_sample_type),
                           "_",
                           sprintf("%04d", df.lab.9$lab_id))

# Create date variable
df.lab.9$lab_date <- NA
df.lab.9$lab_date <- substr(df.lab.9$lab_processing, 1, 10)
df.lab.9$lab_date <- as.Date(df.lab.9$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra9 <- which(colnames(df.lab.9) %in% setdiff(colnames(df.lab.9), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.9.clean <- df.lab.9[, -extra9]

# export extra data
df.lab.9.extra <- df.lab.9[, extra9]
df.lab.9.extra$lab_UID <- df.lab.9$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.9.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.9.clean)
# **********************************************************************************

# **********************************************************************************
#---- 10 USA Atlanta -----
setdiff(colnames(df.lab.10), colnames(df.lab.0))

##### rename variables #####
colnames(df.lab.10)[colnames(df.lab.10) == 'start_dt'] <- 'lab_start_dt'
colnames(df.lab.10)[colnames(df.lab.10) == 'end_dt'] <- 'lab_end_dt'

df.lab.10$lab_sample_type[df.lab.10$lab_sample_type ==  1] <- 8 #Particulate
df.lab.10$lab_sample_type[df.lab.10$lab_sample_type ==  2] <- 2 #Produce
df.lab.10$lab_sample_type[df.lab.10$lab_sample_type ==  3] <- 7 #Swab
df.lab.10$lab_sample_type[df.lab.10$lab_sample_type ==  4] <- 3 #Drinking Water


df.lab.10 <- df.lab.10 %>% group_by(lab_sample_type, lab_analysis_method) %>% 
        mutate(lab_id = row_number(lab_sample_type)) %>% ungroup()

df.lab.10$lab_id <- paste0(sprintf("%01d", df.lab.10$lab_sample_type),
                           sprintf("%03d", df.lab.10$lab_id))
df.lab.10$lab_id <- as.numeric(as.character(df.lab.10$lab_id))
# table(df.lab.10$lab_id)
df.lab.10$lab_id_val <- df.lab.10$lab_id


##### recode #####
df.lab.10$lab_analysis <- df.lab.10$lab_analysis_method

df.lab.10$lab_1_ecoli_reading_idexx[df.lab.10$lab_1_ecoli_reading_idexx ==  2] <- 1
df.lab.10$lab_1_ecoli_reading_idexx[df.lab.10$lab_1_ecoli_reading_idexx ==  3] <- 2
df.lab.10$lab_2_ecoli_reading_idexx[df.lab.10$lab_2_ecoli_reading_idexx ==  2] <- 1
df.lab.10$lab_2_ecoli_reading_idexx[df.lab.10$lab_2_ecoli_reading_idexx ==  3] <- 2
df.lab.10$lab_3_ecoli_reading_idexx[df.lab.10$lab_3_ecoli_reading_idexx ==  2] <- 1
df.lab.10$lab_3_ecoli_reading_idexx[df.lab.10$lab_3_ecoli_reading_idexx ==  3] <- 2

df.lab.10$lab_pa_weight <- 10
df.lab.10$lab_pa_weight <- case_when(
        df.lab.10$lab_sample_type == 8 ~ 10)

# get small big wells for idexx
colnames(df.lab.10)


df.lab.10$lab_1_ecoli_big_idexx
df.lab.10$lab_1_ecoli_small_idexx <- 0

df.lab.10$lab_2_ecoli_big_idexx
df.lab.10$lab_2_ecoli_small_idexx <- 0

df.lab.10$lab_3_ecoli_big_idexx
df.lab.10$lab_3_ecoli_small_idexx[df.lab.10$lab_3_ecoli_small_idexx == 3] <- 0


##### create new variables #####
# table(df.lab.10$lab_analysis)
# 1 idexx
# 2 membrane filtration
df.lab.10 <- df.lab.10 %>% filter(lab_analysis == 1)


df.lab.10.1 <- read.csv(paste0(getwd(), "/data/deployments/10_usa-atl/", "atl_flood_lab", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T, 
                      na.strings = c("n/a", "NA", "N/A", "---"))

df.lab.10 <- bind_rows(df.lab.10, df.lab.10.1)
# Create country or deployment code
df.lab.10$dply_num <- 10

# Create unique ID variable
df.lab.10$lab_UID <- NA

# paste number together
df.lab.10$lab_UID <- paste0(sprintf("%02d", df.lab.10$dply_num),
                            "_",
                            sprintf("%02d", df.lab.10$lab_sample_type),
                            "_",
                            sprintf("%04d", df.lab.10$lab_id))

# Create date variable
df.lab.10$lab_date <- NA
df.lab.10$lab_date <- substr(df.lab.10$lab_processing, 1, 10)
df.lab.10$lab_date <- as.Date(df.lab.10$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra10 <- which(colnames(df.lab.10) %in% setdiff(colnames(df.lab.10), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.10.clean <- df.lab.10[, -extra10]

# export extra data
df.lab.10.extra <- df.lab.10[, extra10]
df.lab.10.extra$lab_UID <- df.lab.10$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.10.clean), colnames(df.lab.0))
df.lab.10$deviceid <- NA
df.lab.10$deviceid <- as.numeric(df.lab.10$deviceid)


df.lab <- plyr:::rbind.fill(df.lab, df.lab.10.clean)
# **********************************************************************************

#---- 11 Vellore -----
setdiff(colnames(df.lab.11), colnames(df.lab.0))

##### rename variables #####
colnames(df.lab.11)[colnames(df.lab.11) == 'sample_type'] <- 'lab_sample_type'
colnames(df.lab.11)[colnames(df.lab.11) == 'sample_id'] <- 'lab_id'
colnames(df.lab.11)[colnames(df.lab.11) == 'collection_datetime'] <- 'lab_start_dt'

colnames(df.lab.11)[colnames(df.lab.11) == 'processing_datetime'] <- 'lab_processing'
colnames(df.lab.11)[colnames(df.lab.11) == 'incubation_start_datetime'] <- 'lab_incubator_placed'
colnames(df.lab.11)[colnames(df.lab.11) == 'incubation_end_datetime'] <- 'lab_incubator_removed'
colnames(df.lab.11)[colnames(df.lab.11) == 'produce_weight'] <- 'lab_p_weight'


colnames(df.lab.11)[colnames(df.lab.11) == 'count1'] <- 'lab_1_ecoli_membrane'
colnames(df.lab.11)[colnames(df.lab.11) == 'count2'] <- 'lab_2_ecoli_membrane'
colnames(df.lab.11)[colnames(df.lab.11) == 'dilution1'] <- 'lab_1_dil_tested'
colnames(df.lab.11)[colnames(df.lab.11) == 'dilution2'] <- 'lab_2_dil_tested'


##### recode #####
df.lab.11$lab_sample_type <- case_when(df.lab.11$lab_sample_type ==  1 ~ 8,
                                       df.lab.11$lab_sample_type ==  2 ~ 7,
                                       df.lab.11$lab_sample_type ==  3 ~ 2,
                                       df.lab.11$lab_sample_type ==  4 ~ 3,
                                       df.lab.11$lab_sample_type ==  7 ~ 9)
df.lab.11$lab_id <- 1000+(df.lab.11$lab_id)
df.lab.11$lab_id_val <- df.lab.11$lab_id

# Create date variable
df.lab.11$lab_date <- as.Date(df.lab.11$lab_start_dt, format = "%m/%d/%Y")

df.lab.11$lab_start_dt <- paste0(as.Date(df.lab.11$lab_start_dt, "%m/%d/%Y"), "T00:00:00.000+00")

#status
df.lab.11$lab_1_ecoli_reading_membrane <- case_when(df.lab.11$plate1_tntc == "TRUE" ~ 1,
                                                    df.lab.11$plate1_tntc == "FALSE" ~ 3)

df.lab.11$lab_2_ecoli_reading_membrane <- case_when(df.lab.11$plate2_tntc == "TRUE" ~ 1,
                                                    df.lab.11$plate2_tntc == "FALSE" ~ 3)

df.lab.11$lab_3_dilution_performed <- 0

df.lab.11$lab_1_dil_tested <- case_when(df.lab.11$lab_1_dil_tested ==  1 ~ 7,
                                        df.lab.11$lab_1_dil_tested ==  10 ~ 6,
                                        df.lab.11$lab_1_dil_tested ==  100 ~ 5,
                                        df.lab.11$lab_1_dil_tested ==  1000 ~ 4)

df.lab.11$lab_2_dil_tested <- case_when(df.lab.11$lab_2_dil_tested ==  1 ~ 7,
                                        df.lab.11$lab_2_dil_tested ==  10 ~ 6,
                                        df.lab.11$lab_2_dil_tested ==  100 ~ 5,
                                        df.lab.11$lab_2_dil_tested ==  1000 ~ 4)
#volume 
df.lab.11$lab_1_volume[df.lab.11$lab_sample_type == 2 ] <- 100 #10
df.lab.11$lab_1_volume[df.lab.11$lab_sample_type == 3 ] <- 100
df.lab.11$lab_1_volume[df.lab.11$lab_sample_type == 7 ] <- 100 #10
df.lab.11$lab_1_volume[df.lab.11$lab_sample_type == 8 ] <- 100 #1
df.lab.11$lab_1_volume[df.lab.11$lab_sample_type == 9 ] <- 100

df.lab.11$lab_2_volume[df.lab.11$lab_sample_type == 2 ] <- 100 #1
df.lab.11$lab_2_volume[df.lab.11$lab_sample_type == 3 ] <- 100 #10
df.lab.11$lab_2_volume[df.lab.11$lab_sample_type == 7 ] <- 100 #1
df.lab.11$lab_2_volume[df.lab.11$lab_sample_type == 8 ] <- 100 #1
df.lab.11$lab_2_volume[df.lab.11$lab_sample_type == 9 ] <- 100 #10


##### create new variables #####
df.lab.11$lab_analysis <- 2 # 2=membrane filtration

# Create country or deployment code
df.lab.11$dply_num <- 11

# Create unique ID variable
df.lab.11$lab_UID <- NA

# paste number together
df.lab.11$lab_UID <- paste0(sprintf("%02d", df.lab.11$dply_num),
                            "_",
                            sprintf("%02d", df.lab.11$lab_sample_type),
                            "_",
                            sprintf("%04d", df.lab.11$lab_id))



##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra11 <- which(colnames(df.lab.11) %in% setdiff(colnames(df.lab.11), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.11.clean <- df.lab.11[, -extra11]

# export extra data
df.lab.11.extra <- df.lab.11[, extra11]
df.lab.11.extra$lab_UID <- df.lab.11$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.11.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.11.clean)
# **********************************************************************************


# **********************************************************************************
#---- 12 SENGAL Dakar-----
setdiff(colnames(df.lab.12), colnames(df.lab.0))

##### rename variables #####
df.lab.12$col_start_dt <- NULL
df.lab.12$col_end_dt <- NULL

colnames(df.lab.12)[colnames(df.lab.12) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.12)[colnames(df.lab.12) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.12)[colnames(df.lab.12) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.12)[colnames(df.lab.12) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.12)[colnames(df.lab.12) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.12)[colnames(df.lab.12) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####
table(df.lab.12$lab_sample_type)

df.lab.12$lab_sample_type[df.lab.12$lab_sample_type ==  11] <- 99 #food swab

df.lab.12$lab_1_ecoli_membrane[df.lab.12$lab_1_ecoli_reading_membrane == 1] <- NA


df.lab.12$lab_id[df.lab.12$lab_start_dt == "2020-01-12T22:34:48.000-00:00"] <- 2040
df.lab.12$lab_id_val[df.lab.12$lab_start_dt == "2020-01-12T22:34:48.000-00:00"] <- 2040

# duplicated(df.lab.12$lab_id)
# # 4019 is duplicated
# n_occur <- data.frame(table(df.lab.12$lab_id))

df.lab.12$lab_id[df.lab.12$lab_start_dt == "2020-01-03T15:26:24.000-00:00"] <- 4028
df.lab.12$lab_id_val[df.lab.12$lab_start_dt == "2020-01-03T15:26:24.000-00:00"] <- 4028

##### create new variables #####
# lab analysis
df.lab.12$lab_analysis <- 2 # 1 idexx, 2 mf

# Create country or deployment code
df.lab.12$dply_num <- 12

# Create unique ID variable
df.lab.12$lab_UID <- NA

# paste number together
df.lab.12$lab_UID <- paste0(sprintf("%02d", df.lab.12$dply_num),
                            "_",
                            sprintf("%02d", df.lab.12$lab_sample_type),
                            "_",
                            sprintf("%04d", df.lab.12$lab_id))

# Create date variable
df.lab.12$lab_date <- NA
df.lab.12$lab_date <- substr(df.lab.12$lab_processing, 1, 10)
df.lab.12$lab_date <- as.Date(df.lab.12$lab_date, format = "%Y-%m-%d")


# filter out food swabs
df.lab.12.swabs <- df.lab.12 %>% filter(lab_sample_type == 99)
df.lab.12 <- df.lab.12 %>% filter(lab_sample_type != 99)


##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra12 <- which(colnames(df.lab.12) %in% setdiff(colnames(df.lab.12), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.12.clean <- df.lab.12[, -extra12]

# export extra data
df.lab.12.extra <- df.lab.12[, extra12]
df.lab.12.extra$lab_UID <- df.lab.12$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.12.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.12.clean)
# **********************************************************************************


# **********************************************************************************
#---- 13 ZAMBIA 2 -----
setdiff(colnames(df.lab.13), colnames(df.lab.0))

##### rename variables #####
df.lab.13$col_start_dt <- NULL
df.lab.13$col_end_dt <- NULL

colnames(df.lab.13)[colnames(df.lab.13) == 'lab_1_group.lab_1_dil_tested'] <- 'lab_1_dil_tested'
colnames(df.lab.13)[colnames(df.lab.13) == 'lab_1_group.lab_1_volume'] <- 'lab_1_volume'
colnames(df.lab.13)[colnames(df.lab.13) == 'lab_2_group.lab_2_dil_tested'] <- 'lab_2_dil_tested'
colnames(df.lab.13)[colnames(df.lab.13) == 'lab_2_group.lab_2_volume'] <- 'lab_2_volume'
colnames(df.lab.13)[colnames(df.lab.13) == 'lab_3_group.lab_3_dil_tested'] <- 'lab_3_dil_tested'
colnames(df.lab.13)[colnames(df.lab.13) == 'lab_3_group.lab_3_volume'] <- 'lab_3_volume'

##### recode #####
table(df.lab.13$lab_sample_type)

df.lab.13$lab_sample_type[df.lab.13$lab_sample_type ==  11] <- 33 #other drinking water


##### create new variables #####
# lab analysis
df.lab.13$lab_analysis <- 1 # 1 idexx, 2 mf

# Create country or deployment code
df.lab.13$dply_num <- 13

# Create unique ID variable
df.lab.13$lab_UID <- NA

# paste number together
df.lab.13$lab_UID <- paste0(sprintf("%02d", df.lab.13$dply_num),
                            "_",
                            sprintf("%02d", df.lab.13$lab_sample_type),
                            "_",
                            sprintf("%04d", df.lab.13$lab_id))

# Create date variable
df.lab.13$lab_date <- NA
df.lab.13$lab_date <- substr(df.lab.13$lab_processing, 1, 10)
df.lab.13$lab_date <- as.Date(df.lab.13$lab_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra13 <- which(colnames(df.lab.13) %in% setdiff(colnames(df.lab.13), colnames(df.lab.0)))

# remove "extra" columns from df4
df.lab.13.clean <- df.lab.13[, -extra13]

# export extra data
df.lab.13.extra <- df.lab.13[, extra13]
df.lab.13.extra$lab_UID <- df.lab.13$lab_UID

##### merge datasets #####
setdiff(colnames(df.lab.13.clean), colnames(df.lab.0))

df.lab <- bind_rows(df.lab, df.lab.13.clean)
# **********************************************************************************


# **********************************************************************************
#---- 99 WRITE DATA -----

write.csv(df.lab, paste0(getwd(), "/data/", "lab_merged_", Sys.Date(), ".csv"), row.names = F, na="")

write.csv(df.lab.1.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_01.csv"), row.names = F, na="")
write.csv(df.lab.2.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_02.csv"), row.names = F, na="")
write.csv(df.lab.3.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_03.csv"), row.names = F, na="")
write.csv(df.lab.5.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_05.csv"), row.names = F, na="")
write.csv(df.lab.8.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_08.csv"), row.names = F, na="")
write.csv(df.lab.10.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_10.csv"), row.names = F, na="")
write.csv(df.lab.11.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_11.csv"), row.names = F, na="")
write.csv(df.lab.12.swabs, paste0(getwd(), "/data/extra_data/", "lab_extra_12_swabs.csv"), row.names = F, na="")
write.csv(df.lab.13.extra, paste0(getwd(), "/data/extra_data/", "lab_extra_13.csv"), row.names = F, na="")
