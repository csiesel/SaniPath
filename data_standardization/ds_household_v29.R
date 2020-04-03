# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# Form: household (h)
# **********************************************************************************
# Deployments: 1-13
# **********************************************************************************
# Last modified: February 12, 2020
# By: Wolfgang
# Version: v29
# **********************************************************************************

# **********************************************************************************

#---- SET-UP ----
source("helper_dataload_meta.R")

# read master form
df.h.0 <- read.table(paste0(getwd(), "/data/deployments/00_master/", "hh - latest version - xml - 2018-06-18-15-01-28", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T)

# read cambodia form
df.h.1 <- read.csv(paste0(getwd(), "/data/deployments/01_cambodia/", "SP CAMBO DATASET - household.csv"), 
                   stringsAsFactors=F, header=T )

# read bangladesh form
df.h.2 <- read.csv(paste0(getwd(), "/data/deployments/02_bangladesh/", "hh_data_011018_wm.csv"), 
                   stringsAsFactors=F, header=T )

# read ghana form
df.h.3 <- read.csv(paste0(getwd(), "/data/deployments/03_ghana/", "ghana h", ".csv"), 
                   stringsAsFactors=F, header=T,
                   na.strings = c("na", "NA", "Na", "---", ""))

# read zambia form
df.h.4 <- read.csv(paste0(getwd(), "/data/deployments/04_zambia/", "hh_lusaka", ".csv"), sep=",",
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.h.5 <- read.csv(paste0(getwd(), "/data/deployments/05_mozambique/", "compiled_behavior", ".csv"), sep=",", 
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana accra form
df.h.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "accrametro-household-1501", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.h.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "ksisanipath-household-1620", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read mozambique form
df.h.8 <- read.csv(paste0(getwd(), "/data/deployments/08_mozambique2/", "Household Survey", ".csv"), sep=",", 
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))
df.h.8c <- read.csv(paste0(getwd(), "/data/deployments/08_mozambique2/", "Children Survey", ".csv"), sep=",", 
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))
# read ghana kumasi form
df.h.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/", "hh_uganda", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.h.10 <- read.csv(paste0(getwd(), "/data/deployments/10_usa-atl/", "hh_redcap", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read vellore form
df.h.11 <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/", "hh_clean", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))


# read senegal form
df.h.12 <- read.csv(paste0(getwd(), "/data/deployments/12_senegal/", 
                           "Household_Survey-xlsform_waspa_final_2020_02_05_17_51_47", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# # read kolkata form
# df.h.99 <- read.csv(paste0(getwd(), "/data/deployments/12_kolkata/", "spt_hh", ".csv"), 
#                     sep=",", stringsAsFactors=F, header=T, 
#                     na.strings = c("n/a", "NA", "N/A", "---"))

# read lusaka2 form
df.h.13 <- read.csv(paste0(getwd(), "/data/deployments/13_zambia2/", "Household_Survey-xlsform_lusaka2019_final_2019_11_04_18_49_19", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))
# **********************************************************************************
#---- MASTER ----
##### create new variables #####
df.h.0$h_odw <- NA
df.h.0$h_odw_a <- NA
df.h.0$h_odw_a_other <- NA
df.h.0$h_odw_c <- NA
df.h.0$h_odw_c_other <- NA

df.h.0$h_odw2 <- NA
df.h.0$h_odw2_a <- NA
df.h.0$h_odw2_a_other <- NA
df.h.0$h_odw2_c <- NA
df.h.0$h_odw2_c_other <- NA

df.h.0$h_extra_data <- NA   #not used in 01 or 02 but should be included for subsequent deployments
df.h.0$dply_num <- NA
df.h.0$h_UID <- NA

df.h.0$h_date <- NA
df.h.0$h_date <- as.Date(df.h.0$h_date, format = "%Y-%m-%d")



##### merge datasets #####
# delete rows and override the original table
df.h <- df.h.0[-c(1:1),]

# **********************************************************************************
#---- 1 CAMBODIA ----

##### rename variables #####
colnames(df.h.1)[colnames(df.h.1) == 'h_location'] <- 'h_coordinates'
colnames(df.h.1)[colnames(df.h.1) == 'X_h_location_latitude'] <- 'X_h_coordinates_latitude'
colnames(df.h.1)[colnames(df.h.1) == 'X_h_location_longitude'] <- 'X_h_coordinates_longitude'
colnames(df.h.1)[colnames(df.h.1) == 'X_h_location_altitude'] <- 'X_h_coordinates_altitude'
colnames(df.h.1)[colnames(df.h.1) == 'X_h_location_precision'] <- 'X_h_coordinates_precision'

colnames(df.h.1)[colnames(df.h.1) == 'h_m'] <- 'h_odw'
colnames(df.h.1)[colnames(df.h.1) == 'h_m_a_prim_num'] <- 'h_odw_a'
colnames(df.h.1)[colnames(df.h.1) == 'h_m_a_prim_num_other'] <- 'h_odw_a_other'
colnames(df.h.1)[colnames(df.h.1) == 'h_m_c_prim_num'] <- 'h_odw_c'
colnames(df.h.1)[colnames(df.h.1) == 'h_m_c_prim_num_other'] <- 'h_odw_c_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_t'] <- 'h_dw_e_wt_note'
colnames(df.h.1)[colnames(df.h.1) == 'h_t_f'] <- 'h_dw_e_wt'
colnames(df.h.1)[colnames(df.h.1) == 'h_t_f_other'] <- 'h_dw_e_wt_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_th'] <- 'h_pl'
colnames(df.h.1)[colnames(df.h.1) == 'h_th_a'] <- 'h_pl_a_th'
colnames(df.h.1)[colnames(df.h.1) == 'h_th_a_other'] <- 'h_pl_a_th_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_tu_a'] <- 'h_pl_a_tu'
colnames(df.h.1)[colnames(df.h.1) == 'h_tu_a_other'] <- 'h_pl_a_tu_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_tw_a'] <- 'h_pl_a_tw'
colnames(df.h.1)[colnames(df.h.1) == 'h_tw_a_other'] <- 'h_pl_a_tw_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_tf_a'] <- 'h_pl_a_tf'
colnames(df.h.1)[colnames(df.h.1) == 'h_tf_a_other'] <- 'h_pl_a_tf_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_ts_a'] <- 'h_pl_a_ts'
colnames(df.h.1)[colnames(df.h.1) == 'h_ts_a_other'] <- 'h_pl_a_ts_other'

colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a'] <- 'h_pl_a_od'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.1'] <- 'h_pl_a_od.1'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.2'] <- 'h_pl_a_od.2'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.3'] <- 'h_pl_a_od.3'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.4'] <- 'h_pl_a_od.4'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.5'] <- 'h_pl_a_od.5'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.6'] <- 'h_pl_a_od.6'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a.7'] <- 'h_pl_a_od.7'
colnames(df.h.1)[colnames(df.h.1) == 'h_pd_a_other'] <- 'h_pl_a_od_other'

#check for differences (returns those in x but not y)
setdiff(colnames(df.h.1), colnames(df.h.0))

df.h.1$h_m_a_prim
df.h.1$h_m_a_sec
##### recode #####

#Recode h_dw_e_wt
# table(df.h.1$h_dw_e_wt)

df.h.1$h_dw_e_wt[df.h.1$h_dw_e_wt ==  6] <- 4         #not applicable
df.h.1$h_dw_e_wt[df.h.1$h_dw_e_wt ==  2] <- 3         #do not know
df.h.1$h_dw_e_wt[df.h.1$h_dw_e_wt ==  0] <- 2         #no

#Recode h_p_e
#doesn't seem to exist in DF1

#df.h.1$h_p_e[df.h.1$h_p_e ==  6] <- 4         #not applicable
#df.h.1$h_p_e[df.h.1$h_p_e ==  2] <- 3         #do not know
#df.h.1$h_p_e[df.h.1$h_p_e ==  0] <- 2         #no

#Recode h_pl_a_th
df.h.1$h_pl_a_th[df.h.1$h_pl_a_th ==  6] <- 4         #not applicable
df.h.1$h_pl_a_th[df.h.1$h_pl_a_th ==  2] <- 3         #do not know
df.h.1$h_pl_a_th[df.h.1$h_pl_a_th ==  0] <- 2         #no

#Recode h_pl_a_tu
df.h.1$h_pl_a_tu[df.h.1$h_pl_a_tu ==  6] <- 4         #not applicable
df.h.1$h_pl_a_tu[df.h.1$h_pl_a_tu ==  2] <- 3         #do not know
df.h.1$h_pl_a_tu[df.h.1$h_pl_a_tu ==  0] <- 2         #no

#Recode h_pl_a_tw
df.h.1$h_pl_a_tw[df.h.1$h_pl_a_tw ==  6] <- 4         #not applicable
df.h.1$h_pl_a_tw[df.h.1$h_pl_a_tw ==  2] <- 3         #do not know
df.h.1$h_pl_a_tw[df.h.1$h_pl_a_tw ==  0] <- 2         #no

#Recode h_pl_a_tf
df.h.1$h_pl_a_tf[df.h.1$h_pl_a_tf ==  6] <- 4         #not applicable
df.h.1$h_pl_a_tf[df.h.1$h_pl_a_tf ==  2] <- 3         #do not know
df.h.1$h_pl_a_tf[df.h.1$h_pl_a_tf ==  0] <- 2         #no

#fix household number
df.h.1$h_population[df.h.1$h_population ==  2001] <- 20        


##### create new variables #####
# Create country or deployment code
df.h.1$dply_num <- 1

# Create column to retain original ID values
df.h.1$h_id_old <- df.h.1$h_id

# Create new h_id variable
df.h.1$h_id <- (1000:(1000+nrow(df.h.1)-1))

# Create unique ID variable
df.h.1$h_UID <- NA

# paste number together
df.h.1$h_UID <- paste0(sprintf("%02d", df.h.1$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.h.1$h_id)))



# Create date variable
df.h.1$h_date <- NA
df.h.1$h_date <- substr(df.h.1$h_start, 1, 10)
df.h.1$h_date <- as.Date(df.h.1$h_date, format = "%Y-%m-%d")

# Create floodwater time interval variable
df.h.1$h_f_weekly <- 0    # 1 if weekly, 0 if not weekly

##### deployment-specific info #####
extra1 <- which(colnames(df.h.1) %in% setdiff(colnames(df.h.1), colnames(df.h.0)))

df.h.1$h_pl_a_od.1 <- as.integer(df.h.1$h_pl_a_od.1)
df.h.1$h_pl_a_od.2 <- as.integer(df.h.1$h_pl_a_od.2)
df.h.1$h_pl_a_od.3 <- as.integer(df.h.1$h_pl_a_od.3)
df.h.1$h_pl_a_od.4 <- as.integer(df.h.1$h_pl_a_od.4)
df.h.1$h_pl_a_od.5 <- as.integer(df.h.1$h_pl_a_od.5)
df.h.1$h_pl_a_od.6 <- as.integer(df.h.1$h_pl_a_od.6)
df.h.1$h_pl_a_od.7 <- as.integer(df.h.1$h_pl_a_od.7)

# remove "extra" columns from df1
df.h.1.clean <- df.h.1[, -extra1]

# export extra data
df.h.1.extra <- df.h.1[, extra1]
df.h.1.extra$h_UID <- df.h.1$h_UID


##### merge datasets #####
# Adjust column type
df.h.0$deviceid <- as.numeric(df.h.0$deviceid)

df.h.0 <- df.h.0[-1,]


# Check that dataframes have same dimensions
setdiff(colnames(df.h.1.clean), colnames(df.h.0))

# Merge clean datasets
df.h <- bind_rows(df.h.0, df.h.1.clean)


# **********************************************************************************
#---- 2 BANGLADESH ----

##### rename variables #####
colnames(df.h.2)[colnames(df.h.2) == 'h_m'] <- 'h_dw'
colnames(df.h.2)[colnames(df.h.2) == 'h_m_a'] <- 'h_dw_a'
colnames(df.h.2)[colnames(df.h.2) == 'h_m_a_other'] <- 'h_dw_a_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_m_c'] <- 'h_dw_c'
colnames(df.h.2)[colnames(df.h.2) == 'h_m_c_other'] <- 'h_dw_c_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_wt'] <- 'h_dw_e_wt_note'
colnames(df.h.2)[colnames(df.h.2) == 'h_wt_a'] <- 'h_dw_e_wt'
colnames(df.h.2)[colnames(df.h.2) == 'h_wt_a_other'] <- 'h_dw_e_wt_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_p_a2'] <- 'h_p_e'
colnames(df.h.2)[colnames(df.h.2) == 'h_p_a2_other'] <- 'h_p_e_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_th'] <- 'h_pl'
colnames(df.h.2)[colnames(df.h.2) == 'h_th_a'] <- 'h_pl_a_th'
colnames(df.h.2)[colnames(df.h.2) == 'h_th_a_other'] <- 'h_pl_a_th_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_tu_a'] <- 'h_pl_a_tu'
colnames(df.h.2)[colnames(df.h.2) == 'h_tu_a_other'] <- 'h_pl_a_tu_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_tw_a'] <- 'h_pl_a_tw'
colnames(df.h.2)[colnames(df.h.2) == 'h_tw_a_other'] <- 'h_pl_a_tw_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_tf_a'] <- 'h_pl_a_tf'

colnames(df.h.2)[colnames(df.h.2) == 'h_tu_a'] <- 'h_pl_a_tu'
colnames(df.h.2)[colnames(df.h.2) == 'h_tu_a_other'] <- 'h_pl_a_tu_other'

colnames(df.h.2)[colnames(df.h.2) == 'h_tf_a_other'] <- 'h_pl_a_tf_other'
#NOTE: I think this should be h_pl_a_tf_other NOT ts


##### recode #####
#Recode h_dw_e_wt
# table(df.h.2$h_dw_e_wt)

df.h.2$h_dw_e_wt[df.h.2$h_dw_e_wt ==  6] <- 4         #not applicable
df.h.2$h_dw_e_wt[df.h.2$h_dw_e_wt ==  5] <- 3         #do not know
df.h.2$h_dw_e_wt[df.h.2$h_dw_e_wt ==  0] <- 2         #no

#Recode h_p_e
df.h.2$h_p_e[df.h.2$h_p_e ==  6] <- 4         #not applicable
df.h.2$h_p_e[df.h.2$h_p_e ==  5] <- 3         #do not know
df.h.2$h_p_e[df.h.2$h_p_e ==  0] <- 2         #no

#Recode h_pl_a_th
df.h.2$h_pl_a_th[df.h.2$h_pl_a_th ==  6] <- 4         #not applicable
df.h.2$h_pl_a_th[df.h.2$h_pl_a_th ==  5] <- 3         #do not know
df.h.2$h_pl_a_th[df.h.2$h_pl_a_th ==  0] <- 2         #no

#Recode h_pl_a_tu
df.h.2$h_pl_a_tu[df.h.2$h_pl_a_tu ==  6] <- 4         #not applicable
df.h.2$h_pl_a_tu[df.h.2$h_pl_a_tu ==  5] <- 3         #do not know
df.h.2$h_pl_a_tu[df.h.2$h_pl_a_tu ==  0] <- 2         #no

#Recode h_pl_a_tw
df.h.2$h_pl_a_tw[df.h.2$h_pl_a_tw ==  6] <- 4         #not applicable
df.h.2$h_pl_a_tw[df.h.2$h_pl_a_tw ==  5] <- 3         #do not know
df.h.2$h_pl_a_tw[df.h.2$h_pl_a_tw ==  0] <- 2         #no   

#Recode h_pl_a_tf
df.h.2$h_pl_a_tf[df.h.2$h_pl_a_tf ==  6] <- 4         #not applicable
df.h.2$h_pl_a_tf[df.h.2$h_pl_a_tf ==  5] <- 3         #do not know
df.h.2$h_pl_a_tf[df.h.2$h_pl_a_tf ==  0] <- 2         #no

#Recode h_neighborhood
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 11] <- 1
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 22] <- 2
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 33] <- 3
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 44] <- 4
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 55] <- 5
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 66] <- 6
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 77] <- 7
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 88] <- 8
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 99] <- 9
df.h.2$h_neighborhood[df.h.2$h_neighborhood == 0] <- 10

#fix population
df.h.2$h_population[df.h.2$h_population ==  173] <- 17        

##### create new variables #####
# Create country or deployment code
df.h.2$dply_num <- 2

# Create unique ID variable
df.h.2$h_UID <- NA

df.h.2$h_UID <- paste0(sprintf("%02d", df.h.2$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.h.2$h_id)))

# Create date variable
df.h.2$h_date <- NA
df.h.2$h_date <- substr(df.h.2$h_start, 1, 10)
df.h.2$h_date <- as.Date(df.h.2$h_date, format = "%Y-%m-%d")

# Create floodwater time interval variable
df.h.2$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly

##### deployment-specific info #####
# Remove extra info from DF2
extra2 <- which(colnames(df.h.2) %in% setdiff(colnames(df.h.2), colnames(df.h.0)))

df.h.2.clean <- df.h.2[, -extra2]

# export extra data
df.h.2.extra <- df.h.2[, extra2]
df.h.2.extra$h_UID <- df.h.2$h_UID

##### merge datasets #####
# Adjust column type
df.h.2.clean$h_enum <- as.character(df.h.2.clean$h_enum)

# Check that dataframes have same dimensions
setdiff(colnames(df.h.2.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.2.clean)

# **********************************************************************************
#---- 3 GHANA ----

##### rename variables #####

colnames(df.h.3)[colnames(df.h.3) == 'form.HouseholdID'] <- 'h_id'
colnames(df.h.3)[colnames(df.h.3) == 'started_time'] <- 'h_start'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_gps'] <- 'h_coordinates'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_lat'] <- 'X_h_coordinates_latitude'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_lon'] <- 'X_h_coordinates_longitude'
# colnames(df.h.3)[colnames(df.h.3) == 'form.neighbor'] <- 'h_neighborhood'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q0'] <- 'h_home_type'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_house_num'] <- 'h_population'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q1'] <- 'h_c'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q4'] <- 'h_s_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q4_other'] <- 'h_s_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q5'] <- 'h_s_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q5_other'] <- 'h_s_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q6'] <- 'h_d_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q6_other'] <- 'h_d_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q7'] <- 'h_d_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q7_other'] <- 'h_d_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q8'] <- 'h_f_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q8_other'] <- 'h_f_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q9'] <- 'h_f_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q9_other'] <- 'h_f_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q10'] <- 'h_dw_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q10_other'] <- 'h_dw_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q11'] <- 'h_dw_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q11_other'] <- 'h_dw_c_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q12'] <- 'h_dw_e_wt'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q12_other'] <- 'h_dw_e_wt_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q2'] <- 'h_o_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q2_other'] <- 'h_o_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q3'] <- 'h_o_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q3_other'] <- 'h_o_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q13'] <- 'h_p_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q13_other'] <- 'h_p_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q14'] <- 'h_p_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q14_other'] <- 'h_p_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q15'] <- 'h_l_a'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q15_other'] <- 'h_l_a_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q16'] <- 'h_l_c'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q16_other'] <- 'h_l_c_other'

colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17'] <- 'h_pl_a_th'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17_other'] <- 'h_pl_a_th_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17a'] <- 'h_pl_a_tu'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17a_other'] <- 'h_pl_a_tu_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17b'] <- 'h_pl_a_tw'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17b_other'] <- 'h_pl_a_tw_other'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_q17c'] <- 'h_pl_a_ts'

colnames(df.h.3)[colnames(df.h.3) == 'completed_time'] <- 'h_end'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_notes'] <- 'h_notes'
colnames(df.h.3)[colnames(df.h.3) == 'form.hh_enumerator_names'] <- 'h_enum'
colnames(df.h.3)[colnames(df.h.3) == 'received_on'] <- 'X_submission_time'

#check for differences (returns those in x but not y)
# setdiff(colnames(df.h.3), colnames(df.h.0))

##### recode #####
# table(df.h.3$h_home_type)

df.h.3$h_s_a[df.h.3$h_home_type ==  0] <- 1

table(df.h.3$h_s_a)

# Recode h_s_a
df.h.3$h_s_a[df.h.3$h_s_a ==  9999] <- 6

# Recode h_s_c
df.h.3$h_s_c[df.h.3$h_s_c ==  9999] <- 6

# Recode h_d_a
df.h.3$h_d_a[df.h.3$h_d_a ==  9999] <- 6

# Recode h_d_c
df.h.3$h_d_c[df.h.3$h_d_c ==  9999] <- 6

# Recode h_f_a
df.h.3$h_f_a[df.h.3$h_f_a ==  9999] <- 6

# Recode h_f_c
df.h.3$h_f_c[df.h.3$h_f_c ==  9999] <- 6

# Recode h_dw_a
df.h.3$h_dw_a[df.h.3$h_dw_a ==  9999] <- 6

# Recode h_dw_c
df.h.3$h_dw_c[df.h.3$h_dw_c ==  9999] <- 6

# Recode h_dw_e_wt
df.h.3$h_dw_e_wt[df.h.3$h_dw_e_wt ==  9999] <- 6

# Recode h_o_a
df.h.3$h_o_a[df.h.3$h_o_a ==  9999] <- 6

# Recode h_o_c
df.h.3$h_o_c[df.h.3$h_o_c ==  9999] <- 6

# Recode h_p_a
df.h.3$h_p_a[df.h.3$h_p_a ==  9999] <- 6

# Recode h_p_c
df.h.3$h_p_c[df.h.3$h_p_c ==  9999] <- 6

# Recode h_l_a
df.h.3$h_l_a[df.h.3$h_l_a ==  9999] <- 6

# Recode h_l_c
df.h.3$h_l_c[df.h.3$h_l_c ==  9999] <- 6

# Recode h_pl_a_th
df.h.3$h_pl_a_th[df.h.3$h_pl_a_th ==  0] <- 2
df.h.3$h_pl_a_th[df.h.3$h_pl_a_th ==  9999] <- 4

# Recode h_pl_a_tu
df.h.3$h_pl_a_tu[df.h.3$h_pl_a_tu ==  0] <- 2
df.h.3$h_pl_a_tu[df.h.3$h_pl_a_tu ==  9999] <- 4

# Recode h_pl_a_tw
df.h.3$h_pl_a_tw[df.h.3$h_pl_a_tw ==  0] <- 2
df.h.3$h_pl_a_tw[df.h.3$h_pl_a_tw ==  9999] <- 4


table(df.h.1$h_pl_a_ts)
table(df.h.3$h_pl_a_ts)

df.h.3$h_pl_a_ts <- case_when(df.h.3$h_pl_a_ts >= 1 & df.h.3$h_pl_a_ts <= 5 ~ 1,
                              df.h.3$h_pl_a_ts > 5 ~ 2,
                              df.h.3$h_pl_a_ts == 0 ~ 3,
                              is.na(df.h.3$h_pl_a_ts) ~ 5)

df.h.3$h_dw_e_wt[df.h.3$h_dw_e_wt ==  6] <- 4         #not applicable
df.h.3$h_dw_e_wt[df.h.3$h_dw_e_wt ==  5] <- 3         #do not know
df.h.3$h_dw_e_wt[df.h.3$h_dw_e_wt ==  0] <- 2         #no

##### create new variables #####
# Create country or deployment code
df.h.3$dply_num <- 3

# Rename number variable
df.h.3$number <- 1:nrow(df.h.3)

# Rename number variable
df.h.3$h_id <- sprintf("%04d", df.h.3$number)


# Create unique ID variable
df.h.3$h_UID <- NA

df.h.3$h_UID <- paste0(sprintf("%02d", df.h.3$dply_num),
                       "_",
                       df.h.3$h_id)

# Create separate location variables using form.hh_gps (now h_coordinates)
# But only keep the altitude and precision???
df.h.3 <- separate(df.h.3, h_coordinates, c("X_h_location_latitude","X_h_location_longitude",
                                            "X_h_location_altitude","X_h_location_precision"), 
                   sep=" ", remove = FALSE)

# Create floodwater time interval variable
df.h.3$h_f_weekly <- 0    # 1 if weekly, 0 if not weekly

##### deployment-specific info #####
# exclude test data (numeric neighborhood)
# Recode h_neighborhood
table(df.h.3$form.neighbor)
# df.h.3$h_neighborhood[df.h.3$h_neighborhood ==  old code] <- new code

df.h.3 <- filter(df.h.3, form.neighbor != 1 & form.neighbor !=2 & form.neighbor !=3 & form.neighbor !=4)

# Code h_neighborhood as numeric
neigh <- meta_neighb[meta_neighb$deployment_id == 3 ,][,c("neighborhood_id", "neighborhood")]
colnames(neigh) <- c("h_neighborhood", "h_neigh")

df.h.3 <- left_join(df.h.3, neigh, by=c("form.neighbor" = "h_neigh"))
table(df.h.3$h_neighborhood)


# Create date variable
df.h.3$h_date <- substr(df.h.3$form.hh_dd, 1, 10)
df.h.3$h_date <- as.Date(df.h.3$h_date, format = "%Y-%m-%d")


# Remove extra info from DF3
extra3 <- which(colnames(df.h.3) %in% setdiff(colnames(df.h.3), colnames(df.h.0)))

df.h.3.clean <- df.h.3[, -extra3]

# export extra data
df.h.3.extra <- df.h.3[, extra3]
df.h.3.extra$h_UID <- df.h.3$h_UID

##### merge datasets #####
# Adjust column type
# df.h$h_neighborhood <- as.character(df.h$h_neighborhood)
df.h.3.clean$h_id <- as.integer(df.h.3.clean$h_id)

# Check that dataframes have same dimensions
setdiff(colnames(df.h.3.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.3.clean)

# **********************************************************************************
#---- 4 ZAMBIA ----
setdiff(colnames(df.h.4), colnames(df.h.0))


##### rename variables #####
colnames(df.h.4)[colnames(df.h.4) == 'h_dwbore_a'] <- 'h_odw_a'
colnames(df.h.4)[colnames(df.h.4) == 'h_dwbore_a_other'] <- 'h_odw_a_other'
colnames(df.h.4)[colnames(df.h.4) == 'h_dwbore_c'] <- 'h_odw_c'
colnames(df.h.4)[colnames(df.h.4) == 'h_dwbore_c_other'] <- 'h_odw_c_other'

##### recode #####
#indicate extra data
df.h.4$h_extra_data <- 1

##### Q: Which should be marked? #####

##### create new variables #####
# Create country or deployment code
df.h.4$dply_num <- 4

# Create floodwater time interval variable
df.h.4$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
##### NOTE: many of the numbers include an S--typo? #####
df.h.4$h_id <- (1000:(1000+nrow(df.h.4)-1))

# Create unique ID variable
df.h.4$h_UID <- NA

# paste number together
df.h.4$h_UID <- paste0(sprintf("%02d", df.h.4$dply_num),
                       "_",
                       df.h.4$h_id)

# Create date variable
df.h.4$h_date <- NA
df.h.4$h_date <- substr(df.h.4$h_start, 1, 10)
df.h.4$h_date <- as.Date(df.h.4$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# Remove extra info from DF4 
extra4 <- which(colnames(df.h.4) %in% setdiff(colnames(df.h.4), colnames(df.h.0))) 

# remove "extra" columns from DF3
df.h.4.clean <- df.h.4[, -extra4]

# export extra data
df.h.4.extra <- df.h.4[, extra4]
df.h.4.extra$h_UID <- df.h.4$h_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.h.4.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.4.clean)

#---- 5 MOZOAMBIQUE ----
# setdiff(colnames(df.h.5), colnames(df.h.0))

##### rename variables #####
colnames(df.h.5)[colnames(df.h.5) == 'HouseholdID'] <- 'h_id'
# colnames(df.h.5)[colnames(df.h.5) == 'dd'] <- 'h_start'
colnames(df.h.5)[colnames(df.h.5) == 'lat'] <- 'X_h_coordinates_latitude'
colnames(df.h.5)[colnames(df.h.5) == 'long'] <- 'X_h_coordinates_longitude'
colnames(df.h.5)[colnames(df.h.5) == 'neighbor'] <- 'h_neighborhood'
colnames(df.h.5)[colnames(df.h.5) == 'hh_q1'] <- 'h_c'
colnames(df.h.5)[colnames(df.h.5) == 'A_od'] <- 'h_s_a'
colnames(df.h.5)[colnames(df.h.5) == 'C_od'] <- 'h_s_c'
colnames(df.h.5)[colnames(df.h.5) == 'A_cf_rs'] <- 'h_f_a'
colnames(df.h.5)[colnames(df.h.5) == 'C_cf_rs'] <- 'h_f_c'
colnames(df.h.5)[colnames(df.h.5) == 'A_use_l'] <- 'h_l_a'
colnames(df.h.5)[colnames(df.h.5) == 'C_use_l'] <- 'h_l_c'
colnames(df.h.5)[colnames(df.h.5) == 'has_l'] <- 'h_pl_a_th'
colnames(df.h.5)[colnames(df.h.5) == 'hh_q9'] <- 'h_pl_a_ts'
colnames(df.h.5)[colnames(df.h.5) == 'no_la_b'] <- 'h_pl_a_od.1'
colnames(df.h.5)[colnames(df.h.5) == 'no_la_c'] <- 'h_pl_a_od.2'
colnames(df.h.5)[colnames(df.h.5) == 'no_la_d'] <- 'h_pl_a_od.3'
colnames(df.h.5)[colnames(df.h.5) == 'no_la_e'] <- 'h_pl_a_od.4'
colnames(df.h.5)[colnames(df.h.5) == 'no_la_a'] <- 'h_pl_a_od.5'
colnames(df.h.5)[colnames(df.h.5) == 'collectr'] <- 'h_enum'


df.h.5$dd <- as.Date(df.h.5$dd, "%m/%d/%Y")
df.h.5$th <- format(strptime(df.h.5$th, "%I:%M:%S %p"), "%H:%M:%S") 


df.h.5$h_start_dt <- paste0(df.h.5$dd,
                                "T",
                              df.h.5$th)

##### recode #####
# table(df.h.5$h_f_a)
# table(df.h.5$h_l_a)

df.h.5$h_s_a[is.na(df.h.5$h_s_a)] <- 6
df.h.5$h_s_c[is.na(df.h.5$h_s_c)] <- 6
df.h.5$h_f_a[is.na(df.h.5$h_f_a)] <- 6
df.h.5$h_f_c[is.na(df.h.5$h_f_c)] <- 6
df.h.5$h_l_a[is.na(df.h.5$h_l_a)] <- 6
df.h.5$h_l_c[is.na(df.h.5$h_l_c)] <- 6

df.h.5$h_pl_a_th <- case_when(
        df.h.5$h_pl_a_th == 1 ~ 1,
        df.h.5$h_pl_a_th == 0 ~ 2,
        df.h.5$h_pl_a_th == 2 ~ 3,
        is.na(df.h.5$h_pl_a_th) ~ 4)

# from integer to multiple choice
df.h.5$h_pl_a_ts <- case_when(df.h.5$h_pl_a_ts >= 1 & df.h.5$h_pl_a_ts <= 5 ~ 1,
                              df.h.5$h_pl_a_ts > 5 ~ 2,
                              df.h.5$h_pl_a_ts == 0 ~ 3,
                              is.na(df.h.5$h_pl_a_ts) ~ 5) #%>% table()

df.h.5$h_pl_a_od.6 <- 0
df.h.5$h_pl_a_od.7 <- 0


##### create new variables #####
# Create country or deployment code
df.h.5$dply_num <- 5

# Create unique ID variable
df.h.5$h_UID <- NA

df.h.5$h_id <- 1000:(1000+nrow(df.h.5)-1)
df.h.5$h_id_val <- df.h.5$h_id

# paste number together
df.h.5$h_UID <- paste0(sprintf("%02d", df.h.5$dply_num),
                       "_", 
                       sprintf("%04d", df.h.5$h_id))

# Create date variable
df.h.5$h_date <- df.h.5$dd
df.h.5$h_date <- as.Date(df.h.5$h_date, format = "%Y-%m-%d")



##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra5 <- which(colnames(df.h.5) %in% setdiff(colnames(df.h.5), colnames(df.h.0)))

# remove "extra" columns from df4
df.h.5.clean <- df.h.5[, -extra5]

# export extra data
df.h.5.extra <- df.h.5[, extra5]
df.h.5.extra$h_UID <- df.h.5$h_UID

##### merge datasets #####
# setdiff(colnames(df.h.5.clean), colnames(df.h.0))

df.h <- bind_rows(df.h, df.h.5.clean)

# **********************************************************************************

#---- 6 GHANA Accra -----
setdiff(colnames(df.h.6), colnames(df.h.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.h.6$h_extra_data <- 1

df.h.6$h_pl_a_od.1 <- as.numeric(as.logical(df.h.6$h_pl_a_od.1))
df.h.6$h_pl_a_od.2 <- as.numeric(as.logical(df.h.6$h_pl_a_od.2))
df.h.6$h_pl_a_od.3 <- as.numeric(as.logical(df.h.6$h_pl_a_od.3))
df.h.6$h_pl_a_od.4 <- as.numeric(as.logical(df.h.6$h_pl_a_od.4))
df.h.6$h_pl_a_od.5 <- as.numeric(as.logical(df.h.6$h_pl_a_od.5))
df.h.6$h_pl_a_od.6 <- as.numeric(as.logical(df.h.6$h_pl_a_od.6))
df.h.6$h_pl_a_od.7 <- as.numeric(as.logical(df.h.6$h_pl_a_od.7))

##### create new variables #####
# Create country or deployment code
df.h.6$dply_num <- 6

# Create floodwater time interval variable
# df.h.6$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
##### NOTE: many of the numbers include an S--typo? #####
df.h.6$h_id <- (1000:(1000+nrow(df.h.6)-1))

# Create unique ID variable
df.h.6$h_UID <- NA

# paste number together
df.h.6$h_UID <- paste0(sprintf("%02d", df.h.6$dply_num),
                       "_",
                       df.h.6$h_id)

# Create date variable
df.h.6$h_date <- NA
df.h.6$h_date <- substr(df.h.6$h_start, 1, 10)
df.h.6$h_date <- as.Date(df.h.6$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
df.h.6$h_neighborhood[df.h.6$h_neighborhood == 268] <- 1
df.h.6$h_neighborhood[df.h.6$h_neighborhood == 269] <- 2

# Remove extra info from DF4 
extra6 <- which(colnames(df.h.6) %in% setdiff(colnames(df.h.6), colnames(df.h.0))) 

# remove "extra" columns from DF3
df.h.6.clean <- df.h.6[, -extra6]

# export extra data
df.h.6.extra <- df.h.6[, extra6]
df.h.6.extra$h_UID <- df.h.6$h_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.h.6.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.6.clean)

# **********************************************************************************
#---- 7 GHANA Kumasi -----
setdiff(colnames(df.h.7), colnames(df.h.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.h.7$h_extra_data <- 1

df.h.7$h_pl_a_od.1 <- as.numeric(as.logical(df.h.7$h_pl_a_od.1))
df.h.7$h_pl_a_od.2 <- as.numeric(as.logical(df.h.7$h_pl_a_od.2))
df.h.7$h_pl_a_od.3 <- as.numeric(as.logical(df.h.7$h_pl_a_od.3))
df.h.7$h_pl_a_od.4 <- as.numeric(as.logical(df.h.7$h_pl_a_od.4))
df.h.7$h_pl_a_od.5 <- as.numeric(as.logical(df.h.7$h_pl_a_od.5))
df.h.7$h_pl_a_od.6 <- as.numeric(as.logical(df.h.7$h_pl_a_od.6))
df.h.7$h_pl_a_od.7 <- as.numeric(as.logical(df.h.7$h_pl_a_od.7))

##### create new variables #####
# Create country or deployment code
df.h.7$dply_num <- 7

# Create floodwater time interval variable
# df.h.7$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
##### NOTE: many of the numbers include an S--typo? #####
df.h.7$h_id <- (1000:(1000+nrow(df.h.7)-1))

# Create unique ID variable
df.h.7$h_UID <- NA

# paste number together
df.h.7$h_UID <- paste0(sprintf("%02d", df.h.7$dply_num),
                       "_",
                       df.h.7$h_id)

# Create date variable
df.h.7$h_date <- NA
df.h.7$h_date <- substr(df.h.7$h_start, 1, 10)
df.h.7$h_date <- as.Date(df.h.7$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
df.h.7$h_neighborhood[df.h.7$h_neighborhood == 280] <- 1
df.h.7$h_neighborhood[df.h.7$h_neighborhood == 281] <- 2
df.h.7$h_neighborhood[df.h.7$h_neighborhood == 282] <- 3
df.h.7$h_neighborhood[df.h.7$h_neighborhood == 283] <- 4
# Remove extra info from DF4 
extra7 <- which(colnames(df.h.7) %in% setdiff(colnames(df.h.7), colnames(df.h.0))) 

# remove "extra" columns from DF
df.h.7.clean <- df.h.7[, -extra7]

# export extra data
df.h.7.extra <- df.h.7[, extra7]
df.h.7.extra$h_UID <- df.h.7$h_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.h.7.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.7.clean)
# **********************************************************************************

#---- 8 MOZOAMBIQUE ----
setdiff(colnames(df.h.8), colnames(df.h.0))

##### rename variables #####
colnames(df.h.8)[colnames(df.h.8) == 'HouseholdID'] <- 'h_id'
# colnames(df.h.8)[colnames(df.h.8) == 'hh_dd'] <- 'h_start' #date
# colnames(df.h.8)[colnames(df.h.8) == 'hh_th'] <- 'h_start' #time
colnames(df.h.8)[colnames(df.h.8) == 'neighbor'] <- 'h_neighborhood'
colnames(df.h.8)[colnames(df.h.8) == 'hh_lat'] <- 'X_h_coordinates_latitude'
colnames(df.h.8)[colnames(df.h.8) == 'hh_long'] <- 'X_h_coordinates_longitude'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q1'] <- 'h_c'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q2'] <- 'h_d_a'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q3'] <- 'h_d_c'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q4'] <- 'h_f_a'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q5'] <- 'h_f_c'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q11'] <- 'h_dw_a'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q12'] <- 'h_dw_c'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q15'] <- 'h_dw_e_wt'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q13'] <- 'h_p_a'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q14'] <- 'h_p_c'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q6'] <- 'h_pl_a_th'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q7'] <- 'h_pl_a_tu'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q9'] <- 'h_pl_a_ts'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q10a'] <- 'h_pl_a_od.5'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q10b'] <- 'h_pl_a_od.1'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q10c'] <- 'h_pl_a_od.2'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q10d'] <- 'h_pl_a_od.3'
colnames(df.h.8)[colnames(df.h.8) == 'hh_q10e'] <- 'h_pl_a_od.4'
colnames(df.h.8)[colnames(df.h.8) == 'collectr'] <- 'h_enum'
# colnames(df.h.8)[colnames(df.h.8) == 'hh_q8'] <- '' #children use latrine

df.h.8$hh_dd <- as.Date(df.h.8$hh_dd, "%m/%d/%Y")
df.h.8$hh_th <- format(strptime(df.h.8$hh_th, "%I:%M:%S %p"), "%H:%M:%S") 


df.h.8$h_start <- paste0(df.h.8$hh_dd,
                            "T",
                            df.h.8$hh_th)


colnames(df.h.8c)
colnames(df.h.8c)[colnames(df.h.8c) == 'HouseholdID'] <- 'h_id'
# colnames(df.h.8c)[colnames(df.h.8c) == 'hh_dd'] <- 'h_start' #date
# colnames(df.h.8c)[colnames(df.h.8c) == 'hh_th'] <- 'h_start' #time
colnames(df.h.8c)[colnames(df.h.8c) == 'neighbor'] <- 'h_neighborhood'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_lat'] <- 'X_h_coordinates_latitude'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_long'] <- 'X_h_coordinates_longitude'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q1'] <- 'h_d_c'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q2'] <- 'h_d_a'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q3'] <- 'h_f_c'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q4'] <- 'h_f_a'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q9'] <- 'h_dw_c'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q10'] <- 'h_dw_a'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q13'] <- 'h_dw_e_wt'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q11'] <- 'h_p_c'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q12'] <- 'h_p_a'
# colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q5'] <- 'h_f_c'
# colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q6'] <- 'h_pl_a_tu' #children
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q7'] <- 'h_pl_a_tu'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q8a'] <- 'h_pl_a_od.5'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q8b'] <- 'h_pl_a_od.1'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q8c'] <- 'h_pl_a_od.2'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q8d'] <- 'h_pl_a_od.3'
colnames(df.h.8c)[colnames(df.h.8c) == 'ch_q8e'] <- 'h_pl_a_od.4'

df.h.8c$hh_dd <- as.Date(df.h.8c$ch_dd, "%m/%d/%Y")
df.h.8c$hh_th <- format(strptime(df.h.8c$ch_th, "%I:%M:%S %p"), "%H:%M:%S") 


df.h.8c$h_start <- paste0(df.h.8c$hh_dd,
                         "T",
                         df.h.8c$hh_th)

df.h.8c$h_id <- df.h.8c$h_id+99

df.h.8 <- bind_rows(df.h.8, df.h.8c)
##### recode #####


#use
df.h.8$h_pl_a_tu <- case_when(
        df.h.8$h_pl_a_tu == 1 ~ 1,
        df.h.8$h_pl_a_tu == 2 ~ 1,
        df.h.8$h_pl_a_tu == 3 ~ 1,
        df.h.8$h_pl_a_tu == 4 ~ 2,
        df.h.8$h_pl_a_tu == 5 ~ NA_real_)

# from integer to multiple choice
df.h.8$h_pl_a_ts <- case_when(df.h.8$h_pl_a_ts >= 1 & df.h.8$h_pl_a_ts <= 5 ~ 1,
                              df.h.8$h_pl_a_ts > 5 ~ 2,
                              df.h.8$h_pl_a_ts == 0 ~ 3,
                              is.na(df.h.8$h_pl_a_ts) ~ 5) #%>% table()
# logic to 1 and 0
df.h.8$h_pl_a_od.1 <- df.h.8$h_pl_a_od.1*1
df.h.8$h_pl_a_od.2 <- df.h.8$h_pl_a_od.2*1
df.h.8$h_pl_a_od.3 <- df.h.8$h_pl_a_od.3*1
df.h.8$h_pl_a_od.4 <- df.h.8$h_pl_a_od.4*1
df.h.8$h_pl_a_od.5 <- df.h.8$h_pl_a_od.5*1
df.h.8$h_pl_a_od.6 <- NA
df.h.8$h_pl_a_od.7 <- NA


##### create new variables #####
# Create country or deployment code
df.h.8$dply_num <- 8

# Create unique ID variable
df.h.8$h_UID <- NA

df.h.8$h_id <- 1000:(1000+nrow(df.h.8)-1)
df.h.8$h_id_val <- df.h.8$h_id

# paste number together
df.h.8$h_UID <- paste0(sprintf("%02d", df.h.8$dply_num),
                       "_", 
                       sprintf("%04d", df.h.8$h_id))

# Create date variable
df.h.8$h_date <- df.h.8$hh_dd
df.h.8$h_date <- as.Date(df.h.8$h_date, format = "%Y-%m-%d")



##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra8 <- which(colnames(df.h.8) %in% setdiff(colnames(df.h.8), colnames(df.h.0)))

# remove "extra" columns from df
df.h.8.clean <- df.h.8[, -extra8]

# export extra data
df.h.8.extra <- df.h.8[, extra8]
df.h.8.extra$h_UID <- df.h.8$h_UID

##### merge datasets #####
# setdiff(colnames(df.h.8.clean), colnames(df.h.0))

df.h <- bind_rows(df.h, df.h.8.clean)

# **********************************************************************************


# **********************************************************************************
#---- 9 UGANDA Kampala -----
setdiff(colnames(df.h.9), colnames(df.h.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.h.9$h_extra_data <- 1

df.h.9$h_pl_a_od.1 <- as.numeric(as.logical(df.h.9$h_pl_a_od.1))
df.h.9$h_pl_a_od.2 <- as.numeric(as.logical(df.h.9$h_pl_a_od.2))
df.h.9$h_pl_a_od.3 <- as.numeric(as.logical(df.h.9$h_pl_a_od.3))
df.h.9$h_pl_a_od.4 <- as.numeric(as.logical(df.h.9$h_pl_a_od.4))
df.h.9$h_pl_a_od.5 <- as.numeric(as.logical(df.h.9$h_pl_a_od.5))
df.h.9$h_pl_a_od.6 <- as.numeric(as.logical(df.h.9$h_pl_a_od.6))
df.h.9$h_pl_a_od.7 <- as.numeric(as.logical(df.h.9$h_pl_a_od.7))

##### create new variables #####
# Create country or deployment code
df.h.9$dply_num <- 9

# Create floodwater time interval variable
# df.h.9$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
##### NOTE: many of the numbers include an S--typo? #####
df.h.9$h_id <- (1000:(1000+nrow(df.h.9)-1))

# Create unique ID variable
df.h.9$h_UID <- NA

# paste number together
df.h.9$h_UID <- paste0(sprintf("%02d", df.h.9$dply_num),
                       "_",
                       df.h.9$h_id)

# Create date variable
df.h.9$h_date <- NA
df.h.9$h_date <- substr(df.h.9$h_start, 1, 10)
df.h.9$h_date <- as.Date(df.h.9$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
table(df.h.9$h_neighborhood)

df.h.9$h_neighborhood[df.h.9$h_neighborhood == 309] <- 1
df.h.9$h_neighborhood[df.h.9$h_neighborhood == 310] <- 2
df.h.9$h_neighborhood[df.h.9$h_neighborhood == 311] <- 3
df.h.9$h_neighborhood[df.h.9$h_neighborhood == 312] <- 4
df.h.9$h_neighborhood[df.h.9$h_neighborhood == 313] <- 5

# Remove extra info from DF
extra9 <- which(colnames(df.h.9) %in% setdiff(colnames(df.h.9), colnames(df.h.0))) 

# remove "extra" columns from DF
df.h.9.clean <- df.h.9[, -extra9]

# export extra data
df.h.9.extra <- df.h.9[, extra9]
df.h.9.extra$h_UID <- df.h.9$h_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.h.9.clean), colnames(df.h))

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.9.clean)
# **********************************************************************************

# **********************************************************************************

#---- 10 USA ----
setdiff(colnames(df.h.10), colnames(df.h.0))


##### rename variables #####
colnames(df.h.10)[colnames(df.h.10) == 'record_id'] <- 'h_id'
colnames(df.h.10)[colnames(df.h.10) == 'my_first_instrument_timestamp'] <- 'date'
colnames(df.h.10)[colnames(df.h.10) == 'neighbor'] <- 'h_neighborhood'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q15'] <- 'h_population'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q1'] <- 'h_c'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q2'] <- 'h_s_a'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q3'] <- 'h_s_c'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q4'] <- 'h_f_a'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q5'] <- 'h_f_c'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q6'] <- 'h_dw_a'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q7'] <- 'h_dw_c'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q8'] <- 'h_p_a'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q9'] <- 'h_p_c'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q10'] <- 'h_l_a'
colnames(df.h.10)[colnames(df.h.10) == 'hh_q11'] <- 'h_l_c'



df.h.10$dd <- as.Date(df.h.10$date, "%m/%d/%Y")
df.h.10$th <- format(strptime(df.h.10$time, "%I:%M:%S %p"), "%H:%M:%S") 


df.h.10$h_start_dt <- paste0(df.h.10$dd,
                             "T",
                             df.h.10$th)

##### recode #####
table(df.h.10$h_population)

df.h.10$h_population <- case_when(
        df.h.10$h_population == 1 ~ 2,
        df.h.10$h_population == 2 ~ 4,
        df.h.10$h_population == 3 ~ 7,
        df.h.10$h_population == 4 ~ 9)



##### create new variables #####
# Create country or deployment code
df.h.10$dply_num <- 10

df.h.10$h_extra_data <- 1

# Create unique ID variable
df.h.10$h_UID <- NA

df.h.10$h_id <- 1000:(1000+nrow(df.h.10)-1)
df.h.10$h_id_val <- df.h.10$h_id

# paste number together
df.h.10$h_UID <- paste0(sprintf("%02d", df.h.10$dply_num),
                        "_", 
                        sprintf("%04d", df.h.10$h_id))

# Create date variable
df.h.10$h_date <- df.h.10$dd
df.h.10$h_date <- as.Date(df.h.10$h_date, format = "%Y-%m-%d")



##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra10 <- which(colnames(df.h.10) %in% setdiff(colnames(df.h.10), colnames(df.h.0)))

# remove "extra" columns from df
df.h.10.clean <- df.h.10[, -extra10]

# export extra data
df.h.10.extra <- df.h.10[, extra10]
df.h.10.extra$h_UID <- df.h.10$h_UID

##### merge datasets #####
# setdiff(colnames(df.h.10.clean), colnames(df.h.0))

df.h <- bind_rows(df.h, df.h.10.clean)
# **********************************************************************************


# **********************************************************************************
#---- 11 India Vellore ----
setdiff(colnames(df.h.11), colnames(df.h.0))

table(df.h.11$h_pl_a_th)
##### rename variables #####
colnames(df.h.11)[colnames(df.h.11) == 'response_id'] <- 'h_id'
colnames(df.h.11)[colnames(df.h.11) == 'neighborhood_id'] <- 'h_neighborhood'

colnames(df.h.11)[colnames(df.h.11) == 'response_date'] <- 'h_date'
colnames(df.h.11)[colnames(df.h.11) == 'GPS_latitude'] <- 'X_h_coordinates_latitude'
colnames(df.h.11)[colnames(df.h.11) == 'GPS_longitude'] <- 'X_h_coordinates_longitude'


df.h.11$h_date <- as.Date(df.h.11$h_date, "%Y-%m-%d")

##### recode #####
df.h.11$h_pl_a_th[df.h.11$h_pl_a_th == 0] <- 2
df.h.11$h_pl_a_tu[df.h.11$h_pl_a_tu == 0] <- 2
df.h.11$h_pl_a_tw[df.h.11$h_pl_a_tw == 0] <- 2
df.h.11$h_dw_e_wt[df.h.11$h_dw_e_wt == 0] <- 2
df.h.11$h_pl_a_tf[df.h.11$h_pl_a_tf == 0] <- 2

df.h.11$h_pl_a_od.1[df.h.11$h_pl_a_od.1 == 1] <- 1
df.h.11$h_pl_a_od.1[df.h.11$h_pl_a_od.2 == 2] <- 1
df.h.11$h_pl_a_od.1[df.h.11$h_pl_a_od.3 == 3] <- 1
df.h.11$h_pl_a_od.1[df.h.11$h_pl_a_od.4 == 4] <- 1
df.h.11$h_pl_a_od.1[df.h.11$h_pl_a_od.5 == 5] <- 1

##### create new variables #####
# Create country or deployment code
df.h.11$dply_num <- 11

df.h.11$h_extra_data <- 1

# Create unique ID variable
df.h.11$h_UID <- NA

df.h.11$h_id <- 1000+df.h.11$h_id
df.h.11$h_id_val <- df.h.11$h_id

# paste number together
df.h.11$h_UID <- paste0(sprintf("%02d", df.h.11$dply_num),
                        "_", 
                        sprintf("%04d", df.h.11$h_id))


##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra11 <- which(colnames(df.h.11) %in% setdiff(colnames(df.h.11), colnames(df.h.0)))

# remove "extra" columns from df
df.h.11.clean <- df.h.11[, -extra11]

# export extra data
df.h.11.extra <- df.h.11[, extra11]
df.h.11.extra$h_UID <- df.h.11$h_UID

##### merge datasets #####
# setdiff(colnames(df.h.11.clean), colnames(df.h.0))

df.h <- bind_rows(df.h, df.h.11.clean)


# # **********************************************************************************
# #---- 12 INDIA KOLKATA -----
# setdiff(colnames(df.h.99), colnames(df.h.0))
# 
# ##### rename variables #####
# 
# ##### recode #####
# #indicate extra data
# df.h.99$h_extra_data <- 1
# 
# df.h.99$h_pl_a_od.1 <- as.numeric(as.logical(df.h.99$h_pl_a_od.1))
# df.h.99$h_pl_a_od.2 <- as.numeric(as.logical(df.h.99$h_pl_a_od.2))
# df.h.99$h_pl_a_od.3 <- as.numeric(as.logical(df.h.99$h_pl_a_od.3))
# df.h.99$h_pl_a_od.4 <- as.numeric(as.logical(df.h.99$h_pl_a_od.4))
# df.h.99$h_pl_a_od.5 <- as.numeric(as.logical(df.h.99$h_pl_a_od.5))
# df.h.99$h_pl_a_od.6 <- as.numeric(as.logical(df.h.99$h_pl_a_od.6))
# df.h.99$h_pl_a_od.7 <- as.numeric(as.logical(df.h.99$h_pl_a_od.7))
# 
# ##### create new variables #####
# # Create country or deployment code
# df.h.99$dply_num <- 12
# 
# # Create floodwater time interval variable
# # df.h.99$h_f_weekly <- 1    # 1 if weekly, 0 if not weekly
# 
# # Create new h_id variable
# ##### NOTE: many of the numbers include an S--typo? #####
# # df.h.99$h_id <- (1000:(1000+nrow(df.h.99)-1))
# 
# # Create unique ID variable
# df.h.99$h_UID <- NA
# 
# # paste number together
# df.h.99$h_UID <- paste0(sprintf("%02d", df.h.99$dply_num),
#                         "_",
#                         df.h.99$h_id)
# 
# # Create date variable
# df.h.99$h_date <- NA
# df.h.99$h_date <- substr(df.h.99$h_start, 1, 10)
# df.h.99$h_date <- as.Date(df.h.99$h_date, format = "%Y-%m-%d")
# 
# ##### deployment-specific info #####
# # df.h.99$h_neighborhood[df.h.99$h_neighborhood == 280] <- 1
# 
# # Remove extra info from DF4 
# extra12 <- which(colnames(df.h.99) %in% setdiff(colnames(df.h.99), colnames(df.h.0))) 
# 
# # remove "extra" columns from DF
# df.h.99.clean <- df.h.99[, -extra99]
# 
# # export extra data
# df.h.99.extra <- df.h.99[, extra12]
# df.h.99.extra$h_UID <- df.h.99$h_UID
# 
# ##### merge datasets #####
# 
# # Check that dataframes have same dimensions
# # setdiff(colnames(df.h.99.clean), colnames(df.h))
# 
# # Merge clean datasets
# df.h <- bind_rows(df.h, df.h.99.clean)
# # **********************************************************************************



# # **********************************************************************************
#---- 12 SENEGAL Dakar ----
setdiff(colnames(df.h.12), colnames(df.h.0))


##### recode #####
#indicate extra data
df.h.12$h_extra_data <- 0

# neighborhood
df.h.12$c_neighborhood[df.h.12$c_neighborhood == 409] <- 1
df.h.12$c_neighborhood[df.h.12$c_neighborhood == 410] <- 2
df.h.12$c_neighborhood[df.h.12$c_neighborhood == 411] <- 3
df.h.12$c_neighborhood[df.h.12$c_neighborhood == 412] <- 4
df.h.12$c_neighborhood[df.h.12$c_neighborhood == 413] <- 5


df.h.12$h_pl_a_od.1 <- as.numeric(as.logical(df.h.12$h_pl_a_od.1))
df.h.12$h_pl_a_od.2 <- as.numeric(as.logical(df.h.12$h_pl_a_od.2))
df.h.12$h_pl_a_od.3 <- as.numeric(as.logical(df.h.12$h_pl_a_od.3))
df.h.12$h_pl_a_od.4 <- as.numeric(as.logical(df.h.12$h_pl_a_od.4))
df.h.12$h_pl_a_od.5 <- as.numeric(as.logical(df.h.12$h_pl_a_od.5))
df.h.12$h_pl_a_od.6 <- as.numeric(as.logical(df.h.12$h_pl_a_od.6))
df.h.12$h_pl_a_od.7 <- as.numeric(as.logical(df.h.12$h_pl_a_od.7))

##### create new variables #####
# Create country or deployment code
df.h.12$dply_num <- 12

# Create new h_id variable
df.h.12$h_id <- (1000:(1000+nrow(df.h.12)-1))

# Create unique ID variable
df.h.12$h_UID <- NA

# paste number together
df.h.12$h_UID <- paste0(sprintf("%02d", df.h.12$dply_num),
                        "_",
                        df.h.12$h_id)

# Create date variable
df.h.12$h_date <- NA
df.h.12$h_date <- substr(df.h.12$h_start, 1, 10)
df.h.12$h_date <- as.Date(df.h.12$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# Remove extra info from DF 
extra12 <- which(colnames(df.h.12) %in% setdiff(colnames(df.h.12), colnames(df.h.0))) 

# remove "extra" columns from DF3
df.h.12.clean <- df.h.12[, -extra12]

# export extra data
df.h.12.extra <- df.h.12[, extra12]
df.h.12.extra$h_UID <- df.h.12$h_UID

##### merge datasets #####

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.12.clean)
# # **********************************************************************************



# # **********************************************************************************
#---- 13 ZAMBIA 2 ----
setdiff(colnames(df.h.13), colnames(df.h.0))


##### rename variables #####
colnames(df.h.13)[colnames(df.h.13) == 'h_dwbore_a'] <- 'h_odw_a'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwbore_a_other'] <- 'h_odw_a_other'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwbore_c'] <- 'h_odw_c'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwbore_c_other'] <- 'h_odw_c_other'

colnames(df.h.13)[colnames(df.h.13) == 'h_dwwell_a'] <- 'h_odw2_a'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwwell_a_other'] <- 'h_odw2_a_other'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwwell_c'] <- 'h_odw2_c'
colnames(df.h.13)[colnames(df.h.13) == 'h_dwwell_c_other'] <- 'h_odw2_c_other'

##### recode #####
#indicate extra data
df.h.13$h_extra_data <- 1

# neighborhood
df.h.13$h_neighborhood[df.h.13$h_neighborhood == 381] <- 1
df.h.13$h_neighborhood[df.h.13$h_neighborhood == 382] <- 2
df.h.13$h_neighborhood[df.h.13$h_neighborhood == 380] <- 3

df.h.13$h_pl_a_od.1 <- as.numeric(as.logical(df.h.13$h_pl_a_od.1))
df.h.13$h_pl_a_od.2 <- as.numeric(as.logical(df.h.13$h_pl_a_od.2))
df.h.13$h_pl_a_od.3 <- as.numeric(as.logical(df.h.13$h_pl_a_od.3))
df.h.13$h_pl_a_od.4 <- as.numeric(as.logical(df.h.13$h_pl_a_od.4))
df.h.13$h_pl_a_od.5 <- as.numeric(as.logical(df.h.13$h_pl_a_od.5))
df.h.13$h_pl_a_od.6 <- as.numeric(as.logical(df.h.13$h_pl_a_od.6))
df.h.13$h_pl_a_od.7 <- as.numeric(as.logical(df.h.13$h_pl_a_od.7))

##### create new variables #####
# Create country or deployment code
df.h.13$dply_num <- 13

# Create new h_id variable
df.h.13$h_id <- (1000:(1000+nrow(df.h.13)-1))

# Create unique ID variable
df.h.13$h_UID <- NA

# paste number together
df.h.13$h_UID <- paste0(sprintf("%02d", df.h.13$dply_num),
                        "_",
                        df.h.13$h_id)

# Create date variable
df.h.13$h_date <- NA
df.h.13$h_date <- substr(df.h.13$h_start, 1, 10)
df.h.13$h_date <- as.Date(df.h.13$h_date, format = "%Y-%m-%d")

##### deployment-specific info #####
# Remove extra info from DF 
extra13 <- which(colnames(df.h.13) %in% setdiff(colnames(df.h.13), colnames(df.h.0))) 

# remove "extra" columns from DF3
df.h.13.clean <- df.h.13[, -extra13]

# export extra data
df.h.13.extra <- df.h.13[, extra13]
df.h.13.extra$h_UID <- df.h.13$h_UID

##### merge datasets #####

# Merge clean datasets
df.h <- bind_rows(df.h, df.h.13.clean)




# **********************************************************************************
# add unique neighborhood numbers (over all projects)
df.h$neighb_UID <- paste0(sprintf("%01d", df.h$dply_num),
                          sprintf("%02d", df.h$h_neighborhood))

df.h$neighb_UID <- as.numeric(df.h$neighb_UID)

df.h <- left_join(df.h, meta_dply[,c(1,4)], by = c("dply_num" = "id"))



# **********************************************************************************
#---- TEMPLATE ----

##### rename variables #####
#colnames(df.s.X)[colnames(df.s.X) == 'OLD COLUMN NAME'] <- 'NEW COLUMN NAME'

##### recode #####


##### create new variables #####


##### deployment-specific info #####


##### merge datasets #####
# **********************************************************************************
#---- 99 WRITE DATA ----

write.csv(df.h, paste0(getwd(), "/data/", "h_merged_", Sys.Date(), ".csv"), row.names = F, na="")

write.csv(df.h.1.extra, paste0(getwd(), "/data/extra_data/", "h_extra_01.csv"), row.names = F, na="")
write.csv(df.h.2.extra, paste0(getwd(), "/data/extra_data/", "h_extra_02.csv"), row.names = F, na="")
write.csv(df.h.3.extra, paste0(getwd(), "/data/extra_data/", "h_extra_03.csv"), row.names = F, na="")
write.csv(df.h.4.extra, paste0(getwd(), "/data/extra_data/", "h_extra_04.csv"), row.names = F, na="")
write.csv(df.h.5.extra, paste0(getwd(), "/data/extra_data/", "h_extra_05.csv"), row.names = F, na="")
write.csv(df.h.7.extra, paste0(getwd(), "/data/extra_data/", "h_extra_07.csv"), row.names = F, na="")
write.csv(df.h.8.extra, paste0(getwd(), "/data/extra_data/", "h_extra_08.csv"), row.names = F, na="")
write.csv(df.h.9.extra, paste0(getwd(), "/data/extra_data/", "h_extra_09.csv"), row.names = F, na="")
write.csv(df.h.10.extra, paste0(getwd(), "/data/extra_data/", "h_extra_10.csv"), row.names = F, na="")
# write.csv(df.h.99.extra, paste0(getwd(), "/data/extra_data/", "h_extra_12.csv"), row.names = F, na="")
write.csv(df.h.13.extra, paste0(getwd(), "/data/extra_data/", "h_extra_13.csv"), row.names = F, na="")

