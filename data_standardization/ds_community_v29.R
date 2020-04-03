# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# Form: community (c)
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
df.c.0 <- read.table(paste0(getwd(), "/data/deployments/00_master/", "COMMUNITY_10_20_17_WM - xml - 2017-10-31-19-13", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T)

# read bangladesh form
df.c.2 <- read.csv(paste0(getwd(), "/data/deployments/02_bangladesh/", "Community_FINAL_2017_08_29_11_45_23", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A"))

# read ghana form
df.c.3 <- read.csv(paste0(getwd(), "/data/deployments/03_ghana/", "ghana c", ".csv"), 
                   stringsAsFactors=F, header=T, na.strings = c("na", "NA", "Na", "---"))

# read zambia form
df.c.4 <- read.csv(paste0(getwd(), "/data/deployments/04_zambia/", "cc_lusaka", ".csv"), sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana accra form
df.c.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "accrametro-community-1500", ".csv"), sep=",",
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.c.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "ksisanipath-community-1619", ".csv"), sep=",",
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read uganda kampala form
df.c.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/", "cc_uganda", ".csv"), sep=",",
                   stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read vellore form
df.c.11 <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/", "community_clean", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# read senegal form
df.c.12 <- read.csv(paste0(getwd(), "/data/deployments/12_senegal/", 
                           "Community_Survey-xlsform_waspa_final_2020_02_05_17_51_45", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# # read kolkata form
# df.c.99 <- read.csv(paste0(getwd(), "/data/deployments/12_kolkata/", "spt_cc", ".csv"), 
#                     sep=",", stringsAsFactors=F, header=T, 
#                     na.strings = c("n/a", "NA", "N/A", "---"))

# read lusaka2 form
df.c.13 <- read.csv(paste0(getwd(), "/data/deployments/13_zambia2/", "Community_Survey-xlsform_lusaka2019_final_2019_11_04_18_48_42", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))
# **********************************************************************************

#---- MASTER -----
##### create new variables #####
df.c.0$c_odw_a <- NA
df.c.0$c_odw_a_note <- NA
df.c.0$c_odw_a_3 <- NA
df.c.0$c_odw_a_2 <- NA
df.c.0$c_odw_a_1 <- NA
df.c.0$c_odw_a_0 <- NA
df.c.0$c_odw_a_na <- NA
df.c.0$c_odw_a_other <- NA
df.c.0$c_odw_c <- NA
df.c.0$c_odw_c_note <- NA
df.c.0$c_odw_c_3 <- NA
df.c.0$c_odw_c_2 <- NA 
df.c.0$c_odw_c_1 <- NA
df.c.0$c_odw_c_0 <- NA
df.c.0$c_odw_c_na <- NA
df.c.0$c_odw_c_other <- NA

df.c.0$c_odw2_a <- NA
df.c.0$c_odw2_a_note <- NA
df.c.0$c_odw2_a_3 <- NA
df.c.0$c_odw2_a_2 <- NA
df.c.0$c_odw2_a_1 <- NA
df.c.0$c_odw2_a_0 <- NA
df.c.0$c_odw2_a_na <- NA
df.c.0$c_odw2_a_other <- NA
df.c.0$c_odw2_c <- NA
df.c.0$c_odw2_c_note <- NA
df.c.0$c_odw2_c_3 <- NA
df.c.0$c_odw2_c_2 <- NA 
df.c.0$c_odw2_c_1 <- NA
df.c.0$c_odw2_c_0 <- NA
df.c.0$c_odw2_c_na <- NA
df.c.0$c_odw2_c_other <- NA

df.c.0$c_extra_data <- NA
df.c.0$dply_num <- NA
df.c.0$c_f_weekly <- NA    # 1 if weekly, 0 if not weekly

df.c.0$c_UID <- NA
df.c.0$c_UID <- as.character(df.c.0$c_UID)

df.c.0$c_date <- NA
df.c.0$c_date <- as.Date(df.c.0$c_date, format = "%Y-%m-%d")

df.c.0$deviceid <- as.numeric(df.c.0$deviceid)

##### merge datasets #####
df.c.0 <- df.c.0[-c(1:1),]


# **********************************************************************************

#---- 2 BANGLADESH -----
##### rename variables #####
setdiff(colnames(df.c.2), colnames(df.c.0))

colnames(df.c.2)[colnames(df.c.2) == 'c_m_a'] <-        'c_dw_a'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_note'] <-   'c_dw_a_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_3'] <-      'c_dw_a_3'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_2'] <-      'c_dw_a_2'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_1'] <-      'c_dw_a_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_0'] <-      'c_dw_a_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_a_na'] <-     'c_dw_a_na'

colnames(df.c.2)[colnames(df.c.2) == 'c_wt'] <-         'c_dw_e_wt'
colnames(df.c.2)[colnames(df.c.2) == 'c_wt_note'] <-    'c_dw_e_wt_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_wt_1'] <-       'c_dw_e_wt_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_wt_0'] <-       'c_dw_e_wt_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_wt_na'] <-      'c_dw_e_wt_na'

colnames(df.c.2)[colnames(df.c.2) == 'c_p_a2_note'] <-  'c_p_e_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_p_a2_2'] <-     'c_p_e_2'
colnames(df.c.2)[colnames(df.c.2) == 'c_p_a2_1'] <-     'c_p_e_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_p_a2_na'] <-    'c_p_e_na'

colnames(df.c.2)[colnames(df.c.2) == 'c_th_a'] <-       'c_pl_a_th'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_note'] <-  'c_pl_a_th_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_1'] <-     'c_pl_a_th_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_0'] <-     'c_pl_a_th_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_na'] <-    'c_pl_a_th_na'
colnames(df.c.2)[colnames(df.c.2) == 'c_tu_a_note'] <-  'c_pl_a_tu_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_tu_a_1'] <-     'c_pl_a_tu_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_tu_a_0'] <-     'c_pl_a_tu_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_tu_a_na'] <-    'c_pl_a_tu_na'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_02'] <-    'c_pl_a_tu_nl'
colnames(df.c.2)[colnames(df.c.2) == 'c_tw_a_note'] <-  'c_pl_a_tw_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_tw_a_1'] <-     'c_pl_a_tw_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_tw_a_0'] <-     'c_pl_a_tw_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_tw_a_na'] <-    'c_pl_a_tw_na'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_03'] <-    'c_pl_a_tw_nl'
colnames(df.c.2)[colnames(df.c.2) == 'c_tf_a_note'] <-  'c_pl_a_tf_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_tf_a_1'] <-     'c_pl_a_tf_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_tf_a_0'] <-     'c_pl_a_tf_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_tf_a_na'] <-    'c_pl_a_tf_na'
colnames(df.c.2)[colnames(df.c.2) == 'c_th_a_04'] <-    'c_pl_a_tf_nl'

colnames(df.c.2)[colnames(df.c.2) == 'c_m_c'] <-        'c_dw_c'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_note'] <-   'c_dw_c_note'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_3'] <-      'c_dw_c_3'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_2'] <-      'c_dw_c_2'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_1'] <-      'c_dw_c_1'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_0'] <-      'c_dw_c_0'
colnames(df.c.2)[colnames(df.c.2) == 'c_m_c_na'] <-     'c_dw_c_na'

df.c.2$c_odw_a_0

##### recode #####
#Recode c_neighborhood
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 11] <- 1
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 22] <- 2
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 33] <- 3
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 44] <- 4
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 55] <- 5
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 66] <- 6
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 77] <- 7
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 88] <- 8
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 99] <- 9
df.c.2$c_neighborhood[df.c.2$c_neighborhood == 0] <- 10


##### create new variables #####
df.c.2$c_extra_data <- NA
df.c.2$c_extra_data <- ifelse(!is.na(df.c.2$c_odw_a_3), 1, 0)

# Create country or deployment code
df.c.2$dply_num <- 2

# Create floodwater time interval variable
df.c.2$c_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new c_id variable
df.c.2$c_id <- (1000:(1000+nrow(df.c.2)-1))

# Create unique ID variable
df.c.2$c_UID <- NA

# paste number together
df.c.2$c_UID <- paste0(sprintf("%02d", df.c.2$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.2$c_id)))
# Create date variable
df.c.2$c_date <- NA
df.c.2$c_date <- substr(df.c.2$c_start, 1, 10)
df.c.2$c_date <- as.Date(df.c.2$c_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.2$c_participant_female[df.c.2$c_id == 1013] <- 17
df.c.2$c_participant_male[df.c.2$c_id == 1013] <- 0
df.c.2$c_participant_female[df.c.2$c_id == 1014] <- 17
df.c.2$c_participant_male[df.c.2$c_id == 1014] <- 0
df.c.2$c_participant_female[df.c.2$c_id == 1024] <- 20
df.c.2$c_participant_male[df.c.2$c_id == 1024] <- 0



##### deployment-specific info #####
df.c.2$c_group <- toupper(df.c.2$c_group)
df.c.2$c_extra_data <- NA


extra2 <- which(colnames(df.c.2) %in% setdiff(colnames(df.c.2), colnames(df.c.0)))

# remove "extra" columns from df1
df.c.2.clean <- df.c.2[, -extra2]

# export extra data
df.c.2.extra <- df.c.2[, extra2]
df.c.2.extra$c_UID <- df.c.2$c_UID

##### merge datasets #####
df.c.2.clean$c_enum <- as.character(df.c.2.clean$c_enum) 

df.c <- bind_rows(df.c.0, df.c.2.clean)


# **********************************************************************************


#---- 3 GHANA -----
##### rename variables #####
colnames(df.c.3)[colnames(df.c.3) == 'form.entity']   <- 'c_group'
colnames(df.c.3)[colnames(df.c.3) == 'started_time']  <- 'c_start'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_gps']   <- 'c_coordinates'
# colnames(df.c.3)[colnames(df.c.3) == 'form.neighbor'] <- 'c_neighborhood'

colnames(df.c.3)[colnames(df.c.3) == 'form.num_partic']  <- 'c_participant_num'
colnames(df.c.3)[colnames(df.c.3) == 'form.num_fem']  <- 'c_participant_female_obs'
colnames(df.c.3)[colnames(df.c.3) == 'form.num_male'] <- 'c_participant_male_obs'

colnames(df.c.3)[colnames(df.c.3) == 'form.com_q1a'] <- 'c_participant_female'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q2a'] <- 'c_participant_male'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q3a'] <- 'c_neighborhood_y'

# Adults
# surface water
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q5a'] <- 'c_s_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q5c'] <- 'c_s_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q5e'] <- 'c_s_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q5g'] <- 'c_s_a_0'

# open drains
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q6a'] <- 'c_d_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q6c'] <- 'c_d_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q6e'] <- 'c_d_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q6g'] <- 'c_d_a_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q6i'] <- 'c_d_a_na'

# floodwater
# NOTE: time frame is month instead of week
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q7a'] <- 'c_f_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q7c'] <- 'c_f_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q7e'] <- 'c_f_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q7g'] <- 'c_f_a_0'

# drinking water
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q8a'] <- 'c_dw_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q8c'] <- 'c_dw_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q8e'] <- 'c_dw_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q8g'] <- 'c_dw_a_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q8i'] <- 'c_dw_a_na'

# drinking water treatment
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q11a'] <- 'c_dw_e_wt_1'

# ocean
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q4a'] <- 'c_o_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q4c'] <- 'c_o_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q4e'] <- 'c_o_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q4g'] <- 'c_o_a_0'

# produce
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q9a'] <- 'c_p_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q9c'] <- 'c_p_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q9e'] <- 'c_p_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q9g'] <- 'c_p_a_0'

# public latrine
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q10a'] <- 'c_l_a_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q10c'] <- 'c_l_a_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q10e'] <- 'c_l_a_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q10g'] <- 'c_l_a_0'

# have a toilet at home
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q12a'] <- 'c_pl_a_th_1'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q12c'] <- 'c_pl_a_th_0'

# use toilet at home
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q13a'] <- 'c_pl_a_tu_1'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q13c'] <- 'c_pl_a_tu_nl'

# flush toilet at home
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q14a'] <- 'c_pl_a_tw_1'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q14c'] <- 'c_pl_a_tw_0'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q14e'] <- 'c_pl_a_tw_nl'

# toilet at home floods
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q15a'] <- 'c_pl_a_tf_1'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q15c'] <- 'c_pl_a_tf_0'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_q15e'] <- 'c_pl_a_tf_nl'

# Children

# surface water
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q20a'] <- 'c_s_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q20c'] <- 'c_s_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q20e'] <- 'c_s_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q20g'] <- 'c_s_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q20i'] <- 'c_s_c_na'

# open drains
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q21a'] <- 'c_d_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q21c'] <- 'c_d_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q21e'] <- 'c_d_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q21g'] <- 'c_d_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q21i'] <- 'c_d_c_na'

# floodwater
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q22a'] <- 'c_f_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q22c'] <- 'c_f_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q22e'] <- 'c_f_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q22g'] <- 'c_f_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q22i'] <- 'c_f_c_na'

# drinking water
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q23a'] <- 'c_dw_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q23c'] <- 'c_dw_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q23e'] <- 'c_dw_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q23g'] <- 'c_dw_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q23i'] <- 'c_dw_c_na'

# ocean
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q19a'] <- 'c_o_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q19c'] <- 'c_o_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q19e'] <- 'c_o_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q19g'] <- 'c_o_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q19i'] <- 'c_o_c_na'

# produce
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q24a'] <- 'c_p_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q24c'] <- 'c_p_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q24e'] <- 'c_p_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q24g'] <- 'c_p_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q24i'] <- 'c_p_c_na'

# public latrine
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q25a'] <- 'c_l_c_3'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q25c'] <- 'c_l_c_2'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q25e'] <- 'c_l_c_1'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q25g'] <- 'c_l_c_0'
colnames(df.c.3)[colnames(df.c.3)  == 'form.com_q25i'] <- 'c_l_c_na'

colnames(df.c.3)[colnames(df.c.3) == 'form.com_notes'] <- 'c_notes'
colnames(df.c.3)[colnames(df.c.3) == 'form.com_enumerator_names'] <- 'c_enum'

#check for differences (returns those in x but not y)
setdiff(colnames(df.c.3), colnames(df.c.0))

##### create new variables #####
# Create country or deployment code
df.c.3$dply_num <- 3

# Create floodwater time interval variable
df.c.3$c_f_weekly <- 0    # 1 if weekly, 0 if not weekly

# Create unique ID variable
df.c.3$c_UID <- NA

df.c.3$c_UID <- paste0(sprintf("%02d", df.c.3$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.3$number)))

# Create date variable
df.c.3$c_date <- NA
df.c.3$c_date <- substr(df.c.3$c_start, 1, 10)
df.c.3$c_date <- as.Date(df.c.3$c_date, format = "%Y-%m-%d")

# Code c_neighborhood as numeric
neigh <- meta_neighb[meta_neighb$deployment_id == 3 ,][,c("neighborhood_id", "neighborhood")]
colnames(neigh) <- c("c_neighborhood", "c_neigh")

df.c.3 <- left_join(df.c.3, neigh, by=c("form.neighbor" = "c_neigh"))


# Create separate location variables using form.ev_lat_lon (now c_coordinates)
df.c.3 <- separate(df.c.3, c_coordinates, c("X_c_coordinates_latitude","X_c_coordinates_longitude",
                                            "X_c_coordinates_altitude","X_c_coordinates_precision"), 
                   sep=" ", remove = FALSE)

df.c.3$X_c_coordinates_latitude <- as.numeric(df.c.3$X_c_coordinates_latitude)
df.c.3$X_c_coordinates_longitude <- as.numeric(df.c.3$X_c_coordinates_longitude)
df.c.3$X_c_coordinates_altitude <- as.numeric(df.c.3$X_c_coordinates_altitude)
df.c.3$X_c_coordinates_precision <- as.numeric(df.c.3$X_c_coordinates_precision)

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.3$c_participant_gender <- ifelse(df.c.3$c_participant_female == 0, 1, NA)
df.c.3$c_participant_gender <- ifelse(df.c.3$c_participant_male == 0, 0, df.c.3$c_participant_gender)


##### deployment-specific info #####
# exclude test data (numeric neighborhood)
df.c.3 <- filter(df.c.3, c_neighborhood != 2)

# Remove extra info from DF3 
extra3 <- which(colnames(df.c.3) %in% setdiff(colnames(df.c.3), colnames(df.c.0)))

# remove "extra" columns from DF3
df.c.3.clean <- df.c.3[, -extra3]

# export extra data
df.c.3.extra <- df.c.3[, extra3]
df.c.3.extra$c_UID <- df.c.3$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.3.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.3.clean)


# **********************************************************************************

# **********************************************************************************
#---- 4 ZAMBIA -----
setdiff(colnames(df.c.4), colnames(df.c.0))

##### rename variables #####
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_c_3'] <- 'c_odw_c_3'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_c_2'] <- 'c_odw_c_2'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_c_1'] <- 'c_odw_c_1'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_c_0'] <- 'c_odw_c_0'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_c_na'] <- 'c_odw_c_na'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_a_3'] <- 'c_odw_a_3'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_a_2'] <- 'c_odw_a_2'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_a_1'] <- 'c_odw_a_1'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_a_0'] <- 'c_odw_a_0'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwbore_a_na'] <- 'c_odw_a_na'

colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_c_3'] <- 'c_odw2_c_3'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_c_2'] <- 'c_odw2_c_2'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_c_1'] <- 'c_odw2_c_1'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_c_0'] <- 'c_odw2_c_0'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_c_na'] <- 'c_odw2_c_na'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_a_3'] <- 'c_odw2_a_3'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_a_2'] <- 'c_odw2_a_2'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_a_1'] <- 'c_odw2_a_1'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_a_0'] <- 'c_odw2_a_0'
colnames(df.c.4)[colnames(df.c.4) == 'c_dwwell_a_na'] <- 'c_odw2_a_na'
##### recode #####
#indicate extra data
df.c.4$c_extra_data <- NA
df.c.4$c_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.c.4$dply_num <- 4

# Create floodwater time interval variable
df.c.4$c_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new c_id variable
df.c.4$c_id <- (1000:(1000+nrow(df.c.4)-1))

# Create unique ID variable
df.c.4$c_UID <- NA

# paste number together
df.c.4$c_UID <- paste0(sprintf("%02d", df.c.4$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.4$c_id)))

# Create date variable
df.c.4$c_date <- NA
df.c.4$c_date <- substr(df.c.4$X_submission_time, 1, 10)
df.c.4$c_date <- as.Date(df.c.4$c_date, format = "%Y-%m-%d")

df.c.4$c_date
# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.4$c_participant_female[df.c.4$c_id == 1000] <- 0
df.c.4$c_participant_male[df.c.4$c_id == 1000] <- 19
df.c.4$c_participant_female[df.c.4$c_id == 1001] <- 0
df.c.4$c_participant_male[df.c.4$c_id == 1001] <- 20
df.c.4$c_participant_female[df.c.4$c_id == 1002] <- 20
df.c.4$c_participant_male[df.c.4$c_id == 1002] <- 0
df.c.4$c_participant_female[df.c.4$c_id == 1003] <- 20
df.c.4$c_participant_male[df.c.4$c_id == 1003] <- 0

##### deployment-specific info #####
# Remove extra info from DF4 
extra4 <- which(colnames(df.c.4) %in% setdiff(colnames(df.c.4), colnames(df.c.0)))

# remove "extra" columns from DF3
df.c.4.clean <- df.c.4[, -extra4]

# export extra data
df.c.4.extra <- df.c.4[, extra4]
df.c.4.extra$c_UID <- df.c.4$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.4.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.4.clean)

# **********************************************************************************

#---- 6 GHANA Accra -----
setdiff(colnames(df.c.6), colnames(df.c.0))

##### rename variables #####
##### recode #####
#indicate extra data
df.c.6$c_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.c.6$dply_num <- 6

# Create new c_id variable
df.c.6$c_id <- (1000:(1000+nrow(df.c.6)-1))

# Create unique ID variable
df.c.6$c_UID <- NA

# paste number together
df.c.6$c_UID <- paste0(sprintf("%02d", df.c.6$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.6$c_id)))

# Create date variable
df.c.6$c_date <- NA
df.c.6$c_date <- substr(df.c.6$c_start, 1, 10)
df.c.6$c_date <- as.Date(df.c.6$c_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.6$c_participant_female[df.c.6$c_id == 1000] <- 19
df.c.6$c_participant_male[df.c.6$c_id == 1000] <- 0
df.c.6$c_participant_female[df.c.6$c_id == 1001] <- 0
df.c.6$c_participant_male[df.c.6$c_id == 1001] <- 18
df.c.6$c_participant_female[df.c.6$c_id == 1002] <- 15
df.c.6$c_participant_male[df.c.6$c_id == 1002] <- 0
df.c.6$c_participant_female[df.c.6$c_id == 1003] <- 0
df.c.6$c_participant_male[df.c.6$c_id == 1003] <- 15
df.c.6$c_participant_female[df.c.6$c_id == 1004] <- 15
df.c.6$c_participant_male[df.c.6$c_id == 1004] <- 0
df.c.6$c_participant_female[df.c.6$c_id == 1005] <- 15
df.c.6$c_participant_male[df.c.6$c_id == 1005] <- 0
df.c.6$c_participant_female[df.c.6$c_id == 1006] <- 15
df.c.6$c_participant_male[df.c.6$c_id == 1006] <- 0
df.c.6$c_participant_female[df.c.6$c_id == 1007] <- 0
df.c.6$c_participant_male[df.c.6$c_id == 1007] <- 15

##### deployment-specific info #####
df.c.6$c_neighborhood[df.c.6$c_neighborhood == 268] <- 1
df.c.6$c_neighborhood[df.c.6$c_neighborhood == 269] <- 2

table(df.c.6$c_neighborhood)

# Remove extra info from DF 
extra6 <- which(colnames(df.c.6) %in% setdiff(colnames(df.c.6), colnames(df.c.0)))

# remove "extra" columns from DF3
df.c.6.clean <- df.c.6[, -extra6]

# export extra data
df.c.6.extra <- df.c.6[, extra6]
df.c.6.extra$c_UID <- df.c.6$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.6.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.6.clean)


# **********************************************************************************
#---- 7 GHANA Kumasi -----
setdiff(colnames(df.c.7), colnames(df.c.0))

##### rename variables #####
##### recode #####
#indicate extra data
df.c.7$c_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.c.7$dply_num <- 7

# Create new c_id variable
df.c.7$c_id <- (1000:(1000+nrow(df.c.7)-1))

# Create unique ID variable
df.c.7$c_UID <- NA

# paste number together
df.c.7$c_UID <- paste0(sprintf("%02d", df.c.7$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.7$c_id)))

# Create date variable
df.c.7$c_date <- NA
df.c.7$c_date <- substr(df.c.7$c_start, 1, 10)
df.c.7$c_date <- as.Date(df.c.7$c_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.7$c_participant_male <- ifelse(df.c.7$c_participant_gender == 0, 15, 0)
df.c.7$c_participant_female <- ifelse(df.c.7$c_participant_gender == 1, 15, 0)


##### deployment-specific info #####
df.c.7$c_neighborhood[df.c.7$c_neighborhood == 280] <- 1
df.c.7$c_neighborhood[df.c.7$c_neighborhood == 281] <- 2
df.c.7$c_neighborhood[df.c.7$c_neighborhood == 282] <- 3
df.c.7$c_neighborhood[df.c.7$c_neighborhood == 283] <- 4

table(df.c.7$c_neighborhood)

# Remove extra info from DF 
extra7 <- which(colnames(df.c.7) %in% setdiff(colnames(df.c.7), colnames(df.c.0)))

# remove "extra" columns from DF
df.c.7.clean <- df.c.7[, -extra7]

# export extra data
df.c.7.extra <- df.c.7[, extra7]
df.c.7.extra$c_UID <- df.c.7$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.7.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.7.clean)
# **********************************************************************************
# **********************************************************************************
#---- 9 UGANDA Kampala -----
setdiff(colnames(df.c.9), colnames(df.c.0))

##### rename variables #####
##### recode #####
#indicate extra data
df.c.9$c_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.c.9$dply_num <- 9

# Create new c_id variable
df.c.9$c_id <- (1000:(1000+nrow(df.c.9)-1))

# Create unique ID variable
df.c.9$c_UID <- NA

# paste number together
df.c.9$c_UID <- paste0(sprintf("%02d", df.c.9$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.c.9$c_id)))

# Create date variable
df.c.9$c_date <- NA
df.c.9$c_date <- substr(df.c.9$c_start, 1, 10)
df.c.9$c_date <- as.Date(df.c.9$c_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.9$c_participant_female[df.c.9$c_id == 1001] <- 8
df.c.9$c_participant_male[df.c.9$c_id == 1001] <- 0
df.c.9$c_participant_female[df.c.9$c_id == 1008] <- 12
df.c.9$c_participant_male[df.c.9$c_id == 1008] <- 0

##### deployment-specific info #####
table(df.c.9$c_neighborhood)

df.c.9$c_neighborhood[df.c.9$c_neighborhood == 309] <- 1
df.c.9$c_neighborhood[df.c.9$c_neighborhood == 310] <- 2
df.c.9$c_neighborhood[df.c.9$c_neighborhood == 311] <- 3
df.c.9$c_neighborhood[df.c.9$c_neighborhood == 312] <- 4
df.c.9$c_neighborhood[df.c.9$c_neighborhood == 313] <- 5


# Remove extra info from DF 
extra9 <- which(colnames(df.c.9) %in% setdiff(colnames(df.c.9), colnames(df.c.0)))

# remove "extra" columns from DF
df.c.9.clean <- df.c.9[, -extra9]

# export extra data
df.c.9.extra <- df.c.9[, extra9]
df.c.9.extra$c_UID <- df.c.9$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.9.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.9.clean)
# **********************************************************************************
# **********************************************************************************


# **********************************************************************************
#---- 11 India Vellore ----
setdiff(colnames(df.c.11), colnames(df.c.0))


##### rename variables #####
colnames(df.c.11)[colnames(df.c.11) == 'response_id'] <- 'c_id'
colnames(df.c.11)[colnames(df.c.11) == 'neighborhood_id'] <- 'c_neighborhood'

colnames(df.c.11)[colnames(df.c.11) == 'response_date'] <- 'c_date'
colnames(df.c.11)[colnames(df.c.11) == 'GPS_latitude'] <- 'X_c_coordinates_latitude'
colnames(df.c.11)[colnames(df.c.11) == 'GPS_longitude'] <- 'X_c_coordinates_longitude'
colnames(df.c.11)[colnames(df.c.11) == 'number_male'] <- 'c_participant_male'
colnames(df.c.11)[colnames(df.c.11) == 'number_female'] <- 'c_participant_female'
colnames(df.c.11)[colnames(df.c.11) == 'number_participant'] <- 'c_participant_num'


df.c.11$c_date <- as.Date(df.c.11$c_date, "%Y-%m-%d")

##### recode #####
df.c.11$c_participant_gender <- c(1,1,0,0,1,1,0,0)

df.c.11$X_c_coordinates_latitude <- as.numeric(df.c.11$X_c_coordinates_latitude)
df.c.11$X_c_coordinates_longitude <- as.numeric(df.c.11$X_c_coordinates_longitude)


##### create new variables #####
# Create country or deployment code
df.c.11$dply_num <- 11

df.c.11$c_extra_data <- 0

# Create unique ID variable
df.c.11$c_UID <- NA

df.c.11$c_id <- 1000+df.c.11$c_id
df.c.11$c_id_val <- df.c.11$c_id

# paste number together
df.c.11$c_UID <- paste0(sprintf("%02d", df.c.11$dply_num),
                        "_", 
                        sprintf("%04d", df.c.11$c_id))

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra11 <- which(colnames(df.c.11) %in% setdiff(colnames(df.c.11), colnames(df.c.0)))

# remove "extra" columns from df
df.c.11.clean <- df.c.11[, -extra11]

# export extra data
df.c.11.extra <- df.c.11[, extra11]
df.c.11.extra$c_UID <- df.c.11$c_UID

##### merge datasets #####
# setdiff(colnames(df.c.11.clean), colnames(df.c.0))

df.c <- bind_rows(df.c, df.c.11.clean)

# # **********************************************************************************
# #---- 99 INDIA KOLKATA -----
# setdiff(colnames(df.c.99), colnames(df.c.0))
# 
# ##### rename variables #####
# 
# ##### recode #####
# #indicate extra data
# df.c.99$c_extra_data <- 1
# 
# ##### create new variables #####
# # Create country or deployment code
# df.c.99$dply_num <- 12
# 
# # Create floodwater time interval variable
# # df.c.99$c_f_weekly <- 1    # 1 if weekly, 0 if not weekly
# 
# # Create new c_id variable
# df.c.99$c_id <- (1000:(1000+nrow(df.c.99)-1))
# 
# # paste number together
# df.c.99$c_UID <- paste0(sprintf("%02d", df.c.99$dply_num),
#                         "_",
#                         sprintf("%04d", as.numeric(df.c.99$c_id)))
# 
# # Create date variable
# df.c.99$c_date <- substr(df.c.99$c_start, 1, 10)
# df.c.99$c_date <- as.Date(df.c.99$c_date, format = "%Y-%m-%d")
# 
# 
# # hardcode fixes to female/ male numbers
# # 0	All male
# # 1	All female
# # 2	A combination of male and female
# df.c.99$c_participant_female[df.c.99$c_id == 1000] <- 16
# df.c.99$c_participant_male[df.c.99$c_id == 1000] <- 0
# df.c.99$c_participant_female[df.c.99$c_id == 1001] <- 16
# df.c.99$c_participant_male[df.c.99$c_id == 1001] <- 0
# df.c.99$c_participant_female[df.c.99$c_id == 1002] <- 0
# df.c.99$c_participant_male[df.c.99$c_id == 1002] <- 15
# df.c.99$c_participant_female[df.c.99$c_id == 1003] <- 0
# df.c.99$c_participant_male[df.c.99$c_id == 1003] <- 17
# 
# ##### deployment-specific info #####
# # Remove extra info from DF4 
# extra12 <- which(colnames(df.c.99) %in% setdiff(colnames(df.c.99), colnames(df.c.0)))
# 
# # remove "extra" columns from DF3
# df.c.99.clean <- df.c.99[, -extra99]
# 
# # export extra data
# df.c.99.extra <- df.c.99[, extra99]
# df.c.99.extra$c_UID <- df.c.99$c_UID
# 
# ##### merge datasets #####
# # Check that dataframes have same dimensions
# setdiff(colnames(df.c.99.clean), colnames(df.c))
# 
# # Merge clean datasets
# df.c <- bind_rows(df.c, df.c.99.clean)

# **********************************************************************************

#---- 12 SENEGAL Dakar-----
setdiff(colnames(df.c.12), colnames(df.c.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.c.12$c_extra_data <- 1

df.c.12$c_neighborhood[df.c.12$c_neighborhood == 409] <- 1
df.c.12$c_neighborhood[df.c.12$c_neighborhood == 410] <- 2
df.c.12$c_neighborhood[df.c.12$c_neighborhood == 411] <- 3
df.c.12$c_neighborhood[df.c.12$c_neighborhood == 412] <- 4
df.c.12$c_neighborhood[df.c.12$c_neighborhood == 413] <- 5


##### create new variables #####
# Create country or deployment code
df.c.12$dply_num <- 12


# Create new c_id variable
df.c.12$c_id <- (1000:(1000+nrow(df.c.12)-1))

# Create unique ID variable
df.c.12$c_UID <- NA

# paste number together
df.c.12$c_UID <- paste0(sprintf("%02d", df.c.12$dply_num),
                        "_",
                        sprintf("%04d", as.numeric(df.c.12$c_id)))

# Create date variable
df.c.12$c_date <- NA
df.c.12$c_date <- substr(df.c.12$X_submission_time, 1, 10)
df.c.12$c_date <- as.Date(df.c.12$c_date, format = "%Y-%m-%d")

df.c.12$c_date
#
df.c.12 <- df.c.12 %>% mutate(c_participant_male = case_when(c_participant_gender == 0 ~ c_participant_num))
df.c.12 <- df.c.12 %>% mutate(c_participant_female = case_when(c_participant_gender == 1 ~ c_participant_num))

df.c.12$c_participant_male[is.na(df.c.12$c_participant_male)] <- 0
df.c.12$c_participant_female[is.na(df.c.12$c_participant_female)] <- 0

# df.c.12$c_participant_male_obs <- df.c.12$c_participant_male
# df.c.12$c_participant_female_obs <- df.c.12$c_participant_female

# all participants were from the corresponding neighborhoods in each survey
df.c.12$c_neighborhood_y <- df.c.12$c_participant_num
df.c.12$c_neighborhood_n <- 0

##### deployment-specific info #####
# Remove extra info from DF 
extra12 <- which(colnames(df.c.12) %in% setdiff(colnames(df.c.12), colnames(df.c.0)))

# remove "extra" columns from DF3
df.c.12.clean <- df.c.12[, -extra12]

# export extra data
df.c.12.extra <- df.c.12[, extra12]
df.c.12.extra$c_UID <- df.c.12$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.12.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.12.clean)

# **********************************************************************************




# **********************************************************************************
#---- 13 ZAMBIA 2 -----
setdiff(colnames(df.c.13), colnames(df.c.0))

##### rename variables #####
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_c_3'] <- 'c_odw_c_3'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_c_2'] <- 'c_odw_c_2'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_c_1'] <- 'c_odw_c_1'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_c_0'] <- 'c_odw_c_0'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_c_na'] <- 'c_odw_c_na'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_a_3'] <- 'c_odw_a_3'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_a_2'] <- 'c_odw_a_2'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_a_1'] <- 'c_odw_a_1'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_a_0'] <- 'c_odw_a_0'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwbore_a_na'] <- 'c_odw_a_na'

colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_c_3'] <- 'c_odw2_c_3'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_c_2'] <- 'c_odw2_c_2'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_c_1'] <- 'c_odw2_c_1'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_c_0'] <- 'c_odw2_c_0'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_c_na'] <- 'c_odw2_c_na'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_a_3'] <- 'c_odw2_a_3'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_a_2'] <- 'c_odw2_a_2'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_a_1'] <- 'c_odw2_a_1'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_a_0'] <- 'c_odw2_a_0'
colnames(df.c.13)[colnames(df.c.13) == 'c_dwwell_a_na'] <- 'c_odw2_a_na'

##### recode #####
#indicate extra data
df.c.13$c_extra_data <- 1

# neighborhood
df.c.13$c_neighborhood[df.c.13$c_neighborhood == 381] <- 1
df.c.13$c_neighborhood[df.c.13$c_neighborhood == 382] <- 2
df.c.13$c_neighborhood[df.c.13$c_neighborhood == 380] <- 3

##### create new variables #####
# Create country or deployment code
df.c.13$dply_num <- 13


# Create new c_id variable
df.c.13$c_id <- (1000:(1000+nrow(df.c.13)-1))

# Create unique ID variable
df.c.13$c_UID <- NA

# paste number together
df.c.13$c_UID <- paste0(sprintf("%02d", df.c.13$dply_num),
                        "_",
                        sprintf("%04d", as.numeric(df.c.13$c_id)))

# Create date variable
df.c.13$c_date <- NA
df.c.13$c_date <- substr(df.c.13$X_submission_time, 1, 10)
df.c.13$c_date <- as.Date(df.c.13$c_date, format = "%Y-%m-%d")

df.c.13$c_date
# populate/ fix female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.c.13$c_participant_male <- ifelse(df.c.13$c_participant_gender == 0, df.c.13$c_participant_num, df.c.13$c_participant_male)
df.c.13$c_participant_female <- ifelse(df.c.13$c_participant_gender == 1, df.c.13$c_participant_num, df.c.13$c_participant_female)

df.c.13$c_participant_male[is.na(df.c.13$c_participant_male)] <- 0
df.c.13$c_participant_female[is.na(df.c.13$c_participant_female)] <- 0



##### deployment-specific info #####
# Remove extra info from DF 
extra13 <- which(colnames(df.c.13) %in% setdiff(colnames(df.c.13), colnames(df.c.0)))

# remove "extra" columns from DF3
df.c.13.clean <- df.c.13[, -extra13]

# export extra data
df.c.13.extra <- df.c.13[, extra13]
df.c.13.extra$c_UID <- df.c.13$c_UID

##### merge datasets #####
# Check that dataframes have same dimensions
setdiff(colnames(df.c.13.clean), colnames(df.c))

# Merge clean datasets
df.c <- bind_rows(df.c, df.c.13.clean)

# **********************************************************************************

# **********************************************************************************
# add unique neighborhood numbers (over all projects)
df.c$neighb_UID <- paste0(sprintf("%01d", df.c$dply_num),
                          sprintf("%02d", df.c$c_neighborhood))

df.c$neighb_UID <- as.numeric(df.c$neighb_UID)

df.c <- left_join(df.c, meta_dply[,c(1,4)], by = c("dply_num" = "id"))


# **********************************************************************************
#---- 99 WRITE DATA -----
write.csv(df.c, paste0(getwd(), "/data/", "c_merged_", Sys.Date(), ".csv"), row.names = F, na="")

write.csv(df.c.2.extra, paste0(getwd(), "/data/extra_data/", "c_extra_02.csv"), row.names = F, na="")
write.csv(df.c.3.extra, paste0(getwd(), "/data/extra_data/", "c_extra_03.csv"), row.names = F, na="")
write.csv(df.c.4.extra, paste0(getwd(), "/data/extra_data/", "c_extra_04.csv"), row.names = F, na="")
# write.csv(df.c.6.extra, paste0(getwd(), "/data/extra_data/", "c_extra_06.csv"), row.names = F, na="")
write.csv(df.c.7.extra, paste0(getwd(), "/data/extra_data/", "c_extra_07.csv"), row.names = F, na="")
write.csv(df.c.9.extra, paste0(getwd(), "/data/extra_data/", "c_extra_09.csv"), row.names = F, na="")
# write.csv(df.c.11.extra, paste0(getwd(), "/data/extra_data/", "c_extra_11.csv"), row.names = F, na="")
write.csv(df.c.12.extra, paste0(getwd(), "/data/extra_data/", "c_extra_12.csv"), row.names = F, na="")
# write.csv(df.c.99.extra, paste0(getwd(), "/data/extra_data/", "c_extra_99.csv"), row.names = F, na="")
write.csv(df.c.13.extra, paste0(getwd(), "/data/extra_data/", "c_extra_13.csv"), row.names = F, na="")

