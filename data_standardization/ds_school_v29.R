# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# Form: School (s)
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
df.s.0 <- read.table(paste0(getwd(), "/data/deployments/00_master/", "SCHOOL_10_20_17_WM - xml - 2017-10-31-18-08", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T)

# read bangladesh form
df.s.2 <- read.csv(paste0(getwd(), "/data/deployments/02_bangladesh/", "School_final_2017_12_05_11_26_59", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, na.strings = c("N/A", "NA"))

# read ghana form
df.s.3 <- read.csv(paste0(getwd(), "/data/deployments/03_ghana/", "ghana s", ".csv"), 
                   stringsAsFactors=F, header=T,
                   na.strings = c("na", "NA", "Na", "N.A.", "---"))

# read zambia form
df.s.4 <- read.csv(paste0(getwd(), "/data/deployments/04_zambia/", "sc_lusaka", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana accra form
df.s.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "accrametro-school-1499", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.s.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "ksisanipath-school-1618(1)", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read uganda kampala
df.s.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/", "sc_uganda", ".csv"), 
                   sep=",", stringsAsFactors=F, header=T, 
                   na.strings = c("n/a", "NA", "N/A", "---"))

# read vellore form
df.s.11 <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/", "school_clean", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# # read kolkata form
# df.s.99 <- read.csv(paste0(getwd(), "/data/deployments/99_kolkata/", "spt_sc", ".csv"), 
#                     sep=",", stringsAsFactors=F, header=T, 
#                     na.strings = c("n/a", "NA", "N/A", "---"))

# read senegal form
df.s.12 <- read.csv(paste0(getwd(), "/data/deployments/12_senegal/", 
                           "School_Survey-xlsform_waspa_final_2020_02_05_17_51_53", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))

# read lusaka2 form
df.s.13 <- read.csv(paste0(getwd(), "/data/deployments/13_zambia2/", "School_Survey-xlsform_lusaka2019_final_2019_11_04_18_49_27", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T, 
                    na.strings = c("n/a", "NA", "N/A", "---"))
# **********************************************************************************

#---- MASTER ----
##### create new variables #####
# other drinking water 1
df.s.0$s_odw_a <- NA
df.s.0$s_odw_a_note <- NA
df.s.0$s_odw_a_3 <- NA
df.s.0$s_odw_a_2 <- NA
df.s.0$s_odw_a_1 <- NA
df.s.0$s_odw_a_0 <- NA
df.s.0$s_odw_a_na <- NA
df.s.0$s_odw_a_other <- NA
df.s.0$s_odw_c <- NA
df.s.0$s_odw_c_note <- NA
df.s.0$s_odw_c_3 <- NA
df.s.0$s_odw_c_2 <- NA 
df.s.0$s_odw_c_1 <- NA
df.s.0$s_odw_c_0 <- NA
df.s.0$s_odw_c_na <- NA
df.s.0$s_odw_c_other <- NA

# other drinking water 2
df.s.0$s_odw2_a <- NA
df.s.0$s_odw2_a_note <- NA
df.s.0$s_odw2_a_3 <- NA
df.s.0$s_odw2_a_2 <- NA
df.s.0$s_odw2_a_1 <- NA
df.s.0$s_odw2_a_0 <- NA
df.s.0$s_odw2_a_na <- NA
df.s.0$s_odw2_a_other <- NA
df.s.0$s_odw2_c <- NA
df.s.0$s_odw2_c_note <- NA
df.s.0$s_odw2_c_3 <- NA
df.s.0$s_odw2_c_2 <- NA 
df.s.0$s_odw2_c_1 <- NA
df.s.0$s_odw2_c_0 <- NA
df.s.0$s_odw2_c_na <- NA
df.s.0$s_odw2_c_other <- NA


df.s.0$s_extra_data <- NA
df.s.0$dply_num <- NA
df.s.0$s_f_weekly <- NA    # 1 if weekly, 0 if not weekly

df.s.0$s_UID <- NA
df.s.0$s_id <- NA
df.s.0$s_UID <- as.character(df.s.0$s_UID)

df.s.0$s_date <- NA
df.s.0$s_date <- as.Date(df.s.0$s_date, format = "%Y-%m-%d")

##### merge datasets #####
df.s.0 <- df.s.0[-c(1:1),]
df.s.0$s_neighborhood <- as.numeric(df.s.0$s_neighborhood)

# **********************************************************************************
#---- 2 BANGLADESH ----
##### rename variables #####
# setdiff(colnames(df.s.2), colnames(df.s.0))

colnames(df.s.2)[colnames(df.s.2) == 's_m_c'] <-        's_dw_c'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_note'] <-   's_dw_c_note'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_3'] <-      's_dw_c_3'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_2'] <-      's_dw_c_2'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_1'] <-      's_dw_c_1'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_0'] <-      's_dw_c_0'
colnames(df.s.2)[colnames(df.s.2) == 's_m_c_na'] <-     's_dw_c_na'

colnames(df.s.2)[colnames(df.s.2) == 's_m_a'] <-        's_dw_a'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_note'] <-   's_dw_a_note'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_3'] <-      's_dw_a_3'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_2'] <-      's_dw_a_2'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_1'] <-      's_dw_a_1'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_0'] <-      's_dw_a_0'
colnames(df.s.2)[colnames(df.s.2) == 's_m_a_na'] <-     's_dw_a_na'

colnames(df.s.2)[colnames(df.s.2) == 's_wt'] <-         's_dw_e_wt'
colnames(df.s.2)[colnames(df.s.2) == 's_wt_note'] <-    's_dw_e_wt_note'
colnames(df.s.2)[colnames(df.s.2) == 's_wt_1'] <-       's_dw_e_wt_1'
colnames(df.s.2)[colnames(df.s.2) == 's_wt_0'] <-       's_dw_e_wt_0'
colnames(df.s.2)[colnames(df.s.2) == 's_wt_na'] <-      's_dw_e_wt_na'

colnames(df.s.2)[colnames(df.s.2) == 's_th_a'] <-       's_pl_a_th'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_note'] <-  's_pl_a_th_note'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_1'] <-     's_pl_a_th_1'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_0'] <-     's_pl_a_th_0'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_na'] <-    's_pl_a_th_na'
colnames(df.s.2)[colnames(df.s.2) == 's_tu_a_note'] <-  's_pl_a_tu_note'
colnames(df.s.2)[colnames(df.s.2) == 's_tu_a_1'] <-     's_pl_a_tu_1'
colnames(df.s.2)[colnames(df.s.2) == 's_tu_a_0'] <-     's_pl_a_tu_0'
colnames(df.s.2)[colnames(df.s.2) == 's_tu_a_na'] <-    's_pl_a_tu_na'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_02'] <-    's_pl_a_tu_nl'
colnames(df.s.2)[colnames(df.s.2) == 's_tw_a_note'] <-  's_pl_a_tw_note'
colnames(df.s.2)[colnames(df.s.2) == 's_tw_a_1'] <-     's_pl_a_tw_1'
colnames(df.s.2)[colnames(df.s.2) == 's_tw_a_0'] <-     's_pl_a_tw_0'
colnames(df.s.2)[colnames(df.s.2) == 's_tw_a_na'] <-    's_pl_a_tw_na'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_03'] <-    's_pl_a_tw_nl'
colnames(df.s.2)[colnames(df.s.2) == 's_tf_a_note'] <-  's_pl_a_tf_note'
colnames(df.s.2)[colnames(df.s.2) == 's_tf_a_1'] <-     's_pl_a_tf_1'
colnames(df.s.2)[colnames(df.s.2) == 's_tf_a_0'] <-     's_pl_a_tf_0'
colnames(df.s.2)[colnames(df.s.2) == 's_tf_a_na'] <-    's_pl_a_tf_na'
colnames(df.s.2)[colnames(df.s.2) == 's_th_a_04'] <-    's_pl_a_tf_nl'

# colnames(df.s.2)[colnames(df.s.2) == 's_p_a2_note'] <-  's_p_e_note'
# colnames(df.s.2)[colnames(df.s.2) == 's_p_a2_2'] <-     's_p_e_2'
# colnames(df.s.2)[colnames(df.s.2) == 's_p_a2_1'] <-     's_p_e_1'
# colnames(df.s.2)[colnames(df.s.2) == 's_p_a2_na'] <-    's_p_e_na'

colnames(df.s.2)[colnames(df.s.2) == 's_p_c2_note'] <-  's_p_e_note'
colnames(df.s.2)[colnames(df.s.2) == 's_p_c2_2'] <-     's_p_e_2'
colnames(df.s.2)[colnames(df.s.2) == 's_p_c2_1'] <-     's_p_e_1'
colnames(df.s.2)[colnames(df.s.2) == 's_p_c2_na'] <-    's_p_e_na'

##### recode #####

# table(df.s.2$s_neighborhood)
#Recode s_neighborhood
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 11] <- 1
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 22] <- 2
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 33] <- 3
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 44] <- 4
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 55] <- 5
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 66] <- 6
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 77] <- 7
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 88] <- 8
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 99] <- 9
df.s.2$s_neighborhood[df.s.2$s_neighborhood == 0] <- 10


##### create new variables #####
# Create country or deployment code
df.s.2$dply_num <- 2

df.s.2$s_extra_data <- NA
df.s.2$s_extra_data <- ifelse(!is.na(df.s.2$s_odw_a_3), 1, 0)

# Create floodwater time interval variable
df.s.2$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new s_id variable
df.s.2$s_id <- (1000:(1000+nrow(df.s.2)-1))

# Create unique ID variable
df.s.2$s_UID <- NA

# paste number together
df.s.2$s_UID <- paste0(sprintf("%02d", df.s.2$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.2$s_id)))


# Create date variable
df.s.2$s_date <- NA
df.s.2$s_date <- substr(df.s.2$s_start, 1, 10)
df.s.2$s_date <- as.Date(df.s.2$s_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.s.2$s_participant_female <- ifelse(df.s.2$s_participant_gender == 0, 0, df.s.2$s_participant_female)
df.s.2$s_participant_male <- ifelse(df.s.2$s_participant_gender == 1, 0, df.s.2$s_participant_male)

df.s.2$s_participant_male[df.s.2$s_id == 1003] <- 20
df.s.2$s_participant_male[df.s.2$s_id == 1029] <- 18

df.s.2$s_participant_female[df.s.2$s_id == 1021] <- 15
df.s.2$s_participant_female[df.s.2$s_id == 1032] <- 12
df.s.2$s_participant_female[df.s.2$s_id == 1033] <- 10

##### deployment-specific info #####

extra2 <- which(colnames(df.s.2) %in% setdiff(colnames(df.s.2), colnames(df.s.0)))

# remove "extra" columns from df1
df.s.2.clean <- df.s.2[, -extra2]

# export extra data
df.s.2.extra <- df.s.2[, extra2]
df.s.2.extra$s_UID <- df.s.2$s_UID

##### merge datasets #####
df.s.0$deviceid <- as.numeric(df.s.0$deviceid)
# df.s.0$s_enum <- as.character(df.s.0$s_enum) 
df.s.2.clean$s_enum <- as.character(df.s.2.clean$s_enum) 

df.s <- bind_rows(df.s.0, df.s.2.clean)

# **********************************************************************************
#---- 3 GHANA ----

##### rename variables #####
colnames(df.s.3)[colnames(df.s.3) == 'form.entity']   <- 's_school'
colnames(df.s.3)[colnames(df.s.3) == 'started_time']  <- 's_start'
colnames(df.s.3)[colnames(df.s.3) == 'form.ch_gps']   <- 's_coordinates'
# colnames(df.s.3)[colnames(df.s.3) == 'form.neighbor'] <- 's_neighborhood'

colnames(df.s.3)[colnames(df.s.3) == 'form.num_partic']  <- 's_participant_num'
colnames(df.s.3)[colnames(df.s.3) == 'form.num_fem']     <- 's_participant_female_obs'
colnames(df.s.3)[colnames(df.s.3) == 'form.num_male']    <- 's_participant_male_obs'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q1a'] <- 's_participant_female'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q2a'] <- 's_participant_male'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q3a'] <- 's_neighborhood_y'

# surface water
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q6a'] <- 's_s_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q6c'] <- 's_s_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q6e'] <- 's_s_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q6g'] <- 's_s_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q7a'] <- 's_s_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q7c'] <- 's_s_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q7e'] <- 's_s_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q7g'] <- 's_s_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q7i'] <- 's_s_a_na'

# open drains
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q8a'] <- 's_d_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q8c'] <- 's_d_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q8e'] <- 's_d_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q8g'] <- 's_d_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q9a'] <- 's_d_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q9c'] <- 's_d_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q9e'] <- 's_d_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q9g'] <- 's_d_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q9i'] <- 's_d_a_na'

# floodwater
# NOTE: time frame of answer choices is month instead of week
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q10a'] <- 's_f_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q10c'] <- 's_f_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q10e'] <- 's_f_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q10g'] <- 's_f_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q11a'] <- 's_f_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q11c'] <- 's_f_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q11e'] <- 's_f_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q11g'] <- 's_f_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q11i'] <- 's_f_a_na'

# drinking water
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q12a'] <- 's_dw_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q12c'] <- 's_dw_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q12e'] <- 's_dw_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q12g'] <- 's_dw_c_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q12i'] <- 's_dw_c_na'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13a'] <- 's_dw_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13c'] <- 's_dw_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13e'] <- 's_dw_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13g'] <- 's_dw_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13i'] <- 's_dw_a_na'

# drinking water treatment
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13k'] <- 's_dw_e_wt_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q13m'] <- 's_dw_e_wt_na'

# oceans
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q4a'] <- 's_o_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q4c'] <- 's_o_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q4e'] <- 's_o_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q4g'] <- 's_o_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q5a'] <- 's_o_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q5c'] <- 's_o_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q5e'] <- 's_o_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q5g'] <- 's_o_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q5i'] <- 's_o_a_na'

# produce 
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q14a'] <- 's_p_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q14c'] <- 's_p_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q14e'] <- 's_p_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q14g'] <- 's_p_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q15a'] <- 's_p_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q15c'] <- 's_p_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q15e'] <- 's_p_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q15g'] <- 's_p_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q15i'] <- 's_p_a_na'

# public toilet
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q16a'] <- 's_l_c_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q16c'] <- 's_l_c_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q16e'] <- 's_l_c_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q16g'] <- 's_l_c_0'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17a'] <- 's_l_a_3'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17c'] <- 's_l_a_2'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17e'] <- 's_l_a_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17g'] <- 's_l_a_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17i'] <- 's_l_a_na'

# have a toilet at home
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q17k'] <- 's_pl_a_th_1'

# use toilet at home
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q18a'] <- 's_pl_a_tu_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q18c'] <- 's_pl_a_tu_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q18e'] <- 's_pl_a_tu_nl'

# flush toilet at home
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q19a'] <- 's_pl_a_tw_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q19c'] <- 's_pl_a_tw_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q19e'] <- 's_pl_a_tw_nl'

# toilet at home floods
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q20a'] <- 's_pl_a_tf_1'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q20c'] <- 's_pl_a_tf_0'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q20e'] <- 's_pl_a_tf_nl'

# participants at end
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q25a'] <- 's_participant_female_obs_end'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_q26a'] <- 's_participant_male_obs_end'

colnames(df.s.3)[colnames(df.s.3) == 'form.sch_notes'] <- 's_notes'
colnames(df.s.3)[colnames(df.s.3) == 'form.sch_enumerator_names'] <- 's_enum'

#check for differences (returns those in x but not y)
# setdiff(colnames(df.s.3), colnames(df.s.0))


##### recode #####
# Exclude test data (numeric neighborhood)
df.s.3 <- filter(df.s.3, form.neighbor != 1 & form.neighbor !=2)

# Code s_neighborhood as numeric
neigh <- meta_neighb[meta_neighb$deployment_id == 3 ,][,c("neighborhood_id", "neighborhood")]
colnames(neigh) <- c("s_neighborhood", "s_neigh")

df.s.3 <- left_join(df.s.3, neigh, by=c("form.neighbor" = "s_neigh"))

##### create new variables #####
# Create country or deployment code
df.s.3$dply_num <- 3

# Create floodwater time interval variable
df.s.3$s_f_weekly <- 0    # 1 if weekly, 0 if not weekly


# renumbering
df.s.3$number <- (1000:(1000+nrow(df.s.3)-1))
df.s.3$s_id <- df.s.3$number

# Create unique ID variable
df.s.3$s_UID <- NA

df.s.3$s_UID <- paste0(sprintf("%02d", df.s.3$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.3$number)))

# Create date variable
df.s.3$s_date <- NA
df.s.3$s_date <- substr(df.s.3$s_start, 1, 10)
df.s.3$s_date <- as.Date(df.s.3$s_date, format = "%Y-%m-%d")

# hardcode fixes to female/ male numbers
# 0	All male
# 1	All female
# 2	A combination of male and female
df.s.3$s_participant_gender <- c(2,2,2,2,2,2,2,2,2,0,2,2)


##### deployment-specific info #####
# Remove extra info from DF3 
extra3 <- which(colnames(df.s.3) %in% setdiff(colnames(df.s.3), colnames(df.s.0)))

df.s.3.clean <- df.s.3[, -extra3]

# Export extra data
df.s.3.extra <- df.s.3[, extra3]
df.s.3.extra$s_UID <- df.s.3$s_UID

# Create separate location variables using form.ev_lat_lon (now s_coordinates)
df.s.3 <- separate(df.s.3, s_coordinates, c("X_s_coordinates_latitude","X_s_coordinates_longitude",
                                            "X_s_coordinates_altitude","X_s_coordinates_precision"), 
                   sep=" ", remove = FALSE)

df.s.3$X_s_coordinates_latitude <- as.numeric(df.s.3$X_s_coordinates_latitude)
df.s.3$X_s_coordinates_longitude <- as.numeric(df.s.3$X_s_coordinates_longitude)
df.s.3$X_s_coordinates_altitude <- as.numeric(df.s.3$X_s_coordinates_altitude)
df.s.3$X_s_coordinates_precision <- as.numeric(df.s.3$X_s_coordinates_precision)

##### merge datasets #####
# Adjust column type
# df.s.0$s_neighborhood <- as.character(df.s.0$s_neighborhood)
df.s.3.clean$s_notes <- as.character(df.s.3.clean$s_notes)

# Check that dataframes have same dimensions
setdiff(colnames(df.s.3.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.3.clean)

# **********************************************************************************
#---- 4 ZAMBIA ----
setdiff(colnames(df.s.4), colnames(df.s.0))

##### rename variables #####
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_c_3'] <- 's_odw_c_3'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_c_2'] <- 's_odw_c_2'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_c_1'] <- 's_odw_c_1'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_c_0'] <- 's_odw_c_0'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_c_na'] <- 's_odw_c_na'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_a_3'] <- 's_odw_a_3'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_a_2'] <- 's_odw_a_2'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_a_1'] <- 's_odw_a_1'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_a_0'] <- 's_odw_a_0'
colnames(df.s.4)[colnames(df.s.4) == 's_dwbore_a_na'] <- 's_odw_a_na'

colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_c_3'] <- 's_odw2_c_3'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_c_2'] <- 's_odw2_c_2'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_c_1'] <- 's_odw2_c_1'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_c_0'] <- 's_odw2_c_0'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_c_na'] <- 's_odw2_c_na'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_a_3'] <- 's_odw2_a_3'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_a_2'] <- 's_odw2_a_2'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_a_1'] <- 's_odw2_a_1'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_a_0'] <- 's_odw2_a_0'
colnames(df.s.4)[colnames(df.s.4) == 's_dwwell_a_na'] <- 's_odw2_a_na'

##### recode #####
#indicate extra data
df.s.4$s_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.s.4$dply_num <- 4

# Create floodwater time interval variable
df.s.4$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
df.s.4$s_id <- (1000:(1000+nrow(df.s.4)-1))

# Create unique ID variable
df.s.4$s_UID <- NA

# paste number together
df.s.4$s_UID <- paste0(sprintf("%02d", df.s.4$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.4$s_id)))


# Create date variable
df.s.4$s_date <- NA
df.s.4$s_date <- substr(df.s.4$s_start, 1, 10)
df.s.4$s_date <- as.Date(df.s.4$s_date, format = "%Y-%m-%d")


##### deployment-specific info #####

# Remove extra info from DF4 
extra4 <- which(colnames(df.s.4) %in% setdiff(colnames(df.s.4), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.4.clean <- df.s.4[, -extra4]

# export extra data
df.s.4.extra <- df.s.4[, extra4]
df.s.4.extra$s_UID <- df.s.4$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.4.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.4.clean)

# **********************************************************************************

#---- 6 GHANA Accra -----
setdiff(colnames(df.s.6), colnames(df.s.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.s.6$s_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.s.6$dply_num <- 6

# Create floodwater time interval variable
# df.s.6$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
df.s.6$s_id <- (1000:(1000+nrow(df.s.6)-1))

# Create unique ID variable
df.s.6$s_UID <- NA

# paste number together
df.s.6$s_UID <- paste0(sprintf("%02d", df.s.6$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.6$s_id)))


# Create date variable
df.s.6$s_date <- NA
df.s.6$s_date <- substr(df.s.6$s_start, 1, 10)
df.s.6$s_date <- as.Date(df.s.6$s_date, format = "%Y-%m-%d")


##### deployment-specific info #####
df.s.6$s_neighborhood[df.s.6$s_neighborhood == 268] <- 1
df.s.6$s_neighborhood[df.s.6$s_neighborhood == 269] <- 2

# Remove extra info from DF4 
extra6 <- which(colnames(df.s.6) %in% setdiff(colnames(df.s.6), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.6.clean <- df.s.6[, -extra6]

# export extra data
df.s.6.extra <- df.s.6[, extra6]
df.s.6.extra$s_UID <- df.s.6$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.6.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.6.clean)

# **********************************************************************************
#---- 7 GHANA Kumasi -----
setdiff(colnames(df.s.7), colnames(df.s.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.s.7$s_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.s.7$dply_num <- 7

# Create floodwater time interval variable
# df.s.7$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
df.s.7$s_id <- (1000:(1000+nrow(df.s.7)-1))

# Create unique ID variable
df.s.7$s_UID <- NA

# paste number together
df.s.7$s_UID <- paste0(sprintf("%02d", df.s.7$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.7$s_id)))


# Create date variable
df.s.7$s_date <- NA
df.s.7$s_date <- substr(df.s.7$s_start, 1, 10)
df.s.7$s_date <- as.Date(df.s.7$s_date, format = "%Y-%m-%d")


##### deployment-specific info #####
df.s.7$s_s_c_na[df.s.7$s_s_c_na == 9999] <- 0
df.s.7$s_s_a_na[df.s.7$s_s_a_na == 9999] <- 0


df.s.7$s_neighborhood[df.s.7$s_neighborhood == 280] <- 1
df.s.7$s_neighborhood[df.s.7$s_neighborhood == 281] <- 2
df.s.7$s_neighborhood[df.s.7$s_neighborhood == 282] <- 3
df.s.7$s_neighborhood[df.s.7$s_neighborhood == 283] <- 4

# Remove extra info from DF4 
extra7 <- which(colnames(df.s.7) %in% setdiff(colnames(df.s.7), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.7.clean <- df.s.7[, -extra7]

# export extra data
df.s.7.extra <- df.s.7[, extra7]
df.s.7.extra$s_UID <- df.s.7$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.7.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.7.clean)

# **********************************************************************************
#---- 9 UGANDA Kampala -----
setdiff(colnames(df.s.9), colnames(df.s.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.s.9$s_extra_data <- 1

##### create new variables #####
# Create country or deployment code
df.s.9$dply_num <- 9

# Create floodwater time interval variable
# df.s.9$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly

# Create new h_id variable
df.s.9$s_id <- (1000:(1000+nrow(df.s.9)-1))

# Create unique ID variable
df.s.9$s_UID <- NA

# paste number together
df.s.9$s_UID <- paste0(sprintf("%02d", df.s.9$dply_num),
                       "_",
                       sprintf("%04d", as.numeric(df.s.9$s_id)))


# Create date variable
df.s.9$s_date <- NA
df.s.9$s_date <- substr(df.s.9$s_start, 1, 10)
df.s.9$s_date <- as.Date(df.s.9$s_date, format = "%Y-%m-%d")


##### deployment-specific info #####
df.s.9$s_neighborhood[df.s.9$s_neighborhood == 309] <- 1
df.s.9$s_neighborhood[df.s.9$s_neighborhood == 310] <- 2
df.s.9$s_neighborhood[df.s.9$s_neighborhood == 311] <- 3
df.s.9$s_neighborhood[df.s.9$s_neighborhood == 312] <- 4
df.s.9$s_neighborhood[df.s.9$s_neighborhood == 313] <- 5

# Remove extra info from DF4 
extra9 <- which(colnames(df.s.9) %in% setdiff(colnames(df.s.9), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.9.clean <- df.s.9[, -extra9]

# export extra data
df.s.9.extra <- df.s.9[, extra9]
df.s.9.extra$s_UID <- df.s.9$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.9.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.9.clean)
# **********************************************************************************


# **********************************************************************************
#---- 11 India Vellore ----
setdiff(colnames(df.s.11), colnames(df.s.0))


##### rename variables #####
colnames(df.s.11)[colnames(df.s.11) == 'response_id'] <- 's_id'
colnames(df.s.11)[colnames(df.s.11) == 'neighborhood_id'] <- 's_neighborhood'

colnames(df.s.11)[colnames(df.s.11) == 'response_date'] <- 's_date'
colnames(df.s.11)[colnames(df.s.11) == 'GPS_latitude'] <- 'X_s_coordinates_latitude'
colnames(df.s.11)[colnames(df.s.11) == 'GPS_longitude'] <- 'X_s_coordinates_longitude'
# colnames(df.s.11)[colnames(df.s.11) == 'number_male'] <- 's_participant_male'
# colnames(df.s.11)[colnames(df.s.11) == 'number_female'] <- 's_participant_female'
colnames(df.s.11)[colnames(df.s.11) == 'number_participant'] <- 's_participant_num'


df.s.11$s_date <- as.Date(df.s.11$s_date, "%Y-%m-%d")

##### recode #####
df.s.11$s_participant_gender <- c(2,0,1,2,1,2,2,2)
df.s.11$s_neighborhood_y <- df.s.11$s_participant_num
df.s.11$s_neighborhood_n <- 0

##### create new variables #####
# Create country or deployment code
df.s.11$dply_num <- 11

df.s.11$s_extra_data <- 0

# Create unique ID variable
df.s.11$s_UID <- NA

df.s.11$s_id <- 1000+df.s.11$s_id
df.s.11$s_id_val <- df.s.11$s_id

# paste number together
df.s.11$s_UID <- paste0(sprintf("%02d", df.s.11$dply_num),
                        "_", 
                        sprintf("%04d", df.s.11$s_id))

# hardcode fixes to female/ male numbers
df.s.11$s_participant_male[df.s.11$s_id == 1107] <- 10

##### deployment-specific info #####
# find columns that are in df4 but not in df0
extra11 <- which(colnames(df.s.11) %in% setdiff(colnames(df.s.11), colnames(df.s.0)))

# remove "extra" columns from df
df.s.11.clean <- df.s.11[, -extra11]

# export extra data
df.s.11.extra <- df.s.11[, extra11]
df.s.11.extra$s_UID <- df.s.11$s_UID

##### merge datasets #####
df.s <- bind_rows(df.s, df.s.11.clean)
# **********************************************************************************


# **********************************************************************************
#---- 12 SENEGAL Dakar-----
setdiff(colnames(df.s.12), colnames(df.s.0))

##### rename variables #####

##### recode #####
#indicate extra data
df.s.12$s_extra_data <- 0

df.s.12 <- df.s.12 %>% mutate(s_participant_male = case_when(s_participant_gender == 0 ~ s_participant_num))
df.s.12 <- df.s.12 %>% mutate(s_participant_female = case_when(s_participant_gender == 1 ~ s_participant_num))

df.s.12$s_participant_male[is.na(df.s.12$s_participant_male)] <- 0
df.s.12$s_participant_female[is.na(df.s.12$s_participant_female)] <- 0

df.s.12$s_participant_male_obs <- df.s.12$s_participant_male
df.s.12$s_participant_female_obs <- df.s.12$s_participant_female

##### create new variables #####
# Create country or deployment code
df.s.12$dply_num <- 12

# Create new h_id variable
df.s.12$s_id <- (1000:(1000+nrow(df.s.12)-1))

# Create unique ID variable
df.s.12$s_UID <- NA

# paste number together
df.s.12$s_UID <- paste0(sprintf("%02d", df.s.12$dply_num),
                        "_",
                        sprintf("%04d", as.numeric(df.s.12$s_id)))


# Create date variable
df.s.12$s_date <- NA
df.s.12$s_date <- substr(df.s.12$s_start, 1, 10)
df.s.12$s_date <- as.Date(df.s.12$s_date, format = "%Y-%m-%d")

# 409	WakhinaneNimzatt
# 410	MedinaGounass
# 411	DTK
# 412	RufisqueEst
# 413	SicapLiberte

##### deployment-specific info #####
df.s.12$s_neighborhood[df.s.12$s_neighborhood == 409] <- 1
df.s.12$s_neighborhood[df.s.12$s_neighborhood == 410] <- 2
df.s.12$s_neighborhood[df.s.12$s_neighborhood == 411] <- 3
df.s.12$s_neighborhood[df.s.12$s_neighborhood == 412] <- 4
df.s.12$s_neighborhood[df.s.12$s_neighborhood == 413] <- 5

# Remove extra info from DF4 
extra12 <- which(colnames(df.s.12) %in% setdiff(colnames(df.s.12), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.12.clean <- df.s.12[, -extra12]

# export extra data
df.s.12.extra <- df.s.12[, extra12]
df.s.12.extra$s_UID <- df.s.12$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.12.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.12.clean)
# **********************************************************************************



#---- 13 ZAMBIA 2 ----
setdiff(colnames(df.s.13), colnames(df.s.0))

##### rename variables #####
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_c_3'] <- 's_odw_c_3'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_c_2'] <- 's_odw_c_2'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_c_1'] <- 's_odw_c_1'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_c_0'] <- 's_odw_c_0'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_c_na'] <- 's_odw_c_na'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_a_3'] <- 's_odw_a_3'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_a_2'] <- 's_odw_a_2'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_a_1'] <- 's_odw_a_1'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_a_0'] <- 's_odw_a_0'
colnames(df.s.13)[colnames(df.s.13) == 's_dwbore_a_na'] <- 's_odw_a_na'


colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_c_3'] <- 's_odw2_c_3'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_c_2'] <- 's_odw2_c_2'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_c_1'] <- 's_odw2_c_1'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_c_0'] <- 's_odw2_c_0'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_c_na'] <- 's_odw2_c_na'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_a_3'] <- 's_odw2_a_3'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_a_2'] <- 's_odw2_a_2'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_a_1'] <- 's_odw2_a_1'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_a_0'] <- 's_odw2_a_0'
colnames(df.s.13)[colnames(df.s.13) == 's_dwwell_a_na'] <- 's_odw2_a_na'

##### recode #####
#indicate extra data
df.s.13$s_extra_data <- 1

# neighborhood
df.s.13$s_neighborhood[df.s.13$s_neighborhood == 381] <- 1
df.s.13$s_neighborhood[df.s.13$s_neighborhood == 382] <- 2
df.s.13$s_neighborhood[df.s.13$s_neighborhood == 380] <- 3

##### create new variables #####
# Create country or deployment code
df.s.13$dply_num <- 13

# Create new h_id variable
df.s.13$s_id <- (1000:(1000+nrow(df.s.13)-1))

# Create unique ID variable
df.s.13$s_UID <- NA

# paste number together
df.s.13$s_UID <- paste0(sprintf("%02d", df.s.13$dply_num),
                        "_",
                        sprintf("%04d", as.numeric(df.s.13$s_id)))


# Create date variable
df.s.13$s_date <- NA
df.s.13$s_date <- substr(df.s.13$s_start, 1, 10)
df.s.13$s_date <- as.Date(df.s.13$s_date, format = "%Y-%m-%d")


##### deployment-specific info #####

# Remove extra info from DF
extra13 <- which(colnames(df.s.13) %in% setdiff(colnames(df.s.13), colnames(df.s.0))) 

# remove "extra" columns from DF3
df.s.13.clean <- df.s.13[, -extra13]

# export extra data
df.s.13.extra <- df.s.13[, extra13]
df.s.13.extra$s_UID <- df.s.13$s_UID

##### merge datasets #####

# Check that dataframes have same dimensions
# setdiff(colnames(df.s.13.clean), colnames(df.s))

# Merge clean datasets
df.s <- bind_rows(df.s, df.s.13.clean)

# **********************************************************************************






# **********************************************************************************
# add unique neighborhood numbers (over all projects)
df.s$neighb_UID <- paste0(sprintf("%01d", df.s$dply_num),
                          sprintf("%02d", df.s$s_neighborhood))

df.s$neighb_UID <- as.numeric(df.s$neighb_UID)


df.s <- left_join(df.s, meta_dply[,c(1,4)], by = c("dply_num" = "id"))

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

write.csv(df.s, paste0(getwd(), "/data/", "s_merged_", Sys.Date(), ".csv"), row.names = F, na="")

write.csv(df.s.2.extra, paste0(getwd(), "/data/extra_data/", "s_extra_02.csv"), row.names = F, na="")
write.csv(df.s.3.extra, paste0(getwd(), "/data/extra_data/", "s_extra_03.csv"), row.names = F, na="")
# write.csv(df.s.4.extra, paste0(getwd(), "/data/extra_data/", "s_extra_04.csv"), row.names = F, na="")
# write.csv(df.s.6.extra, paste0(getwd(), "/data/extra_data/", "s_extra_06.csv"), row.names = F, na="")
# write.csv(df.s.7.extra, paste0(getwd(), "/data/extra_data/", "s_extra_07.csv"), row.names = F, na="")
write.csv(df.s.9.extra, paste0(getwd(), "/data/extra_data/", "s_extra_09.csv"), row.names = F, na="")
# write.csv(df.s.12.extra, paste0(getwd(), "/data/extra_data/", "s_extra_12.csv"), row.names = F, na="")
write.csv(df.s.13.extra, paste0(getwd(), "/data/extra_data/", "s_extra_13.csv"), row.names = F, na="")







# # **********************************************************************************
# #---- 99 INDIA KOLKATA -----
# setdiff(colnames(df.s.99), colnames(df.s.0))
# 
# ##### rename variables #####
# 
# ##### recode #####
# #indicate extra data
# df.s.99$s_extra_data <- 1
# 
# ##### create new variables #####
# # Create country or deployment code
# df.s.99$dply_num <- 12
# 
# # Create floodwater time interval variable
# # df.s.99$s_f_weekly <- 1    # 1 if weekly, 0 if not weekly
# 
# # Create new h_id variable
# df.s.99$s_id <- (1000:(1000+nrow(df.s.99)-1))
# 
# # paste number together
# df.s.99$s_UID <- paste0(sprintf("%02d", df.s.99$dply_num),
#                         "_",
#                         sprintf("%04d", as.numeric(df.s.99$s_id)))
# 
# 
# # Create date variable
# df.s.99$s_date <- substr(df.s.99$s_start, 1, 10)
# df.s.99$s_date <- as.Date(df.s.99$s_date, format = "%Y-%m-%d")
# 
# 
# ##### deployment-specific info #####
# # df.s.99$s_neighborhood[df.s.99$s_neighborhood == 280] <- 1
# 
# 
# # Remove extra info from DF4 
# extra12 <- which(colnames(df.s.99) %in% setdiff(colnames(df.s.99), colnames(df.s.0))) 
# 
# # remove "extra" columns from DF3
# df.s.99.clean <- df.s.99[, -extra12]
# 
# # export extra data
# df.s.99.extra <- df.s.99[, extra12]
# df.s.99.extra$s_UID <- df.s.99$s_UID
# 
# ##### merge datasets #####
# 
# # Check that dataframes have same dimensions
# # setdiff(colnames(df.s.99.clean), colnames(df.s))
# 
# # Merge clean datasets
# df.s <- bind_rows(df.s, df.s.99.clean)
