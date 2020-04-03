# *************************************************************
# hh survey
# Wolfgang 01/09/18
# *************************************************************

setwd("H:/Work/stat/SaniPath Dhaka/")

hh_data <- read.csv("data/HHFinal_1.8.18.csv", header=TRUE)

# check for duplicates
df.dup <- hh_data[which(hh_data$h_id %in% as.numeric(names(table(hh_data$h_id)[table(hh_data$h_id)>=2]))),]
#ID 432 is a duplicate

table(hh_data$h_neighborhood)

table(hh_data$h_population)
# ID 173: population is 173 ... this must be a mistake
# ID 826: population is 50 ... checked scanned copy, it says 50.

# population is 0 for all the paper forms, where the enumerators have no or partial
#  answers from the forms that were given to children to bring home or do not have 
# population is 0 for 37 data points. 

df.pop <- hh_data[hh_data$h_population == 0, ]

table(hh_data$h_s_a)

hh_data$h_s_a[hh_data$h_s_a == "other"] <- 6
hh_data$h_s_c[hh_data$h_s_c == "other"] <- 6
hh_data$h_d_a[hh_data$h_d_a == "other"] <- 6
hh_data$h_d_c[hh_data$h_d_c == "other"] <- 6
hh_data$h_f_a[hh_data$h_f_a == "other"] <- 6
hh_data$h_f_c[hh_data$h_f_c == "other"] <- 6
hh_data$h_m_a[hh_data$h_m_a == "other"] <- 6
hh_data$h_m_c[hh_data$h_m_c == "other"] <- 6
hh_data$h_odw_a[hh_data$h_odw_a == "other"] <- 6
hh_data$h_odw_c[hh_data$h_odw_c == "other"] <- 6
hh_data$h_p_a[hh_data$h_p_a == "other"] <- 6
hh_data$h_p_c[hh_data$h_p_c == "other"] <- 6
hh_data$h_sf_a[hh_data$h_sf_a == "other"] <- 6
hh_data$h_sf_c[hh_data$h_sf_c == "other"] <- 6
hh_data$h_l_a[hh_data$h_l_a == "other"] <- 6
hh_data$h_l_c[hh_data$h_l_c == "other"] <- 6
hh_data$h_produce[hh_data$h_produce == "`"] <- 1

table(hh_data$h_tu_a)



hh_data$h_s_a <- as.numeric(as.character(hh_data$h_s_a))
hh_data$h_s_c <- as.numeric(as.character(hh_data$h_s_c))
hh_data$h_d_a <- as.numeric(as.character(hh_data$h_d_a))
hh_data$h_d_c <- as.numeric(as.character(hh_data$h_d_c))
hh_data$h_f_a <- as.numeric(as.character(hh_data$h_f_a))
hh_data$h_f_c <- as.numeric(as.character(hh_data$h_f_c))
hh_data$h_m_a <- as.numeric(as.character(hh_data$h_m_a))
hh_data$h_m_c <- as.numeric(as.character(hh_data$h_m_c))
hh_data$h_odw_a <- as.numeric(as.character(hh_data$h_odw_a))
hh_data$h_odw_c <- as.numeric(as.character(hh_data$h_odw_c))
hh_data$h_p_a <- as.numeric(as.character(hh_data$h_p_a))
hh_data$h_p_c <- as.numeric(as.character(hh_data$h_p_c))
hh_data$h_sf_a <- as.numeric(as.character(hh_data$h_sf_a))
hh_data$h_sf_c <- as.numeric(as.character(hh_data$h_sf_c))
hh_data$h_l_a <- as.numeric(as.character(hh_data$h_l_a))
hh_data$h_l_c <- as.numeric(as.character(hh_data$h_l_c))
hh_data$h_produce <- as.numeric(as.character(hh_data$h_produce))

sum(is.na(hh_data$h_s_a))
sum(is.na(hh_data$h_s_c))
sum(is.na(hh_data$h_d_a))
sum(is.na(hh_data$h_d_c))
sum(is.na(hh_data$h_f_a))
sum(is.na(hh_data$h_f_c))
sum(is.na(hh_data$h_m_a))
sum(is.na(hh_data$h_m_c))
sum(is.na(hh_data$h_odw_a))
sum(is.na(hh_data$h_odw_c))
sum(is.na(hh_data$h_p_a))
sum(is.na(hh_data$h_p_c))
sum(is.na(hh_data$h_sf_a))
sum(is.na(hh_data$h_sf_c))
sum(is.na(hh_data$h_l_a))
sum(is.na(hh_data$h_l_c))

write.csv(hh_data, "hh_data_011018_wm.csv", row.names = F, na = "")
