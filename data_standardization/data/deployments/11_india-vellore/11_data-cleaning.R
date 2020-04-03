# import sas

# household survey
library(haven)

# HH ----
hh <- read_sas(paste0(getwd(), "/data/deployments/11_india-vellore/Vellore Public Domain Data/household.sas7bdat"))
hh <- as.data.frame(hh)

# rename questions to variable names:
table(hh$question_id)
table(hh$question_text)

hh$question_text[hh$question_id == 400] <- "h_c" #yn
hh$question_text[hh$question_id == 405] <- "h_d_a" 
hh$question_text[hh$question_id == 406] <- "h_d_c"
hh$question_text[hh$question_id == 407] <- "h_f_a"
hh$question_text[hh$question_id == 408] <- "h_f_c"
hh$question_text[hh$question_id == 409] <- "h_dw_a"
hh$question_text[hh$question_id == 410] <- "h_dw_c"
hh$question_text[hh$question_id == 411] <- "h_dw_e_wt" #yn
hh$question_text[hh$question_id == 412] <- "h_p_a"
hh$question_text[hh$question_id == 413] <- "h_p_c"
hh$question_text[hh$question_id == 414] <- "h_l_a"
hh$question_text[hh$question_id == 415] <- "h_l_c"
hh$question_text[hh$question_id == 416] <- "h_pl_a_th" #yn
hh$question_text[hh$question_id == 417] <- "h_pl_a_tu" #yn
hh$question_text[hh$question_id == 418] <- "h_pl_a_tw" #yn
hh$question_text[hh$question_id == 419] <- "h_pl_a_tf" #yn
hh$question_text[hh$question_id == 421] <- "h_pl_a_od" #select multiple question

# rename answer choices
table(hh$choice_text)


hh$choice_text[hh$choice_text == "No"] <- 0
hh$choice_text[hh$choice_text == "Yes"] <- 1

hh$choice_text[grep("16a",hh$choice_text)] <- 1 #10+
hh$choice_text[grep("16b",hh$choice_text)] <- 2 #6-10
hh$choice_text[grep("16c",hh$choice_text)] <- 3 #1-5
hh$choice_text[grep("16d",hh$choice_text)] <- 4 #never
hh$choice_text[grep("16e",hh$choice_text)] <- 5 #do not know

hh$choice_text[grep("6a",hh$choice_text)] <- 1
hh$choice_text[grep("6b",hh$choice_text)] <- 2
hh$choice_text[grep("6c",hh$choice_text)] <- 3
hh$choice_text[grep("6d",hh$choice_text)] <- 4

hh$choice_text[grep("7a",hh$choice_text)] <- 1
hh$choice_text[grep("7b",hh$choice_text)] <- 2
hh$choice_text[grep("7c",hh$choice_text)] <- 3
hh$choice_text[grep("7d",hh$choice_text)] <- 4
hh$choice_text[grep("7e",hh$choice_text)] <- 5

hh$choice_text[grep("8a",hh$choice_text)] <- 1
hh$choice_text[grep("8b",hh$choice_text)] <- 2
hh$choice_text[grep("8c",hh$choice_text)] <- 3
hh$choice_text[grep("8d",hh$choice_text)] <- 4

hh$choice_text[grep("9a",hh$choice_text)] <- 1
hh$choice_text[grep("9b",hh$choice_text)] <- 2
hh$choice_text[grep("9c",hh$choice_text)] <- 3
hh$choice_text[grep("9d",hh$choice_text)] <- 4
hh$choice_text[grep("9e",hh$choice_text)] <- 5

hh$choice_text[grep("10a",hh$choice_text)] <- 1
hh$choice_text[grep("10b",hh$choice_text)] <- 2
hh$choice_text[grep("10c",hh$choice_text)] <- 3
hh$choice_text[grep("10d",hh$choice_text)] <- 4
hh$choice_text[grep("10e",hh$choice_text)] <- 5

hh$choice_text[grep("11a",hh$choice_text)] <- 1
hh$choice_text[grep("11b",hh$choice_text)] <- 2
hh$choice_text[grep("11c",hh$choice_text)] <- 3
hh$choice_text[grep("11d",hh$choice_text)] <- 4
hh$choice_text[grep("11e",hh$choice_text)] <- 5

hh$choice_text[grep("13a",hh$choice_text)] <- 1
hh$choice_text[grep("13b",hh$choice_text)] <- 2
hh$choice_text[grep("13c",hh$choice_text)] <- 3
hh$choice_text[grep("13d",hh$choice_text)] <- 4

hh$choice_text[grep("14a",hh$choice_text)] <- 1
hh$choice_text[grep("14b",hh$choice_text)] <- 2
hh$choice_text[grep("14c",hh$choice_text)] <- 3
hh$choice_text[grep("14d",hh$choice_text)] <- 4
hh$choice_text[grep("14e",hh$choice_text)] <- 5

hh$choice_text[grep("15a",hh$choice_text)] <- 1
hh$choice_text[grep("15b",hh$choice_text)] <- 2
hh$choice_text[grep("15c",hh$choice_text)] <- 3
hh$choice_text[grep("15d",hh$choice_text)] <- 4

# open defecation
hh$choice_text[grep("22a",hh$choice_text)] <- 5
hh$choice_text[grep("22b",hh$choice_text)] <- 1
hh$choice_text[grep("22c",hh$choice_text)] <- 2
hh$choice_text[grep("22e",hh$choice_text)] <- 4


hh$question_text <- ifelse(hh$question_text== "h_pl_a_od" & hh$choice_text == 1, "h_pl_a_od.1", hh$question_text)
hh$question_text <- ifelse(hh$question_text== "h_pl_a_od" & hh$choice_text == 2, "h_pl_a_od.2", hh$question_text)
hh$question_text <- ifelse(hh$question_text== "h_pl_a_od" & hh$choice_text == 4, "h_pl_a_od.4", hh$question_text)
hh$question_text <- ifelse(hh$question_text== "h_pl_a_od" & hh$choice_text == 5, "h_pl_a_od.5", hh$question_text)


write.csv(hh, paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/", "hh.csv"), row.names = F, na="")
hh <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/", "hh.csv"))

neighb <- hh %>% select(response_id, neighborhood_id)
neighb <- neighb[!duplicated(neighb$response_id),]



# rearrange data
hh <- hh %>% select(response_id, question_text, choice_text)
hh <- hh %>% spread(question_text, choice_text)

hh <- left_join(hh, neighb, by = "response_id")


hh <- hh %>% select(response_id, neighborhood_id, everything())
table(hh$neighborhood_id)



# School ----
school <- read_sas(paste0(getwd(), "/data/deployments/11_india-vellore/Vellore Public Domain Data/school.sas7bdat"))
school <- as.data.frame(school)

table(school$question_id)
table(school$question_text)

school1 <- school %>% select(response_id, question_id, question_text, count_yes, number_participant)
table(school1$question_id)
table(school1$question_text)

#variable names
school1$question_text[school1$question_id == 1] <- "s_participant_female"
school1$question_text[school1$question_id == 2] <- "s_participant_male"
school1$question_text[school1$question_id == 3] <- "s_neighborhood_y"

school1$question_text[school1$question_id == 27] <- "s_d_c_3"
school1$question_text[school1$question_id == 28] <- "s_d_c_2"
school1$question_text[school1$question_id == 29] <- "s_d_c_1"
school1$question_text[school1$question_id == 30] <- "s_d_c_0"
school1$question_text[school1$question_id == 32] <- "s_d_a_3"
school1$question_text[school1$question_id == 33] <- "s_d_a_2"
school1$question_text[school1$question_id == 34] <- "s_d_a_1"
school1$question_text[school1$question_id == 35] <- "s_d_a_0"
school1$question_text[school1$question_id == 36] <- "s_d_a_na"

school1$question_text[school1$question_id == 38] <- "s_f_c_3"
school1$question_text[school1$question_id == 39] <- "s_f_c_2"
school1$question_text[school1$question_id == 40] <- "s_f_c_1"
school1$question_text[school1$question_id == 41] <- "s_f_c_0"
school1$question_text[school1$question_id == 43] <- "s_f_a_3"
school1$question_text[school1$question_id == 44] <- "s_f_a_2"
school1$question_text[school1$question_id == 45] <- "s_f_a_1"
school1$question_text[school1$question_id == 46] <- "s_f_a_0"
school1$question_text[school1$question_id == 47] <- "s_f_a_na"

school1$question_text[school1$question_id == 49] <- "s_dw_c_3"
school1$question_text[school1$question_id == 50] <- "s_dw_c_2"
school1$question_text[school1$question_id == 51] <- "s_dw_c_1"
school1$question_text[school1$question_id == 52] <- "s_dw_c_0"
school1$question_text[school1$question_id == 53] <- "s_dw_c_na"
school1$question_text[school1$question_id == 55] <- "s_dw_a_3"
school1$question_text[school1$question_id == 56] <- "s_dw_a_2"
school1$question_text[school1$question_id == 57] <- "s_dw_a_1"
school1$question_text[school1$question_id == 58] <- "s_dw_a_0"
school1$question_text[school1$question_id == 59] <- "s_dw_a_na"

school1$question_text[school1$question_id == 60] <- "s_dw_e_wt_1" #need _0 answer

school1$question_text[school1$question_id == 62] <- "s_p_c_3"
school1$question_text[school1$question_id == 63] <- "s_p_c_2"
school1$question_text[school1$question_id == 64] <- "s_p_c_1"
school1$question_text[school1$question_id == 65] <- "s_p_c_0"
school1$question_text[school1$question_id == 67] <- "s_p_a_3"
school1$question_text[school1$question_id == 68] <- "s_p_a_2"
school1$question_text[school1$question_id == 69] <- "s_p_a_1"
school1$question_text[school1$question_id == 70] <- "s_p_a_0"
school1$question_text[school1$question_id == 71] <- "s_p_a_na"

school1$question_text[school1$question_id == 73] <- "s_l_c_3"
school1$question_text[school1$question_id == 74] <- "s_l_c_2"
school1$question_text[school1$question_id == 75] <- "s_l_c_1"
school1$question_text[school1$question_id == 76] <- "s_l_c_0"

school1$question_text[school1$question_id == 78] <- "s_l_a_3"
school1$question_text[school1$question_id == 79] <- "s_l_a_2"
school1$question_text[school1$question_id == 80] <- "s_l_a_1"
school1$question_text[school1$question_id == 81] <- "s_l_a_0"
school1$question_text[school1$question_id == 82] <- "s_l_a_na"

school1$question_text[school1$question_id == 83] <- "s_pl_a_th_1" #need _0 answer
school1$question_text[school1$question_id == 84] <- "s_pl_a_tu_1" #need _0 answer
school1$question_text[school1$question_id == 85] <- "s_pl_a_tw_1" #need _0 answer
school1$question_text[school1$question_id == 86] <- "s_pl_a_tf_1" #need _0 answer

# school1$question_text[school1$question_id == 88] <- "" # not in survey anymore # need to delete
# school1$question_text[school1$question_id == 89] <- ""
# school1$question_text[school1$question_id == 90] <- ""
# school1$question_text[school1$question_id == 92] <- ""
# school1$question_text[school1$question_id == 93] <- ""
# school1$question_text[school1$question_id == 94] <- ""
# school1$question_text[school1$question_id == 95] <- ""
# school1$question_text[school1$question_id == 96] <- ""
# school1$question_text[school1$question_id == 99] <- ""

#delete questions
school1 <- school1[ !(school1$question_id %in% c(88,89,90,92,93,94,95,96,99)), ]

school1$question_text[school1$question_id == 97] <- "s_participant_female_obs_end"
school1$question_text[school1$question_id == 98] <- "s_participant_male_obs_end"

#add missing questions with _0 ending - manually
{
        wt1 <- data.frame( 61, 160, "s_dw_e_wt_0", 22, 22)
        wt2 <- data.frame(106, 160, "s_dw_e_wt_0", 8, 20)
        wt3 <- data.frame(107, 160, "s_dw_e_wt_0", 6, 19)
        wt4 <- data.frame(108, 160, "s_dw_e_wt_0", 5, 16)
        wt5 <- data.frame(109, 160, "s_dw_e_wt_0", 5, 22)
        wt6 <- data.frame(110, 160, "s_dw_e_wt_0", 6, 13)
        wt7 <- data.frame(111, 160, "s_dw_e_wt_0", 3, 19)
        wt8 <- data.frame(112, 160, "s_dw_e_wt_0", 8, 20)
        
        school1 <- rbind(school1, setNames(wt1, names(school1)))
        school1 <- rbind(school1, setNames(wt2, names(school1)))
        school1 <- rbind(school1, setNames(wt3, names(school1)))
        school1 <- rbind(school1, setNames(wt4, names(school1)))
        school1 <- rbind(school1, setNames(wt5, names(school1)))
        school1 <- rbind(school1, setNames(wt6, names(school1)))
        school1 <- rbind(school1, setNames(wt7, names(school1)))
        school1 <- rbind(school1, setNames(wt8, names(school1)))    
}


{
        wt1 <- data.frame( 61, 160, "s_pl_a_th_0", 9, 22)
        wt2 <- data.frame(106, 160, "s_pl_a_th_0", 0, 20)
        wt3 <- data.frame(107, 160, "s_pl_a_th_0", 6, 19)
        wt4 <- data.frame(108, 160, "s_pl_a_th_0", 8, 16)
        wt5 <- data.frame(109, 160, "s_pl_a_th_0", 0, 22)
        wt6 <- data.frame(110, 160, "s_pl_a_th_0", 3, 13)
        wt7 <- data.frame(111, 160, "s_pl_a_th_0", 5, 19)
        wt8 <- data.frame(112, 160, "s_pl_a_th_0", 13, 20)
        
        school1 <- rbind(school1, setNames(wt1, names(school1)))
        school1 <- rbind(school1, setNames(wt2, names(school1)))
        school1 <- rbind(school1, setNames(wt3, names(school1)))
        school1 <- rbind(school1, setNames(wt4, names(school1)))
        school1 <- rbind(school1, setNames(wt5, names(school1)))
        school1 <- rbind(school1, setNames(wt6, names(school1)))
        school1 <- rbind(school1, setNames(wt7, names(school1)))
        school1 <- rbind(school1, setNames(wt8, names(school1)))
}

{
        wt1 <- data.frame( 61, 160, "s_pl_a_tu_0", 9, 22)
        wt2 <- data.frame(106, 160, "s_pl_a_tu_0", 0, 20)
        wt3 <- data.frame(107, 160, "s_pl_a_tu_0", 6, 19)
        wt4 <- data.frame(108, 160, "s_pl_a_tu_0", 4, 16)
        wt5 <- data.frame(109, 160, "s_pl_a_tu_0", 0, 22)
        wt6 <- data.frame(110, 160, "s_pl_a_tu_0", 3, 13)
        wt7 <- data.frame(111, 160, "s_pl_a_tu_0", 5, 19)
        wt8 <- data.frame(112, 160, "s_pl_a_tu_0", 13, 20)
        
        school1 <- rbind(school1, setNames(wt1, names(school1)))
        school1 <- rbind(school1, setNames(wt2, names(school1)))
        school1 <- rbind(school1, setNames(wt3, names(school1)))
        school1 <- rbind(school1, setNames(wt4, names(school1)))
        school1 <- rbind(school1, setNames(wt5, names(school1)))
        school1 <- rbind(school1, setNames(wt6, names(school1)))
        school1 <- rbind(school1, setNames(wt7, names(school1)))
        school1 <- rbind(school1, setNames(wt8, names(school1)))
}

{
        wt1 <- data.frame( 61, 160, "s_pl_a_tw_0", 9, 22)
        wt2 <- data.frame(106, 160, "s_pl_a_tw_0", 0, 20)
        wt3 <- data.frame(107, 160, "s_pl_a_tw_0", 6, 19)
        wt4 <- data.frame(108, 160, "s_pl_a_tw_0", 4, 16)
        wt5 <- data.frame(109, 160, "s_pl_a_tw_0", 0, 22)
        wt6 <- data.frame(110, 160, "s_pl_a_tw_0", 3, 13)
        wt7 <- data.frame(111, 160, "s_pl_a_tw_0", 5, 19)
        wt8 <- data.frame(112, 160, "s_pl_a_tw_0", 13, 20)
        
        school1 <- rbind(school1, setNames(wt1, names(school1)))
        school1 <- rbind(school1, setNames(wt2, names(school1)))
        school1 <- rbind(school1, setNames(wt3, names(school1)))
        school1 <- rbind(school1, setNames(wt4, names(school1)))
        school1 <- rbind(school1, setNames(wt5, names(school1)))
        school1 <- rbind(school1, setNames(wt6, names(school1)))
        school1 <- rbind(school1, setNames(wt7, names(school1)))
        school1 <- rbind(school1, setNames(wt8, names(school1)))
}

{
        wt1 <- data.frame( 61, 160, "s_pl_a_tf_0", 21, 22)
        wt2 <- data.frame(106, 160, "s_pl_a_tf_0", 20, 20)
        wt3 <- data.frame(107, 160, "s_pl_a_tf_0", 18, 19)
        wt4 <- data.frame(108, 160, "s_pl_a_tf_0", 15, 16)
        wt5 <- data.frame(109, 160, "s_pl_a_tf_0", 22, 22)
        wt6 <- data.frame(110, 160, "s_pl_a_tf_0", 13, 13)
        wt7 <- data.frame(111, 160, "s_pl_a_tf_0", 18, 19)
        wt8 <- data.frame(112, 160, "s_pl_a_tf_0", 20, 20)
        
        school1 <- rbind(school1, setNames(wt1, names(school1)))
        school1 <- rbind(school1, setNames(wt2, names(school1)))
        school1 <- rbind(school1, setNames(wt3, names(school1)))
        school1 <- rbind(school1, setNames(wt4, names(school1)))
        school1 <- rbind(school1, setNames(wt5, names(school1)))
        school1 <- rbind(school1, setNames(wt6, names(school1)))
        school1 <- rbind(school1, setNames(wt7, names(school1)))
        school1 <- rbind(school1, setNames(wt8, names(school1)))
}

neighb <- school %>% select(response_id, neighborhood_id)
neighb <- neighb[!duplicated(neighb$response_id),]



# rearrange data
school1 <- school1 %>% select(response_id, question_text, count_yes)
school1 <- school1 %>% spread(question_text, count_yes)
school1 <- left_join(school1, neighb, by = "response_id")

particip <- school %>% select(response_id, number_participant, number_male, number_female)
particip <- particip[!duplicated(particip$response_id),]
school1 <- left_join(school1, particip, by = "response_id")

school1 <- school1 %>% select(response_id, neighborhood_id, number_participant, number_male, number_female, everything())
table(school1$neighborhood_id)


# community ----
comm <- read_sas(paste0(getwd(), "/data/deployments/11_india-vellore/Vellore Public Domain Data/community.sas7bdat"))
comm <- as.data.frame(comm)

table(comm$question_id)
table(comm$question_text)

comm1 <- comm %>% select(response_id, question_id, question_text, count_yes, number_participant)
table(comm1$question_id)
table(comm1$question_text)

#variable names
comm1$question_text[comm1$question_id == 200] <- "c_participant_female"
comm1$question_text[comm1$question_id == 201] <- "c_participant_male"
comm1$question_text[comm1$question_id == 202] <- "c_neighborhood_y"
comm1$question_text[comm1$question_id == 203] <- "c_c_y"

comm1$question_text[comm1$question_id == 227] <- "c_d_a_3"
comm1$question_text[comm1$question_id == 228] <- "c_d_a_2"
comm1$question_text[comm1$question_id == 229] <- "c_d_a_1"
comm1$question_text[comm1$question_id == 230] <- "c_d_a_0"
comm1$question_text[comm1$question_id == 232] <- "c_d_c_3"
comm1$question_text[comm1$question_id == 233] <- "c_d_c_2"
comm1$question_text[comm1$question_id == 234] <- "c_d_c_1"
comm1$question_text[comm1$question_id == 235] <- "c_d_c_0"
comm1$question_text[comm1$question_id == 236] <- "c_d_c_na"

comm1$question_text[comm1$question_id == 238] <- "c_f_a_3"
comm1$question_text[comm1$question_id == 239] <- "c_f_a_2"
comm1$question_text[comm1$question_id == 240] <- "c_f_a_1"
comm1$question_text[comm1$question_id == 241] <- "c_f_a_0"
comm1$question_text[comm1$question_id == 243] <- "c_f_c_3"
comm1$question_text[comm1$question_id == 244] <- "c_f_c_2"
comm1$question_text[comm1$question_id == 245] <- "c_f_c_1"
comm1$question_text[comm1$question_id == 246] <- "c_f_c_0"
comm1$question_text[comm1$question_id == 247] <- "c_f_c_na"

comm1$question_text[comm1$question_id == 249] <- "c_dw_a_3"
comm1$question_text[comm1$question_id == 250] <- "c_dw_a_2"
comm1$question_text[comm1$question_id == 251] <- "c_dw_a_1"
comm1$question_text[comm1$question_id == 252] <- "c_dw_a_0"
comm1$question_text[comm1$question_id == 253] <- "c_dw_a_na"
comm1$question_text[comm1$question_id == 255] <- "c_dw_c_3"
comm1$question_text[comm1$question_id == 256] <- "c_dw_c_2"
comm1$question_text[comm1$question_id == 257] <- "c_dw_c_1"
comm1$question_text[comm1$question_id == 258] <- "c_dw_c_0"
comm1$question_text[comm1$question_id == 259] <- "c_dw_c_na"
comm1$question_text[comm1$question_id == 260] <- "c_dw_e_wt_1"

comm1$question_text[comm1$question_id == 262] <- "c_p_a_3"
comm1$question_text[comm1$question_id == 263] <- "c_p_a_2"
comm1$question_text[comm1$question_id == 264] <- "c_p_a_1"
comm1$question_text[comm1$question_id == 265] <- "c_p_a_0"
comm1$question_text[comm1$question_id == 267] <- "c_p_c_3"
comm1$question_text[comm1$question_id == 268] <- "c_p_c_2"
comm1$question_text[comm1$question_id == 269] <- "c_p_c_1"
comm1$question_text[comm1$question_id == 270] <- "c_p_c_0"
comm1$question_text[comm1$question_id == 271] <- "c_p_c_na"

comm1$question_text[comm1$question_id == 273] <- "c_l_a_3"
comm1$question_text[comm1$question_id == 274] <- "c_l_a_2"
comm1$question_text[comm1$question_id == 275] <- "c_l_a_1"
comm1$question_text[comm1$question_id == 276] <- "c_l_a_0"

comm1$question_text[comm1$question_id == 278] <- "c_l_c_3"
comm1$question_text[comm1$question_id == 279] <- "c_l_c_2"
comm1$question_text[comm1$question_id == 280] <- "c_l_c_1"
comm1$question_text[comm1$question_id == 281] <- "c_l_c_0"
comm1$question_text[comm1$question_id == 282] <- "c_l_c_na"
comm1$question_text[comm1$question_id == 283] <- "c_pl_a_th_1"
comm1$question_text[comm1$question_id == 284] <- "c_pl_a_tu_1"
comm1$question_text[comm1$question_id == 285] <- "c_pl_a_tw_1"
comm1$question_text[comm1$question_id == 286] <- "c_pl_a_tf_1"

# comm1$question_text[comm1$question_id == 288] <- "" 
# comm1$question_text[comm1$question_id == 289] <- ""
# comm1$question_text[comm1$question_id == 290] <- ""
# 
# comm1$question_text[comm1$question_id == 292] <- ""
# comm1$question_text[comm1$question_id == 293] <- ""
# comm1$question_text[comm1$question_id == 294] <- ""
# comm1$question_text[comm1$question_id == 295] <- ""
# comm1$question_text[comm1$question_id == 296] <- ""

comm1$question_text[comm1$question_id == 297] <- "c_participant_female_obs_end"
comm1$question_text[comm1$question_id == 298] <- "c_participant_male_obs_end"
# comm1$question_text[comm1$question_id == 299] <- ""

#delete questions
comm1 <- comm1[ !(comm1$question_id %in% c(288,289,290,292,293,294,295,296,299)), ]

#add missing questions with _0 ending - manually
{
        wt1 <- data.frame( 57, 360, "c_dw_e_wt_0", 17, 18)
        wt2 <- data.frame( 58, 360, "c_dw_e_wt_0", 13, 18)
        wt3 <- data.frame( 59, 360, "c_dw_e_wt_0", 9, 12)
        wt4 <- data.frame( 60, 360, "c_dw_e_wt_0", 10, 16)
        wt5 <- data.frame(113, 360, "c_dw_e_wt_0", 2, 14)
        wt6 <- data.frame(114, 360, "c_dw_e_wt_0", 10, 12)
        wt7 <- data.frame(115, 360, "c_dw_e_wt_0", 4, 14)
        wt8 <- data.frame(216, 360, "c_dw_e_wt_0", 11, 13)
        
        comm1 <- rbind(comm1, setNames(wt1, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt2, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt3, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt4, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt5, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt6, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt7, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt8, names(comm1)))    
}

{
        wt1 <- data.frame( 57, 360, "c_pl_a_th_0", 12, 18)
        wt2 <- data.frame( 58, 360, "c_pl_a_th_0", 9, 18)
        wt3 <- data.frame( 59, 360, "c_pl_a_th_0", 8, 12)
        wt4 <- data.frame( 60, 360, "c_pl_a_th_0", 7, 16)
        wt5 <- data.frame(113, 360, "c_pl_a_th_0", 6, 14)
        wt6 <- data.frame(114, 360, "c_pl_a_th_0", 2, 12)
        wt7 <- data.frame(115, 360, "c_pl_a_th_0", 2, 14)
        wt8 <- data.frame(216, 360, "c_pl_a_th_0", 1, 13)
        
        comm1 <- rbind(comm1, setNames(wt1, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt2, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt3, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt4, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt5, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt6, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt7, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt8, names(comm1)))    
}

{
        wt1 <- data.frame( 57, 360, "c_pl_a_tu_0", 12, 18)
        wt2 <- data.frame( 58, 360, "c_pl_a_tu_0", 9, 18)
        wt3 <- data.frame( 59, 360, "c_pl_a_tu_0", 7, 12)
        wt4 <- data.frame( 60, 360, "c_pl_a_tu_0", 7, 16)
        wt5 <- data.frame(113, 360, "c_pl_a_tu_0", 7, 14)
        wt6 <- data.frame(114, 360, "c_pl_a_tu_0", 2, 12)
        wt7 <- data.frame(115, 360, "c_pl_a_tu_0", 2, 14)
        wt8 <- data.frame(216, 360, "c_pl_a_tu_0", 1, 13)
        
        comm1 <- rbind(comm1, setNames(wt1, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt2, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt3, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt4, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt5, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt6, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt7, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt8, names(comm1)))    
}

{
        wt1 <- data.frame( 57, 360, "c_pl_a_tw_0", 13, 18)
        wt2 <- data.frame( 58, 360, "c_pl_a_tw_0", 9, 18)
        wt3 <- data.frame( 59, 360, "c_pl_a_tw_0", 9, 12)
        wt4 <- data.frame( 60, 360, "c_pl_a_tw_0", 7, 16)
        wt5 <- data.frame(113, 360, "c_pl_a_tw_0", 6, 14)
        wt6 <- data.frame(114, 360, "c_pl_a_tw_0", 2, 12)
        wt7 <- data.frame(115, 360, "c_pl_a_tw_0", 2, 14)
        wt8 <- data.frame(216, 360, "c_pl_a_tw_0", 1, 13)
        
        comm1 <- rbind(comm1, setNames(wt1, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt2, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt3, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt4, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt5, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt6, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt7, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt8, names(comm1)))    
}

{
        wt1 <- data.frame( 57, 360, "c_pl_a_tf_0", 16, 18)
        wt2 <- data.frame( 58, 360, "c_pl_a_tf_0", 18, 18)
        wt3 <- data.frame( 59, 360, "c_pl_a_tf_0", 11, 12)
        wt4 <- data.frame( 60, 360, "c_pl_a_tf_0", 16, 16)
        wt5 <- data.frame(113, 360, "c_pl_a_tf_0", 12, 14)
        wt6 <- data.frame(114, 360, "c_pl_a_tf_0", 10, 12)
        wt7 <- data.frame(115, 360, "c_pl_a_tf_0", 12, 14)
        wt8 <- data.frame(216, 360, "c_pl_a_tf_0", 9, 13)
        
        comm1 <- rbind(comm1, setNames(wt1, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt2, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt3, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt4, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt5, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt6, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt7, names(comm1)))
        comm1 <- rbind(comm1, setNames(wt8, names(comm1)))    
}

neighb <- comm %>% select(response_id, neighborhood_id)
neighb <- neighb[!duplicated(neighb$response_id),]



# rearrange data
comm1 <- comm1 %>% select(response_id, question_text, count_yes)
comm1 <- comm1 %>% spread(question_text, count_yes)
comm1 <- left_join(comm1, neighb, by = "response_id")

particip <- comm %>% select(response_id, number_participant, number_male, number_female)
particip <- particip[!duplicated(particip$response_id),]
comm1 <- left_join(comm1, particip, by = "response_id")

comm1 <- comm1 %>% select(response_id, neighborhood_id, number_participant, number_male, number_female, everything())
table(comm1$neighborhood_id)



# add gps and dates to all files

meta.hh <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/hh_metainfo.csv"))
meta.hh <- meta.hh %>% select(response_id, response_date, GPS_latitude, GPS_longitude)
hh <- left_join(hh, meta.hh, by = "response_id")
hh$response_date <- as.Date(hh$response_date, format = "%m/%d/%Y")
write.csv(hh, paste0(getwd(), "/data/deployments/11_india-vellore/", "hh_clean.csv"), row.names = F, na="")



meta.sc <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/school_metainfo.csv"))
meta.sc <- meta.sc %>% select(response_id, response_date, GPS_latitude, GPS_longitude)
school1 <- left_join(school1, meta.sc, by = "response_id")
school1$response_date <- as.Date(school1$response_date, format = "%m/%d/%Y")
write.csv(school1, paste0(getwd(), "/data/deployments/11_india-vellore/", "school_clean.csv"), row.names = F, na="")

meta.cc <- read.csv(paste0(getwd(), "/data/deployments/11_india-vellore/raw_data/community_metainfo.csv"))
meta.cc <- meta.cc %>% select(response_id, response_date, GPS_latitude, GPS_longitude)
comm1 <- left_join(comm1, meta.cc, by = "response_id")
comm1$response_date <- as.Date(comm1$response_date, format = "%m/%d/%Y")
write.csv(comm1, paste0(getwd(), "/data/deployments/11_india-vellore/", "community_clean.csv"), row.names = F, na="")


