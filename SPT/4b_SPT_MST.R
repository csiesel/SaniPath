source("~/Desktop/SaniPath/SPT/3_SPT_Merge.R")

#parameters;
sub.TNTC <- 300
sub.TDTC <- 300
sub.complete.lysis <- 300 #replace complete lysis with 300?
sub.LLOD <- 0.5

#calculate MST concentration in the sample tested (count/mL);
#This need to be recalculated by sample type using denominators;
calc_3.2 <- mst

#Complete lysis was coded as 3 as reading and 0 as count; 
#Need to change as 4 as reading and NA as count;
#WG5
wg1_issue_id <- c("SW1008","SF1006","SL1009","PLC1027","PLC1028","PLC1029","PLC1030","PLC1032","PLC1033",
                  "PLC1034","PLC1035","PLC1036","PLC1042","PLC1043","PLC1045","PLC1046","PLC1047","PLC1049",
                  "PLC1051","PLC1052","PLC1053","PLC1055","PLC1058","PLC1060","PLC1062","PLC1063","PLC1064",
                  "PLC1065","PLC1066","PLC1067","PLC1068","PLC1071","PLC1072","PLC1074","PLC1076","PLC1077",
                  "PLC1078","PLC1079","PLC1083","PLC1085","PLC1086","PLC1089","PLC1092","PLC1097","PLC1098",
                  "PLC1101","PLC1105","PLC1106","PLC1107","PLC1112","PLC1119","PLC1121","PLC1122","PLC1123",
                  "PLC1129","PLC1130","PLC1133","PLC1136","PLC1137","PLC1138","PLC1146","PLC1148","PLC1150",
                  "PLC1151","PLC1152","PLC1153","PLC1156","PLC1157","PLC1159","PLC1160","PLC1161","PLC1162",
                  "PLC1165","PLC1168","PLC1169","PLC1172","PLC1173","PLC1178","PLC1179","PLC1180","PLC1182",
                  "PLC1184R","PLC1185R","PLC1200","PLC1207","PLC1216","UF1001","UF1010","UF1012","UF1013","UF1014",
                  "UF1014R","UF1015","UF1017","UF1019R","UF1021","UF1022","UF1024","UF1026","UF1027","UF1028","UF1030")

wg2_issue_id <- c("PLC1029","PLC1030","PLC1060","PLC1083","PLC1086","PLC1092","PLC1106","PLC1150","PLC1152",
                  "PLC1156","PLC1160","PLC1180","PLC1182","UF1010","UF1012")

calc_3.2$lab_1_wg5_plaque_reading_mst[which(calc_3.2$lab_id %in% wg1_issue_id)] <- 4
calc_3.2$lab_1_wg5_plaque_mst[which(calc_3.2$lab_id %in% wg1_issue_id)] <- NA

calc_3.2$lab_2_wg5_plaque_reading_mst[which(calc_3.2$lab_id %in% wg2_issue_id)] <- 4
calc_3.2$lab_2_wg5_plaque_mst[which(calc_3.2$lab_id %in% wg2_issue_id)] <- NA

calc_3.2$lab_1_wg5_plaque_reading_mst[which(calc_3.2$lab_id %in% c("PLC1040"))] <- 1
calc_3.2$lab_1_wg5_plaque_mst[which(calc_3.2$lab_id %in% c("PLC1040"))] <- NA
calc_3.2$lab_1_wg5_plaque_reading_mst[which(calc_3.2$lab_id %in% c("PLC1096","PLC1142"))] <- 2
calc_3.2$lab_1_wg5_plaque_mst[which(calc_3.2$lab_id %in% c("PLC1040","PLC1142"))] <- NA

#GB124
calc_3.2$lab_3_gb124_plaque_reading_mst[which(calc_3.2$lab_id %in% c("SW1008","UF1001"))] <- 4
calc_3.2$lab_3_gb124_plaque_mst[which(calc_3.2$lab_id %in% c("SW1008","UF1001"))] <- NA
calc_3.2$lab_1_gb124_plaque_reading_mst[which(calc_3.2$lab_id %in% c("SW1010","SL1013","PLC1091","PLC1093","PLC1097","PLC1122","PLC1123","PLC1128","PLC1130","PLC1195","PLC1200","PLC1027"))] <- 4
calc_3.2$lab_1_gb124_plaque_mst[which(calc_3.2$lab_id %in% c("SW1010","SL1013","PLC1091","PLC1093","PLC1097","PLC1122","PLC1123","PLC1128","PLC1130","PLC1195","PLC1200","PLC1027"))] <- NA
calc_3.2$lab_2_gb124_plaque_reading_mst[which(calc_3.2$lab_id %in% c("PLC1093","PLC1097","PLC1200"))] <- 4
calc_3.2$lab_2_gb124_plaque_mst[which(calc_3.2$lab_id %in% c("PLC1093","PLC1097","PLC1200"))] <- NA

#PLC1163 with different dilutions compared with what Renuka has.

#WG5 with enrichment
calc_3.2$lab_1_wg5_enr_plaque_reading_mst[which(calc_3.2$lab_id %in% c("FW1001","OD1003","OD1004","OD1005","SF1005","SF1006"))] <- 4
calc_3.2$lab_1_wg5_enr_plaque_mst[which(calc_3.2$lab_id %in% c("FW1001","OD1003","OD1004","OD1005","SF1005","SF1006"))] <- NA

#GB124 with enrichment
calc_3.2$lab_3_gb124_enr_plaque_reading_mst[which(calc_3.2$lab_id %in% c("FW1001","FW1003","OD1004","OD1005","SW1005","SW1006","SW1008","SL1009","SL1010"))] <- 4
calc_3.2$lab_3_gb124_enr_plaque_mst[which(calc_3.2$lab_id %in% c("FW1001","FW1003","OD1004","OD1005","SW1005","SW1006","SW1008","SL1009","SL1010"))] <- NA

calc_3.2$lab_1_gb124_enr_plaque_reading_mst[which(calc_3.2$lab_id %in% c("SF1001","FW1001","OD1004","OD1005","SW1004","SW1005","SW1006","SL1009","SL1010"))] <- 4
calc_3.2$lab_1_gb124_enr_plaque_mst[which(calc_3.2$lab_id %in% c("SF1001","FW1001","OD1004","OD1005","SW1004","SW1005","SW1006","SL1009","SL1010"))] <- NA

calc_3.2$lab_2_gb124_enr_plaque_reading_mst[which(calc_3.2$lab_id %in% c("SW1004"))] <- 4
calc_3.2$lab_2_gb124_enr_plaque_mst[which(calc_3.2$lab_id %in% c("SW1004"))] <- NA

calc_3.2$lab_1_gb124_enr_plaque_reading_mst[which(calc_3.2$lab_id %in% c("SW1008"))] <- 1
calc_3.2$lab_1_gb124_enr_plaque_mst[which(calc_3.2$lab_id %in% c("SW1008"))] <- NA


#WG5
############################################################################################################
calc_3.2$calc_1_wg5 <- calc_3.2$lab_1_wg5_plaque_mst
calc_3.2$calc_1_wg5[which(calc_3.2$lab_1_wg5_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_1_wg5[which(calc_3.2$lab_1_wg5_plaque_reading_mst==2)] <- sub.TDTC
calc_3.2$calc_1_wg5[which(calc_3.2$lab_1_wg5_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_1_wg5[which(calc_3.2$calc_1_wg5==0)] <- sub.LLOD

calc_3.2$calc_2_wg5 <- calc_3.2$lab_2_wg5_plaque_mst
calc_3.2$calc_2_wg5[which(calc_3.2$lab_2_wg5_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_2_wg5[which(calc_3.2$lab_2_wg5_plaque_reading_mst==2)] <- sub.TDTC
calc_3.2$calc_2_wg5[which(calc_3.2$lab_2_wg5_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_2_wg5[which(calc_3.2$calc_2_wg5==0)] <- sub.LLOD

calc_3.2$calc_3_wg5 <- calc_3.2$lab_3_wg5_plaque_mst
calc_3.2$calc_3_wg5[which(calc_3.2$lab_3_wg5_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_3_wg5[which(calc_3.2$lab_3_wg5_plaque_reading_mst==2)] <- sub.TDTC
calc_3.2$calc_3_wg5[which(calc_3.2$lab_3_wg5_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_3_wg5[which(calc_3.2$calc_3_wg5==0)] <- sub.LLOD

calc_3.2$calc_1_wg5_dil<-(10^calc_3.2$lab_1_wg5_dil_tested)/(10^7)*calc_3.2$lab_1_wg5_volume
calc_3.2$calc_2_wg5_dil<-(10^calc_3.2$lab_2_wg5_dil_tested)/(10^7)*calc_3.2$lab_2_wg5_volume
calc_3.2$calc_3_wg5_dil<-(10^calc_3.2$lab_3_wg5_dil_tested)/(10^7)*calc_3.2$lab_3_wg5_volume

calc_3.2$calc_1_wg5_conc <- calc_3.2$calc_1_wg5/calc_3.2$calc_1_wg5_dil
calc_3.2$calc_2_wg5_conc <- calc_3.2$calc_2_wg5/calc_3.2$calc_2_wg5_dil
calc_3.2$calc_3_wg5_conc <- calc_3.2$calc_3_wg5/calc_3.2$calc_3_wg5_dil

calc_3.2$calc_wg5_conc <- NA
#one dilution;
only_dil_1 <- is.na(calc_3.2$lab_2_wg5_volume) & is.na(calc_3.2$lab_3_wg5_volume)
calc_3.2$calc_wg5_conc[only_dil_1] <- calc_3.2$calc_1_wg5_conc[only_dil_1]

#two dilutions;
no_dil_3 <- is.na(calc_3.2$lab_3_wg5_volume)
cond_1 <- which(no_dil_3 & calc_3.2$calc_1_wg5==0.5 & calc_3.2$calc_2_wg5==0.5)
cond_2 <- which(no_dil_3 & calc_3.2$calc_1_wg5>0.5 & calc_3.2$calc_1_wg5<300 & calc_3.2$calc_2_wg5==0.5)
cond_3 <- which(no_dil_3 & calc_3.2$calc_1_wg5>0.5 & calc_3.2$calc_1_wg5<300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300)
cond_4 <- which(no_dil_3 & calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300)
cond_5 <- which(no_dil_3 & calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>=300)

#three dilutions;
cond_6 <- which(calc_3.2$calc_1_wg5==0.5 & calc_3.2$calc_2_wg5==0.5 & calc_3.2$calc_3_wg5==0.5)
cond_7 <- which(calc_3.2$calc_1_wg5>0.5 & calc_3.2$calc_1_wg5<300 & calc_3.2$calc_2_wg5==0.5 & calc_3.2$calc_3_wg5==0.5)
cond_8 <- which(calc_3.2$calc_1_wg5>0.5 & calc_3.2$calc_1_wg5<300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300 & calc_3.2$calc_3_wg5==0.5)
cond_9 <- which(calc_3.2$calc_1_wg5>0.5 & calc_3.2$calc_1_wg5<300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300 & calc_3.2$calc_3_wg5>0.5 & calc_3.2$calc_3_wg5<300)
cond_10 <- which(calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300 & calc_3.2$calc_3_wg5>0.5 & calc_3.2$calc_3_wg5<300)
cond_11 <- which(calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>=300 & calc_3.2$calc_3_wg5>0.5 & calc_3.2$calc_3_wg5<300)
cond_12 <- which(calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>=300 & calc_3.2$calc_3_wg5>=300)
cond_13 <- which(calc_3.2$calc_1_wg5>=300 & calc_3.2$calc_2_wg5>0.5 & calc_3.2$calc_2_wg5<300 & calc_3.2$calc_3_wg5==0.5)

calc_3.2$calc_wg5_conc[cond_1] <- pmin(calc_3.2$calc_1_wg5_conc[cond_1],calc_3.2$calc_2_wg5_conc[cond_1])
calc_3.2$calc_wg5_conc[cond_2] <- calc_3.2$calc_1_wg5_conc[cond_2]
calc_3.2$calc_wg5_conc[cond_3] <- (calc_3.2$calc_1_wg5_conc[cond_3] + calc_3.2$calc_2_wg5_conc[cond_3])/2
calc_3.2$calc_wg5_conc[cond_4] <- calc_3.2$calc_2_wg5_conc[cond_4]
calc_3.2$calc_wg5_conc[cond_5] <- pmax(calc_3.2$calc_1_wg5_conc[cond_5],calc_3.2$calc_2_wg5_conc[cond_5])

calc_3.2$calc_wg5_conc[cond_6] <- pmin(calc_3.2$calc_1_wg5_conc[cond_6],calc_3.2$calc_2_wg5_conc[cond_6],calc_3.2$calc_3_wg5_conc[cond_6])
calc_3.2$calc_wg5_conc[cond_7] <- calc_3.2$calc_1_wg5_conc[cond_7]
calc_3.2$calc_wg5_conc[cond_8] <- (calc_3.2$calc_1_wg5_conc[cond_8] + calc_3.2$calc_2_wg5_conc[cond_8])/2
calc_3.2$calc_wg5_conc[cond_9] <- (calc_3.2$calc_1_wg5_conc[cond_9] + calc_3.2$calc_2_wg5_conc[cond_9] + calc_3.2$calc_3_wg5_conc[cond_9])/3
calc_3.2$calc_wg5_conc[cond_10] <- (calc_3.2$calc_2_wg5_conc[cond_10] + calc_3.2$calc_3_wg5_conc[cond_10])/2
calc_3.2$calc_wg5_conc[cond_11] <- calc_3.2$calc_3_wg5_conc[cond_11]
calc_3.2$calc_wg5_conc[cond_12] <- pmax(calc_3.2$calc_1_wg5_conc[cond_12],calc_3.2$calc_2_wg5_conc[cond_12],calc_3.2$calc_3_wg5_conc[cond_12])
calc_3.2$calc_wg5_conc[cond_13] <- calc_3.2$calc_2_wg5_conc[cond_13]

#dilution of SW1002, PLC1027, PLC1028 are different NEED TO CONFIRM WITH RENUKA;

# View(calc_3.2[which(is.na(calc_3.2$calc_wg5_conc) & !is.na(calc_3.2$calc_1_wg5)),c(11,89:98)])
#"SL1013"  "PLC1032" "PLC1033" "PLC1062" "PLC1098" "PLC8122" "PLC8123" "PLC1166" "UF1020" are still unable to calculate the concentration of WG5;
prob_3.2_wg5 <- calc_3.2[which(is.na(calc_3.2$calc_wg5_conc) & !is.na(calc_3.2$calc_1_wg5_conc)),]
#View(prob_3.2_wg5)
############################################################################################################

#GB124
############################################################################################################
calc_3.2$calc_1_gb124 <- calc_3.2$lab_1_gb124_plaque_mst
calc_3.2$calc_1_gb124[which(calc_3.2$lab_1_gb124_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_1_gb124[which(calc_3.2$lab_1_gb124_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_1_gb124[which(calc_3.2$calc_1_gb124==0)] <- sub.LLOD

calc_3.2$calc_2_gb124 <- calc_3.2$lab_2_gb124_plaque_mst
calc_3.2$calc_2_gb124[which(calc_3.2$lab_2_gb124_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_2_gb124[which(calc_3.2$lab_2_gb124_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_2_gb124[which(calc_3.2$calc_2_gb124==0)] <- sub.LLOD

calc_3.2$calc_3_gb124 <- calc_3.2$lab_3_gb124_plaque_mst
calc_3.2$calc_3_gb124[which(calc_3.2$lab_3_gb124_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_3_gb124[which(calc_3.2$lab_3_gb124_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_3_gb124[which(calc_3.2$calc_3_gb124==0)] <- sub.LLOD

calc_3.2$calc_1_gb124_dil<-(10^calc_3.2$lab_1_gb124_dil_tested)/(10^7)*calc_3.2$lab_1_gb124_volume
calc_3.2$calc_2_gb124_dil<-(10^calc_3.2$lab_2_gb124_dil_tested)/(10^7)*calc_3.2$lab_2_gb124_volume
calc_3.2$calc_3_gb124_dil<-(10^calc_3.2$lab_3_gb124_dil_tested)/(10^7)*calc_3.2$lab_3_gb124_volume

calc_3.2$calc_1_gb124_conc <- calc_3.2$calc_1_gb124/calc_3.2$calc_1_gb124_dil
calc_3.2$calc_2_gb124_conc <- calc_3.2$calc_2_gb124/calc_3.2$calc_2_gb124_dil
calc_3.2$calc_3_gb124_conc <- calc_3.2$calc_3_gb124/calc_3.2$calc_3_gb124_dil

calc_3.2$calc_gb124_conc <- NA
#one dilution;
only_dil_1 <- is.na(calc_3.2$lab_2_gb124_volume) & is.na(calc_3.2$lab_3_gb124_volume)
calc_3.2$calc_gb124_conc[only_dil_1] <- calc_3.2$calc_1_gb124_conc[only_dil_1]

#two dilutions;
no_dil_3 <- is.na(calc_3.2$lab_3_gb124_volume)
cond_1 <- which(no_dil_3 & calc_3.2$calc_1_gb124==0.5 & calc_3.2$calc_2_gb124==0.5)
cond_2 <- which(no_dil_3 & calc_3.2$calc_1_gb124>0.5 & calc_3.2$calc_1_gb124<300 & calc_3.2$calc_2_gb124==0.5)
cond_3 <- which(no_dil_3 & calc_3.2$calc_1_gb124>0.5 & calc_3.2$calc_1_gb124<300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300)
cond_4 <- which(no_dil_3 & calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300)
cond_5 <- which(no_dil_3 & calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>=300)

#three dilutions;
cond_6 <- which(calc_3.2$calc_1_gb124==0.5 & calc_3.2$calc_2_gb124==0.5 & calc_3.2$calc_3_gb124==0.5)
cond_7 <- which(calc_3.2$calc_1_gb124>0.5 & calc_3.2$calc_1_gb124<300 & calc_3.2$calc_2_gb124==0.5 & calc_3.2$calc_3_gb124==0.5)
cond_8 <- which(calc_3.2$calc_1_gb124>0.5 & calc_3.2$calc_1_gb124<300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300 & calc_3.2$calc_3_gb124==0.5)
cond_9 <- which(calc_3.2$calc_1_gb124>0.5 & calc_3.2$calc_1_gb124<300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300 & calc_3.2$calc_3_gb124>0.5 & calc_3.2$calc_3_gb124<300)
cond_10 <- which(calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300 & calc_3.2$calc_3_gb124>0.5 & calc_3.2$calc_3_gb124<300)
cond_11 <- which(calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>=300 & calc_3.2$calc_3_gb124>0.5 & calc_3.2$calc_3_gb124<300)
cond_12 <- which(calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>=300 & calc_3.2$calc_3_gb124>=300)
cond_13 <- which(calc_3.2$calc_1_gb124>=300 & calc_3.2$calc_2_gb124>0.5 & calc_3.2$calc_2_gb124<300 & calc_3.2$calc_3_gb124==0.5)

calc_3.2$calc_gb124_conc[cond_1] <- pmin(calc_3.2$calc_1_gb124_conc[cond_1],calc_3.2$calc_2_gb124_conc[cond_1])
calc_3.2$calc_gb124_conc[cond_2] <- calc_3.2$calc_1_gb124_conc[cond_2]
calc_3.2$calc_gb124_conc[cond_3] <- (calc_3.2$calc_1_gb124_conc[cond_3] + calc_3.2$calc_2_gb124_conc[cond_3])/2
calc_3.2$calc_gb124_conc[cond_4] <- calc_3.2$calc_2_gb124_conc[cond_4]
calc_3.2$calc_gb124_conc[cond_5] <- pmax(calc_3.2$calc_1_gb124_conc[cond_5],calc_3.2$calc_2_gb124_conc[cond_5])

calc_3.2$calc_gb124_conc[cond_6] <- pmin(calc_3.2$calc_1_gb124_conc[cond_6],calc_3.2$calc_2_gb124_conc[cond_6],calc_3.2$calc_3_gb124_conc[cond_6])
calc_3.2$calc_gb124_conc[cond_7] <- calc_3.2$calc_1_gb124_conc[cond_7]
calc_3.2$calc_gb124_conc[cond_8] <- (calc_3.2$calc_1_gb124_conc[cond_8] + calc_3.2$calc_2_gb124_conc[cond_8])/2
calc_3.2$calc_gb124_conc[cond_9] <- (calc_3.2$calc_1_gb124_conc[cond_9] + calc_3.2$calc_2_gb124_conc[cond_9] + calc_3.2$calc_3_gb124_conc[cond_9])/3
calc_3.2$calc_gb124_conc[cond_10] <- (calc_3.2$calc_2_gb124_conc[cond_10] + calc_3.2$calc_3_gb124_conc[cond_10])/2
calc_3.2$calc_gb124_conc[cond_11] <- calc_3.2$calc_3_gb124_conc[cond_11]
calc_3.2$calc_gb124_conc[cond_12] <- pmax(calc_3.2$calc_1_gb124_conc[cond_12],calc_3.2$calc_2_gb124_conc[cond_12],calc_3.2$calc_3_gb124_conc[cond_12])
calc_3.2$calc_gb124_conc[cond_13] <- calc_3.2$calc_2_gb124_conc[cond_13]

calc_3.2$calc_gb124_conc[which(calc_3.2$lab_id %in% c("OD1004","UF1001"))] <- calc_3.2$calc_2_gb124_conc[which(calc_3.2$lab_id %in% c("OD1004","UF1001"))]
calc_3.2$calc_gb124_conc[which(calc_3.2$lab_id %in% c("OD1005","SW1008"))] <- (calc_3.2$calc_1_gb124_conc[which(calc_3.2$lab_id %in% c("OD1005","SW1008"))] + calc_3.2$calc_2_gb124_conc[which(calc_3.2$lab_id %in% c("OD1005","SW1008"))])/2

# View(calc_3.2[which(is.na(calc_3.2$calc_gb124_conc) & !is.na(calc_3.2$calc_1_gb124)),c(11,99:108)])

#"SL1004"  "FPA1002" "FPD1002" "FPE1002" "PLC1041" "PLC1073" "PLC8122" "PLC8123" "UF1011" "PLC1163" "PLC1195" "PLC1200" "FPE2004" are still unable to calculate the concentration of GB124;
prob_3.2_gb124 <- calc_3.2[which(is.na(calc_3.2$calc_gb124_conc) & !is.na(calc_3.2$calc_1_gb124_conc)),]
#View(prob_3.2_gb124)
############################################################################################################

#WG5 enrichment
############################################################################################################
calc_3.2$calc_1_wg5_enr <- calc_3.2$lab_1_wg5_enr_plaque_mst
calc_3.2$calc_1_wg5_enr[which(calc_3.2$lab_1_wg5_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_1_wg5_enr[which(calc_3.2$lab_1_wg5_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_1_wg5_enr[which(calc_3.2$calc_1_wg5_enr==0)] <- sub.LLOD

calc_3.2$calc_2_wg5_enr <- calc_3.2$lab_2_wg5_enr_plaque_mst
calc_3.2$calc_2_wg5_enr[which(calc_3.2$lab_2_wg5_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_2_wg5_enr[which(calc_3.2$lab_2_wg5_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_2_wg5_enr[which(calc_3.2$calc_2_wg5_enr==0)] <- sub.LLOD

calc_3.2$calc_3_wg5_enr <- calc_3.2$lab_3_wg5_enr_plaque_mst
calc_3.2$calc_3_wg5_enr[which(calc_3.2$lab_3_wg5_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_3_wg5_enr[which(calc_3.2$lab_3_wg5_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_3_wg5_enr[which(calc_3.2$calc_3_wg5_enr==0)] <- sub.LLOD

calc_3.2$calc_1_wg5_enr_dil<-(10^calc_3.2$lab_1_wg5_enr_dil_tested)/(10^7)*calc_3.2$lab_1_wg5_enr_volume
calc_3.2$calc_2_wg5_enr_dil<-(10^calc_3.2$lab_2_wg5_enr_dil_tested)/(10^7)*calc_3.2$lab_2_wg5_enr_volume
calc_3.2$calc_3_wg5_enr_dil<-(10^calc_3.2$lab_3_wg5_enr_dil_tested)/(10^7)*calc_3.2$lab_3_wg5_enr_volume

calc_3.2$calc_1_wg5_enr_conc <- calc_3.2$calc_1_wg5_enr/calc_3.2$calc_1_wg5_enr_dil
calc_3.2$calc_2_wg5_enr_conc <- calc_3.2$calc_2_wg5_enr/calc_3.2$calc_2_wg5_enr_dil
calc_3.2$calc_3_wg5_enr_conc <- calc_3.2$calc_3_wg5_enr/calc_3.2$calc_3_wg5_enr_dil

calc_3.2$calc_wg5_enr_conc <- NA
#one dilution;
only_dil_1 <- is.na(calc_3.2$lab_2_wg5_enr_volume) & is.na(calc_3.2$lab_3_wg5_enr_volume)
calc_3.2$calc_wg5_enr_conc[only_dil_1] <- calc_3.2$calc_1_wg5_enr_conc[only_dil_1]

#two dilutions;
no_dil_3 <- is.na(calc_3.2$lab_3_wg5_enr_volume)
cond_1 <- which(no_dil_3 & calc_3.2$calc_1_wg5_enr==0.5 & calc_3.2$calc_2_wg5_enr==0.5)
cond_2 <- which(no_dil_3 & calc_3.2$calc_1_wg5_enr>0.5 & calc_3.2$calc_1_wg5_enr<300 & calc_3.2$calc_2_wg5_enr==0.5)
cond_3 <- which(no_dil_3 & calc_3.2$calc_1_wg5_enr>0.5 & calc_3.2$calc_1_wg5_enr<300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300)
cond_4 <- which(no_dil_3 & calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300)
cond_5 <- which(no_dil_3 & calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>=300)

#three dilutions;
cond_6 <- which(calc_3.2$calc_1_wg5_enr==0.5 & calc_3.2$calc_2_wg5_enr==0.5 & calc_3.2$calc_3_wg5_enr==0.5)
cond_7 <- which(calc_3.2$calc_1_wg5_enr>0.5 & calc_3.2$calc_1_wg5_enr<300 & calc_3.2$calc_2_wg5_enr==0.5 & calc_3.2$calc_3_wg5_enr==0.5)
cond_8 <- which(calc_3.2$calc_1_wg5_enr>0.5 & calc_3.2$calc_1_wg5_enr<300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300 & calc_3.2$calc_3_wg5_enr==0.5)
cond_9 <- which(calc_3.2$calc_1_wg5_enr>0.5 & calc_3.2$calc_1_wg5_enr<300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300 & calc_3.2$calc_3_wg5_enr>0.5 & calc_3.2$calc_3_wg5_enr<300)
cond_10 <- which(calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300 & calc_3.2$calc_3_wg5_enr>0.5 & calc_3.2$calc_3_wg5_enr<300)
cond_11 <- which(calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>=300 & calc_3.2$calc_3_wg5_enr>0.5 & calc_3.2$calc_3_wg5_enr<300)
cond_12 <- which(calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>=300 & calc_3.2$calc_3_wg5_enr>=300)
cond_13 <- which(calc_3.2$calc_1_wg5_enr>=300 & calc_3.2$calc_2_wg5_enr>0.5 & calc_3.2$calc_2_wg5_enr<300 & calc_3.2$calc_3_wg5_enr==0.5)

calc_3.2$calc_wg5_enr_conc[cond_1] <- pmin(calc_3.2$calc_1_wg5_enr_conc[cond_1],calc_3.2$calc_2_wg5_enr_conc[cond_1])
calc_3.2$calc_wg5_enr_conc[cond_2] <- calc_3.2$calc_1_wg5_enr_conc[cond_2]
calc_3.2$calc_wg5_enr_conc[cond_3] <- (calc_3.2$calc_1_wg5_enr_conc[cond_3] + calc_3.2$calc_2_wg5_enr_conc[cond_3])/2
calc_3.2$calc_wg5_enr_conc[cond_4] <- calc_3.2$calc_2_wg5_enr_conc[cond_4]
calc_3.2$calc_wg5_enr_conc[cond_5] <- pmax(calc_3.2$calc_1_wg5_enr_conc[cond_5],calc_3.2$calc_2_wg5_enr_conc[cond_5])

calc_3.2$calc_wg5_enr_conc[cond_6] <- pmin(calc_3.2$calc_1_wg5_enr_conc[cond_6],calc_3.2$calc_2_wg5_enr_conc[cond_6],calc_3.2$calc_3_wg5_enr_conc[cond_6])
calc_3.2$calc_wg5_enr_conc[cond_7] <- calc_3.2$calc_1_wg5_enr_conc[cond_7]
calc_3.2$calc_wg5_enr_conc[cond_8] <- (calc_3.2$calc_1_wg5_enr_conc[cond_8] + calc_3.2$calc_2_wg5_enr_conc[cond_8])/2
calc_3.2$calc_wg5_enr_conc[cond_9] <- (calc_3.2$calc_1_wg5_enr_conc[cond_9] + calc_3.2$calc_2_wg5_enr_conc[cond_9] + calc_3.2$calc_3_wg5_enr_conc[cond_9])/3
calc_3.2$calc_wg5_enr_conc[cond_10] <- (calc_3.2$calc_2_wg5_enr_conc[cond_10] + calc_3.2$calc_3_wg5_enr_conc[cond_10])/2
calc_3.2$calc_wg5_enr_conc[cond_11] <- calc_3.2$calc_3_wg5_enr_conc[cond_11]
calc_3.2$calc_wg5_enr_conc[cond_12] <- pmax(calc_3.2$calc_1_wg5_enr_conc[cond_12],calc_3.2$calc_2_wg5_enr_conc[cond_12],calc_3.2$calc_3_wg5_enr_conc[cond_12])
calc_3.2$calc_wg5_enr_conc[cond_13] <- calc_3.2$calc_2_wg5_enr_conc[cond_13]

# View(calc_3.2[which(is.na(calc_3.2$calc_wg5_enr_conc) & !is.na(calc_3.2$calc_1_wg5_enr)),c(11,109:118)])
#UF1001 need to check the dilution 1 whether it is complete lysis;
prob_3.2_wg5_enr <- calc_3.2[which(is.na(calc_3.2$calc_wg5_enr_conc) & !is.na(calc_3.2$calc_1_wg5_enr_conc)),]
#View(prob_3.2_wg5_enr)
############################################################################################################

#GB124 enrichment
############################################################################################################
calc_3.2$calc_1_gb124_enr <- calc_3.2$lab_1_gb124_enr_plaque_mst
calc_3.2$calc_1_gb124_enr[which(calc_3.2$lab_1_gb124_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_1_gb124_enr[which(calc_3.2$lab_1_gb124_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_1_gb124_enr[which(calc_3.2$calc_1_gb124_enr==0)] <- sub.LLOD

calc_3.2$calc_2_gb124_enr <- calc_3.2$lab_2_gb124_enr_plaque_mst
calc_3.2$calc_2_gb124_enr[which(calc_3.2$lab_2_gb124_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_2_gb124_enr[which(calc_3.2$lab_2_gb124_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_2_gb124_enr[which(calc_3.2$calc_2_gb124_enr==0)] <- sub.LLOD

calc_3.2$calc_3_gb124_enr <- calc_3.2$lab_3_gb124_enr_plaque_mst
calc_3.2$calc_3_gb124_enr[which(calc_3.2$lab_3_gb124_enr_plaque_reading_mst==1)] <- sub.TNTC
calc_3.2$calc_3_gb124_enr[which(calc_3.2$lab_3_gb124_enr_plaque_reading_mst==4)] <- sub.complete.lysis
calc_3.2$calc_3_gb124_enr[which(calc_3.2$calc_3_gb124_enr==0)] <- sub.LLOD

calc_3.2$calc_1_gb124_enr_dil<-(10^calc_3.2$lab_1_gb124_enr_dil_tested)/(10^7)*calc_3.2$lab_1_gb124_enr_volume
calc_3.2$calc_2_gb124_enr_dil<-(10^calc_3.2$lab_2_gb124_enr_dil_tested)/(10^7)*calc_3.2$lab_2_gb124_enr_volume
calc_3.2$calc_3_gb124_enr_dil<-(10^calc_3.2$lab_3_gb124_enr_dil_tested)/(10^7)*calc_3.2$lab_3_gb124_enr_volume

calc_3.2$calc_1_gb124_enr_conc <- calc_3.2$calc_1_gb124_enr/calc_3.2$calc_1_gb124_enr_dil
calc_3.2$calc_2_gb124_enr_conc <- calc_3.2$calc_2_gb124_enr/calc_3.2$calc_2_gb124_enr_dil
calc_3.2$calc_3_gb124_enr_conc <- calc_3.2$calc_3_gb124_enr/calc_3.2$calc_3_gb124_enr_dil

calc_3.2$calc_gb124_enr_conc <- NA
#one dilution;
only_dil_1 <- is.na(calc_3.2$lab_2_gb124_enr_volume) & is.na(calc_3.2$lab_3_gb124_enr_volume)
calc_3.2$calc_gb124_enr_conc[only_dil_1] <- calc_3.2$calc_1_gb124_enr_conc[only_dil_1]

#two dilutions;
no_dil_3 <- is.na(calc_3.2$lab_3_gb124_enr_volume)
cond_1 <- which(no_dil_3 & calc_3.2$calc_1_gb124_enr==0.5 & calc_3.2$calc_2_gb124_enr==0.5)
cond_2 <- which(no_dil_3 & calc_3.2$calc_1_gb124_enr>0.5 & calc_3.2$calc_1_gb124_enr<300 & calc_3.2$calc_2_gb124_enr==0.5)
cond_3 <- which(no_dil_3 & calc_3.2$calc_1_gb124_enr>0.5 & calc_3.2$calc_1_gb124_enr<300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300)
cond_4 <- which(no_dil_3 & calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300)
cond_5 <- which(no_dil_3 & calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>=300)

#three dilutions;
cond_6 <- which(calc_3.2$calc_1_gb124_enr==0.5 & calc_3.2$calc_2_gb124_enr==0.5 & calc_3.2$calc_3_gb124_enr==0.5)
cond_7 <- which(calc_3.2$calc_1_gb124_enr>0.5 & calc_3.2$calc_1_gb124_enr<300 & calc_3.2$calc_2_gb124_enr==0.5 & calc_3.2$calc_3_gb124_enr==0.5)
cond_8 <- which(calc_3.2$calc_1_gb124_enr>0.5 & calc_3.2$calc_1_gb124_enr<300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300 & calc_3.2$calc_3_gb124_enr==0.5)
cond_9 <- which(calc_3.2$calc_1_gb124_enr>0.5 & calc_3.2$calc_1_gb124_enr<300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300 & calc_3.2$calc_3_gb124_enr>0.5 & calc_3.2$calc_3_gb124_enr<300)
cond_10 <- which(calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300 & calc_3.2$calc_3_gb124_enr>0.5 & calc_3.2$calc_3_gb124_enr<300)
cond_11 <- which(calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>=300 & calc_3.2$calc_3_gb124_enr>0.5 & calc_3.2$calc_3_gb124_enr<300)
cond_12 <- which(calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>=300 & calc_3.2$calc_3_gb124_enr>=300)
cond_13 <- which(calc_3.2$calc_1_gb124_enr>=300 & calc_3.2$calc_2_gb124_enr>0.5 & calc_3.2$calc_2_gb124_enr<300 & calc_3.2$calc_3_gb124_enr==0.5)

calc_3.2$calc_gb124_enr_conc[cond_1] <- pmin(calc_3.2$calc_1_gb124_enr_conc[cond_1],calc_3.2$calc_2_gb124_enr_conc[cond_1])
calc_3.2$calc_gb124_enr_conc[cond_2] <- calc_3.2$calc_1_gb124_enr_conc[cond_2]
calc_3.2$calc_gb124_enr_conc[cond_3] <- (calc_3.2$calc_1_gb124_enr_conc[cond_3] + calc_3.2$calc_2_gb124_enr_conc[cond_3])/2
calc_3.2$calc_gb124_enr_conc[cond_4] <- calc_3.2$calc_2_gb124_enr_conc[cond_4]
calc_3.2$calc_gb124_enr_conc[cond_5] <- pmax(calc_3.2$calc_1_gb124_enr_conc[cond_5],calc_3.2$calc_2_gb124_enr_conc[cond_5])

calc_3.2$calc_gb124_enr_conc[cond_6] <- pmin(calc_3.2$calc_1_gb124_enr_conc[cond_6],calc_3.2$calc_2_gb124_enr_conc[cond_6],calc_3.2$calc_3_gb124_enr_conc[cond_6])
calc_3.2$calc_gb124_enr_conc[cond_7] <- calc_3.2$calc_1_gb124_enr_conc[cond_7]
calc_3.2$calc_gb124_enr_conc[cond_8] <- (calc_3.2$calc_1_gb124_enr_conc[cond_8] + calc_3.2$calc_2_gb124_enr_conc[cond_8])/2
calc_3.2$calc_gb124_enr_conc[cond_9] <- (calc_3.2$calc_1_gb124_enr_conc[cond_9] + calc_3.2$calc_2_gb124_enr_conc[cond_9] + calc_3.2$calc_3_gb124_enr_conc[cond_9])/3
calc_3.2$calc_gb124_enr_conc[cond_10] <- (calc_3.2$calc_2_gb124_enr_conc[cond_10] + calc_3.2$calc_3_gb124_enr_conc[cond_10])/2
calc_3.2$calc_gb124_enr_conc[cond_11] <- calc_3.2$calc_3_gb124_enr_conc[cond_11]
calc_3.2$calc_gb124_enr_conc[cond_12] <- pmax(calc_3.2$calc_1_gb124_enr_conc[cond_12],calc_3.2$calc_2_gb124_enr_conc[cond_12],calc_3.2$calc_3_gb124_enr_conc[cond_12])
calc_3.2$calc_gb124_enr_conc[cond_13] <- calc_3.2$calc_2_gb124_enr_conc[cond_13]

calc_3.2$calc_gb124_enr_conc[which(calc_3.2$lab_id %in% c("FW1003","UF1001","SL1010"))] <- calc_3.2$calc_2_gb124_enr_conc[which(calc_3.2$lab_id %in% c("FW1003","UF1001","SL1010"))]
calc_3.2$calc_gb124_enr_conc[which(calc_3.2$lab_id %in% c("DWA1003OLD","OD1006"))] <- (calc_3.2$calc_1_gb124_enr_conc[which(calc_3.2$lab_id %in% c("DWA1003OLD","OD1006"))] + calc_3.2$calc_2_gb124_enr_conc[which(calc_3.2$lab_id %in% c("DWA1003OLD","OD1006"))])/2

# View(calc_3.2[which(is.na(calc_3.2$calc_gb124_enr_conc) & !is.na(calc_3.2$calc_1_gb124_enr)),c(11,119:128)])

prob_3.2_gb124_enr <- calc_3.2[which(is.na(calc_3.2$calc_gb124_enr_conc) & !is.na(calc_3.2$calc_1_gb124_enr_conc)),]
#View(prob_3.2_gb124_enr)

# write.csv(prob_3.2_wg5,file="./problem_3.2_wg5.csv")
# write.csv(prob_3.2_wg5_enr,file="./problem_3.2_wg5_enr.csv")
# write.csv(prob_3.2_gb124,file="./problem_3.2_gb124.csv")
# write.csv(prob_3.2_gb124_enr,file="./problem_3.2_gb124_enr.csv")


mst_conc <- calc_3.2

