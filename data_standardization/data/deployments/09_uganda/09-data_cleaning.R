


# read uganda form
df.col.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/original files/", 
                            "sample_edit_deleted", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read uganda  form
df.lab.9 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/original files/", 
                            "lab_edit_deleted", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))


#find matching col_id and lab_id
# col 397 obs
# lab 336 obs

#in col but not in lab
del.list1 <- setdiff(df.col.9$col_id, df.lab.9$lab_id)
df.col.9 <- df.col.9 %>% filter(!col_id %in% del.list1)

#in lab but not in col
del.list2 <- setdiff(df.lab.9$lab_id, df.col.9$col_id)
df.lab.9 <- df.lab.9 %>% filter(!lab_id %in% del.list2)

table(df.col.9$col_neighborhood)

df.col.9$col_neighborhood[df.col.9$col_neighborhood == 309] <- 1
df.col.9$col_neighborhood[df.col.9$col_neighborhood == 310] <- 2
df.col.9$col_neighborhood[df.col.9$col_neighborhood == 311] <- 3
df.col.9$col_neighborhood[df.col.9$col_neighborhood == 312] <- 4
df.col.9$col_neighborhood[df.col.9$col_neighborhood == 313] <- 5


table(df.lab.9$lab_sample_type)
table(df.col.9$col_sample_type)
table(df.col.9$col_neighborhood, df.col.9$col_sample_type)


# streetfood comparison/ adjustment

# deployment 9. kampala
# add mean values for missing SF weight

df.col.9 %>% filter(col_sample_type == 10) -> df.col.91
df.lab.9 %>% filter(lab_sample_type == 10) -> df.lab.91

#which ids have 0 grams for sf weight?
grams0 <- df.lab.9$lab_id[df.lab.9$lab_sf_weight == 0]

df.col.91 %>% filter(col_id %in% grams0)

# id and type of sf
#1211, 1 rolex
#1212, 88 Chapati with beans
#1213, 1 rolex
#1214, 88 Chapati with beans
#1215, 3 samosas

# 1
sf1 <- df.col.9$col_id[df.col.9$col_sf_type == 1 & df.col.9$col_sample_type == 10]
sf2 <- sf1[!sf1 %in% c(1211,1213)] 
df.lab.9 %>% filter(lab_id %in% sf2) %>% summarise(min = min(lab_sf_weight),
                                                   max = max(lab_sf_weight),
                                                   mean = mean(lab_sf_weight))

df.lab.9$lab_sf_weight[df.lab.9$lab_id == 1211] <- 182
df.lab.9$lab_sf_weight[df.lab.9$lab_id == 1213] <- 182

# 3
sf1 <- df.col.9$col_id[df.col.9$col_sf_type == 3 & df.col.9$col_sample_type == 10]
sf2 <- sf1[!sf1 %in% 1215] 
df.lab.9 %>% filter(lab_id %in% sf2) %>% summarise(min = min(lab_sf_weight),
                                                   max = max(lab_sf_weight),
                                                   mean = mean(lab_sf_weight))

df.lab.9$lab_sf_weight[df.lab.9$lab_id == 1215] <- 110

# 88
df.col.9$col_sf_type_other[df.col.9$col_sf_type == 88 & df.col.9$col_sample_type == 10 
                           & df.col.9$col_sf_type_other == "Chapati with beans"]

sf1 <- df.col.9$col_id[df.col.9$col_sf_type == 88 & df.col.9$col_sample_type == 10
                       & df.col.9$col_sf_type_other == "Chapati with beans"]
sf2 <- sf1[!sf1 %in% c(1212,1214)] 
df.lab.9 %>% filter(lab_id %in% sf2) %>% summarise(min = min(lab_sf_weight),
                                                   max = max(lab_sf_weight),
                                                   mean = mean(lab_sf_weight))

df.lab.9$lab_sf_weight[df.lab.9$lab_id == 1212] <- 306
df.lab.9$lab_sf_weight[df.lab.9$lab_id == 1214] <- 306


# recode from the last data edit from kampala 09/09/19
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1077]

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1079] <- 18
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1079] <- 30

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1161] <- 4
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1161] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1162] <- 72
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1162] <- 198

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1163] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1163] <- 6

df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1164] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1164] <- 230

df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1166] <- 3
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1166] <- 37
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1166] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1166] <- NA
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1166] <- 32

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1167] <- 3
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1167] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1167] <- 18
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1167] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1169] <- 3
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1169] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1169] <- NA
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1169] <- 0

df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1173] <- 3
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1173] <- 25
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1173] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1173] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1173] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1173] <- 0

df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1174] <- 3
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1174] <- 39

df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1179] <- 3
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1179] <- 21
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1179] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1179] <- 0

df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1180] <- 19
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1180] <- 3

df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1191] <- 2
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1191] <- 0

df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1195] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1195] <- 0

df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1198] <- 35
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1198] <- 0

df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1203] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1213] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1213] <- 17

df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1221] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1221] <- 218

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1225] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1225] <- 26

df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1227] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1227] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1227] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1227] <- 0

df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1228] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1228] <- NA

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1230] <- 22
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1230] <- 50
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1230] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1232] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1232] <- 57

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1234] <- 22
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1234] <- 36

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1235] <- 46
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1235] <- 75
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1235] <- 0

df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1236] <- 3
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1236] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1237] <- 0
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1237] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1237] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1239] <- 4
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1239] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1239] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1239] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1239] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1240] <- 23
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1240] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1240] <- NA
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1240] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1242] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1242] <- 8

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1243] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1243] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1244] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1244] <- 9
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1244] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1245] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1245] <- 2
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1245] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1246] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1246] <- 37
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1246] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1248] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1248] <- 2

# recode from the last data edit from kampala 01/30/20
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1055] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1055] <- 1
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1055] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1057] <- 64
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1057] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1071] <- 15
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1071] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1071] <- 142
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1071] <- 16

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1077] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1077] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1077] <- 54
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1077] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1079] <- 30
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1079] <- 18

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1091] <- 39
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1091] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1096] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1096] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1159] <- 10
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1159] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1161] <- NA #TDTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1161] <- 2
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1161] <- 4
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1161] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1221] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1221] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1221] <- 85
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1221] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1223] <- 42
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1223] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1234] <- 36
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1234] <- 22

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1238] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1238] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1238] <- 59
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1238] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1239] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1239] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1239] <- 4
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1239] <- 3
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1239] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1240] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 1240] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1240] <- 23
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 1240] <- 3
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 1240] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1244] <- 9
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1244] <- 1

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1247] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1247] <- 1
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1247] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1354] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1354] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1357] <- 4
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1357] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1373] <- 7
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1373] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1386] <- 22
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1386] <- 8
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 1386] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1391] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1391] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2001] <- NA #TDTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 2001] <- 2
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2001] <- 16
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 2001] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2004] <- 15
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2004] <- 7

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2008] <- NA #TNTC
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 2008] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2008] <- 5
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 2008] <- 3

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2032] <- 57
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2032] <- 18
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2032] <- 2

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2033] <- 42
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 2033] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2033] <- 18
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 2033] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2033] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 2033] <- 2

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2034] <- 61
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2034] <- 52
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2034] <- 7

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2035] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2035] <- 0
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2035] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2038] <- 36
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2038] <- 6
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2038] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2039] <- 2
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2039] <- 1
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2039] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2040] <- 45
df.lab.9$lab_1_ecoli_reading_membrane[df.lab.9$lab_id == 2040] <- 3
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2040] <- NA
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 2040] <- 2
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2040] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 2040] <- 1

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2040] <- 6
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2040] <- 0
df.lab.9$lab_2_ecoli_reading_membrane[df.lab.9$lab_id == 2040] <- 3
df.lab.9$lab_3_ecoli_membrane[df.lab.9$lab_id == 2040] <- NA
df.lab.9$lab_3_ecoli_reading_membrane[df.lab.9$lab_id == 2040] <- 2

# another round of edits: 02/06/20
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1063] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1063] <- 9

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1067] <- 1
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1067] <- 14

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1069] <- 8
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1069] <- 65

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1163] <- 6
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1163] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1232] <- 57
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1232] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1242] <- 8
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1242] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1245] <- 2
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1245] <- 1

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1246] <- 0
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1246] <- 37

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1248] <- 2
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1248] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1353] <- NA #TNTC
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1367] <- NA #TNTC
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1368] <- NA #TNTC
df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1369] <- NA #TNTC

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 1406] <- 6
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 1406] <- 0

df.lab.9$lab_1_ecoli_membrane[df.lab.9$lab_id == 2027] <- 19
df.lab.9$lab_2_ecoli_membrane[df.lab.9$lab_id == 2027] <- 12






write.csv(df.col.9, paste0(getwd(), "/data/deployments/09_uganda/", "col_clean", ".csv"), row.names = F, na="")
write.csv(df.lab.9, paste0(getwd(), "/data/deployments/09_uganda/", "lab_clean", ".csv"), row.names = F, na="")



# 
# ##### import and export 
# df.c.9 <- read.csv(paste0(getwd(), "/data/", "c_merged_2019-04-18", ".csv"), sep=",", stringsAsFactors=F, header=T, 
#                    na.strings = c("n/a", "NA", "N/A", "---"))
# 
# df.s.9 <- read.csv(paste0(getwd(), "/data/", "s_merged_2019-04-18", ".csv"), sep=",", stringsAsFactors=F, header=T, 
#                    na.strings = c("n/a", "NA", "N/A", "---"))
# 
# df.h.9 <- read.csv(paste0(getwd(), "/data/", "h_merged_2019-04-18", ".csv"), sep=",", stringsAsFactors=F, header=T, 
#                    na.strings = c("n/a", "NA", "N/A", "---"))
# 
# df.c.9 <- df.c.9 %>% filter(dply_num == 9)
# df.s.9 <- df.s.9 %>% filter(dply_num == 9)
# df.h.9 <- df.h.9 %>% filter(dply_num == 9)
# 
# write.csv(df.c.9, paste0(getwd(), "/data/deployments/09_uganda/", "c_clean1", ".csv"), row.names = F, na="")
# write.csv(df.s.9, paste0(getwd(), "/data/deployments/09_uganda/", "s_clean1", ".csv"), row.names = F, na="")
# write.csv(df.h.9, paste0(getwd(), "/data/deployments/09_uganda/", "h_clean1", ".csv"), row.names = F, na="")


# 
# df.col.9 <- read.csv(paste0(getwd(), "/data/", "col_merged_2019-04-12", ".csv"), sep=",", stringsAsFactors=F, header=T,
#                    na.strings = c("n/a", "NA", "N/A", "---"))
# 
# df.col.9 <- df.col.9 %>% filter(dply_num == 9)
# write.csv(df.col.9, paste0(getwd(), "/data/deployments/09_uganda/", "col_clean", ".csv"), row.names = F, na="")
# 
# table(df.col.9$col_neighborhood, df.col.9$col_sample_type)
# table(df.lab.9$lab_sample_type)
# 
# 
# 


# 09 extra analysis

df.col <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                          "col_clean_odw", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))
df.lab <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                          "lab_clean_odw", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

df.s <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                        "s_clean1_odw", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))
df.s2 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                         "s_extra_09", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))
df.c <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                        "c_clean1_odw", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))
df.h <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                        "h_clean1_odw", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))
df.h2 <- read.csv(paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", 
                         "h_extra_09", ".csv"), sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))

df.s <- left_join(df.s, df.s2)
df.h <- left_join(df.h, df.h2)

# df.s ----
colnames(df.s)
df.s <- df.s %>% select(-c(s_dw_c_3, s_dw_c_2, s_dw_c_1, s_dw_c_0, s_dw_c_na, s_dw_a_3, s_dw_a_2, s_dw_a_1, s_dw_a_0, s_dw_a_na,
                           s_dw_e_wt_1, s_dw_e_wt_0, s_dw_e_wt_na))

df.s <- df.s %>% rename_at("s_odw_c_3",~"s_dw_c_3")
df.s <- df.s %>% rename_at("s_odw_c_2",~"s_dw_c_2")
df.s <- df.s %>% rename_at("s_odw_c_1",~"s_dw_c_1")
df.s <- df.s %>% rename_at("s_odw_c_0",~"s_dw_c_0")
df.s <- df.s %>% rename_at("s_odw_c_na",~"s_dw_c_na")
df.s <- df.s %>% rename_at("s_odw_a_3",~"s_dw_a_3")
df.s <- df.s %>% rename_at("s_odw_a_2",~"s_dw_a_2")
df.s <- df.s %>% rename_at("s_odw_a_1",~"s_dw_a_1")
df.s <- df.s %>% rename_at("s_odw_a_0",~"s_dw_a_0")
df.s <- df.s %>% rename_at("s_odw_a_na",~"s_dw_a_na")

# df.h ----
colnames(df.h)
df.h <- df.h %>% select(-c(h_dw_a, h_dw_c))
df.h <- df.h %>% rename_at("h_odw_a",~"h_dw_a")
df.h <- df.h %>% rename_at("h_odw_c",~"h_dw_c")
df.h <- df.h %>% filter(h_bw_source == 2) #spring water

# df.c
colnames(df.c)
df.c <- df.c %>% select(-c(c_dw_c_3, c_dw_c_2, c_dw_c_1, c_dw_c_0, c_dw_c_na, c_dw_a_3, c_dw_a_2, c_dw_a_1, c_dw_a_0, c_dw_a_na))


df.col <- df.col %>% filter(col_sample_type == 9) 
df.col$col_sample_type[df.col$col_sample_type == 9] <- 3

df.lab <- df.lab %>% filter(lab_sample_type == 9) 
df.lab$lab_sample_type[df.lab$lab_sample_type == 9] <- 3

# 
table(df.lab$lab_sample_type)
table(df.col$col_sample_type)


write.csv(df.col, paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", "sep_col_extra", ".csv"), row.names = F, na="")
write.csv(df.lab, paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", "sep_lab_extra", ".csv"), row.names = F, na="")
write.csv(df.h, paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", "sep_h_extra", ".csv"), row.names = F, na="")
write.csv(df.c, paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", "sep_c_extra", ".csv"), row.names = F, na="")
write.csv(df.s, paste0(getwd(), "/data/deployments/09_uganda/extra_analysis/", "sep_s_extra", ".csv"), row.names = F, na="")


