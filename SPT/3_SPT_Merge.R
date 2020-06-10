source("~/Desktop/SaniPath/SPT/2_SPT_Status.R")

#### Setting final files ####
col_spt <- raw_2.1
col_so <- raw_2.2
col_es <- raw_2.4
mf <- raw_3.1
mst <- raw_3.2
dna <- raw_3.3
pcr <- raw_3.4
enr <- raw_3.5

#### Cleaning mf data for PLc/PL volume issues ####
probs <-
  toupper(c(
    "PLc1025",
    "PLc1036",
    "PLc1038",
    "PLc1039",
    "PLc1040",
    "PLc1041",
    "PLc1042",
    "PLc1043",
    "PLc1049",
    "PLc1050",
    "PLc1051",
    "PLc1052",
    "PLc1053",
    "PLc1054",
    "PLc1055",
    "PLc1056",
    "PLc1057",
    "PLc1058",
    "PLc1059",
    "PLc1062",
    "PLc1063",
    "PLc1065",
    'PLc1073',
    'PLc1074',
    'PLc1075',
    'PLc1077',
    'PLc1078',
    'PLc1079',
    'PLc1080',
    'PLc1095',
    'PLc1096',
    'PLc1097',
    'PLc1098',
    'PLc1099',
    'PLc1107',
    'PLc1108',
    'PLc1109',
    'PLc1110',
    'PLc1111',
    'PLc1112',
    'PLc1113',
    'PLc1114',
    'PLc1115',
    'PLc1116',
    'PLc1117',
    'PLc1135',
    'PLc1136',
    'PLc1137',
    'PLc1138',
    'PLc1139',
    'PLc1142',
    'PLc1143',
    'PLc1144',
    'PLc1145',
    'PLc1173',
    'PLc1174',
    'PLc1175',
    'PLc1176',
    'PLc1177',
    'PLc1178',
    'PLc1180',
    'PLc1181',
    'PLc1182',
    'PLc1183',
    'PL1021',
    'PL1022',
    'PL1023',
    'SW1010',
    'SW2001',
    'SF2001',
    'SL1012',
    'SL2001',
    'SL2002',
    'UF1005',
    'UF1006',
    'UF1007',
    'UF1009',
    'UF1012',
    'UF1015',
    'UF1017'
  ))

mf$lab_1_volume[which(mf$lab_id %in% probs)] <- 10
mf$lab_2_volume[which(mf$lab_id %in% probs)] <- 10
mf$lab_3_volume[which(mf$lab_id %in% probs)] <- 10


#### SPT Merge ####
spt_sample <- merge(spt, raw_2.1, by.x="sample_index", by.y="_index", all.x=TRUE)
spt_mf <- merge(spt_sample, raw_3.1, by.x="mf_index", by.y="_index", all.x = TRUE)
spt_mf_mst <- merge(spt_mf, raw_3.2, by.x="mst_index", by.y="_index", all.x =TRUE)
spt_mf_mst$enr_done <- ifelse(is.na(spt_mf_mst$enr_date), 0, 1)
spt_mf_mst$dna_done <- ifelse(is.na(spt_mf_mst$dna_date), 0, 1)
spt_mf_mst_pcr <- merge(spt_mf_mst, raw_3.4, by.x="pcr_index", by.y="_index", all.x = TRUE)
spt_full <- spt_mf_mst_pcr
rm(spt_sample, spt_mf, spt_mf_mst, spt_mf_mst_pcr)


#### ES Merge ####
es_sample <- merge(es, raw_2.4, by.x="sample_index", by.y="_index", all.x=TRUE)
es_mf <- merge(es_sample, raw_3.1, by.x="mf_index", by.y="_index", all.x = TRUE)
es_mf_mst <- merge(es_mf, raw_3.2, by.x="mst_index", by.y="_index", all.x =TRUE)
es_mf_mst$enr_done <- ifelse(is.na(es_mf_mst$enr_date), 0, 1)
es_mf_mst$dna_done <- ifelse(is.na(es_mf_mst$dna_date), 0, 1)
es_mf_mst_pcr <- merge(es_mf_mst, raw_3.4, by.x="pcr_index", by.y="_index", all.x = TRUE)
es_full <- es_mf_mst_pcr
rm(es_sample, es_mf, es_mf_mst, es_mf_mst_pcr)


#### SO Merge ####
# removing non SO lab entries and removing training data
mf_so <- mf %>% filter(lab_sample_type %in% c(15, 14))
mf_so <- mf_so[-c(1:7),]
# merging the files by the SO index with sampling info
mf_so$so_index=NA
for(i in 1:nrow(mf_so)){
  mf_so$so_index[i]<-col_so$`_index`[which(
    mf_so$lab_id[i] == col_so$col_id_so |
    mf_so$lab_id[i] == col_so$col_coa_id |
    mf_so$lab_id[i] == col_so$col_cob_ids |
    mf_so$lab_id[i] == col_so$col_cob_ids2 |
    mf_so$lab_id[i] == col_so$col_cob_ids3 |
    mf_so$lab_id[i] == col_so$col_cob_ids4)]
}


so <- merge(col_so, mf_so, by.y="so_index", by.x="_index")







