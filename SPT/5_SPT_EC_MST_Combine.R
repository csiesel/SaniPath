
spt$ec_conc <- 0
spt$calc_wg5_conc <- 0
spt$calc_gb124_conc <- 0
spt$calc_wg5_enr_conc <- 0
spt$calc_gb124_enr_conc <- 0
# spt$ecid <-""
# spt$mstid <- ""
for(i in 1:nrow(spt)){
  spt$ec_conc[i] <- ifelse(is.na(spt$mf_index[i]), NA, ec_data_spt$ec_conc[which(spt$mf_index[i]==ec_data_spt$`_index.y`)])
  spt$calc_wg5_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$calc_wg5_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_gb124_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$calc_gb124_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_wg5_enr_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$calc_wg5_enr_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_gb124_enr_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$calc_gb124_enr_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  # spt$ecid[i] <- ifelse(is.na(spt$mf_index[i]), NA, ec_data_spt$sampleid[which(spt$mf_index[i]==ec_data_spt$`_index.y`)])
  # spt$mstid[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$lab_id[which(spt$mst_index[i]==mst_conc$`_index`)])
  
}


es$ec_conc <- 0
es$calc_wg5_conc <- 0
es$calc_gb124_conc <- 0
es$calc_wg5_enr_conc <- 0
es$calc_gb124_enr_conc <- 0
# es$ecid <-""
# es$mstid <- ""
for(i in 1:nrow(es)){
  es$ec_conc[i] <- ifelse(is.na(es$mf_index[i]), NA, ec_data_es$ec_conc[which(es$mf_index[i]==ec_data_es$`_index.y`)])
  es$calc_wg5_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$calc_wg5_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_gb124_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$calc_gb124_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_wg5_enr_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$calc_wg5_enr_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_gb124_enr_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$calc_gb124_enr_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  # es$ecid[i] <- ifelse(is.na(es$mf_index[i]), NA, ec_data_es$sampleid[which(es$mf_index[i]==ec_data_es$`_index.y`)])
  # es$mstid[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$lab_id[which(es$mst_index[i]==mst_conc$`_index`)])
  
}


#NEED TO REWORK THIS ONE
so$ec_conc <- 0
so$calc_wg5_conc <- 0
so$calc_gb124_conc <- 0
so$calc_wg5_enr_conc <- 0
so$calc_gb124_enr_conc <- 0
# so$ecid <-""
# so$mstid <- ""
for(i in 1:nrow(so)){
  so$ec_conc[i] <- ifelse(is.na(so$mf_index[i]), NA, ec_data_so$ec_conc[which(so$mf_index[i]==ec_data_so$`_index.y`)])
  so$calc_wg5_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$calc_wg5_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_gb124_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$calc_gb124_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_wg5_enr_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$calc_wg5_enr_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_gb124_enr_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$calc_gb124_enr_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  # so$ecid[i] <- ifelse(is.na(so$mf_index[i]), NA, ec_data_so$sampleid[which(so$mf_index[i]==ec_data_so$`_index.y`)])
  # so$mstid[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$lab_id[which(so$mst_index[i]==mst_conc$`_index`)])
  
}

