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







