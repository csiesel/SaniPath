path <- "/Users/caseysiesel/Desktop/SaniPath/"

source(paste0(path, "SPT/5_SPT_Final_Status.R"))



#### SPT EC/MST Combine ####
spt$ec_conc <- 0
spt$calc_wg5_conc <- 0
spt$calc_gb124_conc <- 0
spt$calc_wg5_enr_conc <- 0
spt$calc_gb124_enr_conc <- 0
spt$typhi_positive <- 0
spt$typhi_presumptive <- 0
spt$paratyphi_positive <- 0
spt$paratyphi_presumptive <- 0

spt$t1 <-0
spt$t2 <- 0
spt$p1 <- 0
spt$p2 <- 0




# spt$ecid <-""
# spt$mstid <- ""
for(i in 1:nrow(spt)){
  spt$ec_conc[i] <- ifelse(is.na(spt$mf_index[i]), NA, 
                           ec_data_spt$ec_conc[which(spt$mf_index[i]==ec_data_spt$`_index.y`)])
  spt$calc_wg5_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, 
                                 mst_conc$calc_wg5_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_gb124_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, 
                                   mst_conc$calc_gb124_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_wg5_enr_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, 
                                     mst_conc$calc_wg5_enr_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  spt$calc_gb124_enr_conc[i] <- ifelse(is.na(spt$mst_index[i]), NA, 
                                       mst_conc$calc_gb124_enr_conc[which(spt$mst_index[i]==mst_conc$`_index`)])
  # spt$ecid[i] <- ifelse(is.na(spt$mf_index[i]), NA, ec_data_spt$sampleid[which(spt$mf_index[i]==ec_data_spt$`_index.y`)])
  # spt$mstid[i] <- ifelse(is.na(spt$mst_index[i]), NA, mst_conc$lab_id[which(spt$mst_index[i]==mst_conc$`_index`)])

  
  spt$typhi_positive[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                                  pcr$typhi_positive[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$typhi_presumptive[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                                     pcr$typhi_presumptive[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$paratyphi_positive[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                                  pcr$paratyphi_positive[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$paratyphi_presumptive[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                                      pcr$paratyphi_presumptive[which(spt$pcr_index[i]==pcr$`_index`)])
  
  
  spt$t1[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results1[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$t2[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results2[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$p1[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results1[which(spt$pcr_index[i]==pcr$`_index`)])
  spt$p2[i] <- ifelse(is.na(spt$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results2[which(spt$pcr_index[i]==pcr$`_index`)])
  
  
  spt$fb_pcr[i] <- ifelse(is_empty(pcr$lab_id[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))]),
                          NA,
                          str_c(pcr$lab_id[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt$fb_typhi_pos[i] <-ifelse(is_empty(pcr$typhi_positive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))]),
                                              NA,
                                              str_c(pcr$typhi_positive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt$fb_typhi_pres[i] <-ifelse(is_empty(pcr$typhi_presumptive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))]),
                               NA,
                               str_c(pcr$typhi_presumptive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt$fb_paratyphi_pos[i] <-ifelse(is_empty(pcr$paratyphi_positive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))]),
                               NA,
                               str_c(pcr$paratyphi_positive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt$fb_paratyphi_pres[i] <-ifelse(is_empty(pcr$paratyphi_presumptive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))]),
                                NA,
                                str_c(pcr$paratyphi_presumptive[which(grepl(paste(spt$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  
  
  if (i==1) message("SPT")
  progress(value=(((i-1)/nrow(spt))*100), progress.bar=TRUE)
  if (i==nrow(spt)) message("DONE!")
}

spt_final <- spt %>% select(-c("mf_ward", "mf_hood", "mst_ward", "mst_hood", "dna_ward", "dna_hood", "enr_ward", "enr_hood",
                               "pcr_ward", "pcr_hood"))




#### ES EC/MST Combine ####
es$ec_conc <- 0
es$calc_wg5_conc <- 0
es$calc_gb124_conc <- 0
es$calc_wg5_enr_conc <- 0
es$calc_gb124_enr_conc <- 0
es$typhi_positive <- 0
es$typhi_presumptive <- 0
es$paratyphi_positive <- 0
es$paratyphi_presumptive <- 0

es$t1 <-0
es$t2 <- 0
es$p1 <- 0
es$p2 <- 0




# es$ecid <-""
# es$mstid <- ""
for(i in 1:nrow(es)){
  es$ec_conc[i] <- ifelse(is.na(es$mf_index[i]), NA, 
                          ec_data_es$ec_conc[which(es$mf_index[i]==ec_data_es$`_index.y`)])
  es$calc_wg5_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, 
                                mst_conc$calc_wg5_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_gb124_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, 
                                  mst_conc$calc_gb124_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_wg5_enr_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, 
                                    mst_conc$calc_wg5_enr_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  es$calc_gb124_enr_conc[i] <- ifelse(is.na(es$mst_index[i]), NA, 
                                      mst_conc$calc_gb124_enr_conc[which(es$mst_index[i]==mst_conc$`_index`)])
  # es$ecid[i] <- ifelse(is.na(es$mf_index[i]), NA, ec_data_es$sampleid[which(es$mf_index[i]==ec_data_es$`_index.y`)])
  # es$mstid[i] <- ifelse(is.na(es$mst_index[i]), NA, mst_conc$lab_id[which(es$mst_index[i]==mst_conc$`_index`)])
  
  
  es$typhi_positive[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                                  pcr$typhi_positive[which(es$pcr_index[i]==pcr$`_index`)])
  es$typhi_presumptive[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                                    pcr$typhi_presumptive[which(es$pcr_index[i]==pcr$`_index`)])
  es$paratyphi_positive[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                                      pcr$paratyphi_positive[which(es$pcr_index[i]==pcr$`_index`)])
  es$paratyphi_presumptive[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                                         pcr$paratyphi_presumptive[which(es$pcr_index[i]==pcr$`_index`)])
  
  
  es$t1[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results1[which(es$pcr_index[i]==pcr$`_index`)])
  es$t2[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results2[which(es$pcr_index[i]==pcr$`_index`)])
  es$p1[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results1[which(es$pcr_index[i]==pcr$`_index`)])
  es$p2[i] <- ifelse(is.na(es$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results2[which(es$pcr_index[i]==pcr$`_index`)])
  
  
  es$fb_pcr[i] <- ifelse(is_empty(pcr$lab_id[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))]),
                          NA,
                          str_c(pcr$lab_id[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es$fb_typhi_pos[i] <-ifelse(is_empty(pcr$typhi_positive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))]),
                               NA,
                               str_c(pcr$typhi_positive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es$fb_typhi_pres[i] <-ifelse(is_empty(pcr$typhi_presumptive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))]),
                                NA,
                                str_c(pcr$typhi_presumptive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es$fb_paratyphi_pos[i] <-ifelse(is_empty(pcr$paratyphi_positive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))]),
                                   NA,
                                   str_c(pcr$paratyphi_positive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es$fb_paratyphi_pres[i] <-ifelse(is_empty(pcr$paratyphi_presumptive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))]),
                                    NA,
                                    str_c(pcr$paratyphi_presumptive[which(grepl(paste(es$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  
  if (i==1) message("ES")
  progress(value=(((i-1)/nrow(es))*100), progress.bar=TRUE)
  if (i==nrow(es)) message("DONE!")
}


es_final <- es %>% select(-c("mf_ward", "mf_hood", "mst_ward", "mst_hood", "dna_ward", "dna_hood", "enr_ward", "enr_hood",
                               "pcr_ward", "pcr_hood"))


#### SO EC/MST Combine ####
so$ec_conc <- 0
so$calc_wg5_conc <- 0
so$calc_gb124_conc <- 0
so$calc_wg5_enr_conc <- 0
so$calc_gb124_enr_conc <- 0

so$typhi_positive <- 0
so$typhi_presumptive <- 0
so$paratyphi_positive <- 0
so$paratyphi_presumptive <- 0

so$t1 <-0
so$t2 <- 0
so$p1 <- 0
so$p2 <- 0



# so$ecid <-""
# so$mstid <- ""
for(i in 1:nrow(so)){
  so$ec_conc[i] <- ifelse(is.na(so$mf_index[i]), NA, 
                          ec_data_so$ec_conc[which(so$mf_index[i]==ec_data_so$`_index.y`)])
  so$calc_wg5_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, 
                                mst_conc$calc_wg5_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_gb124_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, 
                                  mst_conc$calc_gb124_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_wg5_enr_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, 
                                    mst_conc$calc_wg5_enr_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  so$calc_gb124_enr_conc[i] <- ifelse(is.na(so$mst_index[i]), NA, 
                                      mst_conc$calc_gb124_enr_conc[which(so$mst_index[i]==mst_conc$`_index`)])
  # so$ecid[i] <- ifelse(is.na(so$mf_index[i]), NA, ec_data_so$sampleid[which(so$mf_index[i]==ec_data_so$`_index.y`)])
  # so$mstid[i] <- ifelse(is.na(so$mst_index[i]), NA, mst_conc$lab_id[which(so$mst_index[i]==mst_conc$`_index`)])
  
  
  so$typhi_positive[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                                 pcr$typhi_positive[which(so$pcr_index[i]==pcr$`_index`)])
  so$typhi_presumptive[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                                    pcr$typhi_presumptive[which(so$pcr_index[i]==pcr$`_index`)])
  so$paratyphi_positive[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                                     pcr$paratyphi_positive[which(so$pcr_index[i]==pcr$`_index`)])
  so$paratyphi_presumptive[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                                        pcr$paratyphi_presumptive[which(so$pcr_index[i]==pcr$`_index`)])
  
  
  so$t1[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                     pcr$lab_pcr_ty_results1[which(so$pcr_index[i]==pcr$`_index`)])
  so$t2[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                     pcr$lab_pcr_ty_results2[which(so$pcr_index[i]==pcr$`_index`)])
  so$p1[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                     pcr$lab_pcr_para_results1[which(so$pcr_index[i]==pcr$`_index`)])
  so$p2[i] <- ifelse(is.na(so$pcr_index[i]), NA,
                     pcr$lab_pcr_para_results2[which(so$pcr_index[i]==pcr$`_index`)])
  
  
  so$fb_pcr[i] <- ifelse(is_empty(pcr$lab_id[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))]),
                          NA,
                          str_c(pcr$lab_id[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  so$fb_typhi_pos[i] <-ifelse(is_empty(pcr$typhi_positive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))]),
                               NA,
                               str_c(pcr$typhi_positive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  so$fb_typhi_pres[i] <-ifelse(is_empty(pcr$typhi_presumptive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))]),
                                NA,
                                str_c(pcr$typhi_presumptive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  so$fb_paratyphi_pos[i] <-ifelse(is_empty(pcr$paratyphi_positive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))]),
                                   NA,
                                   str_c(pcr$paratyphi_positive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  so$fb_paratyphi_pres[i] <-ifelse(is_empty(pcr$paratyphi_presumptive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))]),
                                    NA,
                                    str_c(pcr$paratyphi_presumptive[which(grepl(paste(so$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  
  if (i==1) message("SO")
  progress(value=(((i-1)/nrow(so))*100), progress.bar=TRUE)
  if (i==nrow(so)) message("DONE!")
  
}

so_final <- so %>% select(-c("mf_ward", "mf_hood", "mst_ward", "mst_hood", "dna_ward", "dna_hood", "enr_ward", "enr_hood",
                             "pcr_ward", "pcr_hood"))




#### spt1 EC/MST Combine ####
spt1$ec_conc <- 0
spt1$calc_wg5_conc <- 0
spt1$calc_gb124_conc <- 0
spt1$calc_wg5_enr_conc <- 0
spt1$calc_gb124_enr_conc <- 0
spt1$typhi_positive <- 0
spt1$typhi_presumptive <- 0
spt1$paratyphi_positive <- 0
spt1$paratyphi_presumptive <- 0

spt1$t1 <-0
spt1$t2 <- 0
spt1$p1 <- 0
spt1$p2 <- 0




# spt1$ecid <-""
# spt1$mstid <- ""
for(i in 1:nrow(spt1)){
  spt1$ec_conc[i] <- ifelse(is.na(spt1$mf_index[i]), NA, 
                            ec_data_spt$ec_conc[which(spt1$mf_index[i]==ec_data_spt$`_index.y`)])
  spt1$calc_wg5_conc[i] <- ifelse(is.na(spt1$mst_index[i]), NA, 
                                  mst_conc$calc_wg5_conc[which(spt1$mst_index[i]==mst_conc$`_index`)])
  spt1$calc_gb124_conc[i] <- ifelse(is.na(spt1$mst_index[i]), NA, 
                                    mst_conc$calc_gb124_conc[which(spt1$mst_index[i]==mst_conc$`_index`)])
  spt1$calc_wg5_enr_conc[i] <- ifelse(is.na(spt1$mst_index[i]), NA, 
                                      mst_conc$calc_wg5_enr_conc[which(spt1$mst_index[i]==mst_conc$`_index`)])
  spt1$calc_gb124_enr_conc[i] <- ifelse(is.na(spt1$mst_index[i]), NA, 
                                        mst_conc$calc_gb124_enr_conc[which(spt1$mst_index[i]==mst_conc$`_index`)])
  # spt1$ecid[i] <- ifelse(is.na(spt1$mf_index[i]), NA, ec_data_spt$sampleid[which(spt1$mf_index[i]==ec_data_spt$`_index.y`)])
  # spt1$mstid[i] <- ifelse(is.na(spt1$mst_index[i]), NA, mst_conc$lab_id[which(spt1$mst_index[i]==mst_conc$`_index`)])
  
  
  spt1$typhi_positive[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                                   pcr$typhi_positive[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$typhi_presumptive[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                                      pcr$typhi_presumptive[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$paratyphi_positive[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                                       pcr$paratyphi_positive[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$paratyphi_presumptive[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                                          pcr$paratyphi_presumptive[which(spt1$pcr_index[i]==pcr$`_index`)])
  
  
  spt1$t1[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                       pcr$lab_pcr_ty_results1[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$t2[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                       pcr$lab_pcr_ty_results2[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$p1[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                       pcr$lab_pcr_para_results1[which(spt1$pcr_index[i]==pcr$`_index`)])
  spt1$p2[i] <- ifelse(is.na(spt1$pcr_index[i]), NA,
                       pcr$lab_pcr_para_results2[which(spt1$pcr_index[i]==pcr$`_index`)])
  
  
  spt1$fb_pcr[i] <- ifelse(is_empty(pcr$lab_id[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))]),
                           NA,
                           str_c(pcr$lab_id[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt1$fb_typhi_pos[i] <-ifelse(is_empty(pcr$typhi_positive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                NA,
                                str_c(pcr$typhi_positive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt1$fb_typhi_pres[i] <-ifelse(is_empty(pcr$typhi_presumptive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                 NA,
                                 str_c(pcr$typhi_presumptive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt1$fb_paratyphi_pos[i] <-ifelse(is_empty(pcr$paratyphi_positive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                    NA,
                                    str_c(pcr$paratyphi_positive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  spt1$fb_paratyphi_pres[i] <-ifelse(is_empty(pcr$paratyphi_presumptive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                     NA,
                                     str_c(pcr$paratyphi_presumptive[which(grepl(paste(spt1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  
  
  if (i==1) message("spt1")
  progress(value=(((i-1)/nrow(spt1))*100), progress.bar=TRUE)
  if (i==nrow(spt1)) message("DONE!")
}

spt1_final <- spt1 %>% select(-c("mf_ward", "mf_hood", "mst_ward", "mst_hood", "dna_ward", "dna_hood", "enr_ward", "enr_hood",
                                 "pcr_ward", "pcr_hood"))




#### es1 EC/MST Combine ####
es1$ec_conc <- 0
es1$calc_wg5_conc <- 0
es1$calc_gb124_conc <- 0
es1$calc_wg5_enr_conc <- 0
es1$calc_gb124_enr_conc <- 0
es1$typhi_positive <- 0
es1$typhi_presumptive <- 0
es1$paratyphi_positive <- 0
es1$paratyphi_presumptive <- 0

es1$t1 <-0
es1$t2 <- 0
es1$p1 <- 0
es1$p2 <- 0




# es1$ecid <-""
# es1$mstid <- ""
for(i in 1:nrow(es1)){
  es1$ec_conc[i] <- ifelse(is.na(es1$mf_index[i]), NA, 
                           ec_data_es$ec_conc[which(es1$mf_index[i]==ec_data_es$`_index.y`)])
  es1$calc_wg5_conc[i] <- ifelse(is.na(es1$mst_index[i]), NA, 
                                 mst_conc$calc_wg5_conc[which(es1$mst_index[i]==mst_conc$`_index`)])
  es1$calc_gb124_conc[i] <- ifelse(is.na(es1$mst_index[i]), NA, 
                                   mst_conc$calc_gb124_conc[which(es1$mst_index[i]==mst_conc$`_index`)])
  es1$calc_wg5_enr_conc[i] <- ifelse(is.na(es1$mst_index[i]), NA, 
                                     mst_conc$calc_wg5_enr_conc[which(es1$mst_index[i]==mst_conc$`_index`)])
  es1$calc_gb124_enr_conc[i] <- ifelse(is.na(es1$mst_index[i]), NA, 
                                       mst_conc$calc_gb124_enr_conc[which(es1$mst_index[i]==mst_conc$`_index`)])
  # es1$ecid[i] <- ifelse(is.na(es1$mf_index[i]), NA, ec_data_es$sampleid[which(es1$mf_index[i]==ec_data_es$`_index.y`)])
  # es1$mstid[i] <- ifelse(is.na(es1$mst_index[i]), NA, mst_conc$lab_id[which(es1$mst_index[i]==mst_conc$`_index`)])
  
  
  es1$typhi_positive[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                                  pcr$typhi_positive[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$typhi_presumptive[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                                     pcr$typhi_presumptive[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$paratyphi_positive[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                                      pcr$paratyphi_positive[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$paratyphi_presumptive[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                                         pcr$paratyphi_presumptive[which(es1$pcr_index[i]==pcr$`_index`)])
  
  
  es1$t1[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results1[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$t2[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                      pcr$lab_pcr_ty_results2[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$p1[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results1[which(es1$pcr_index[i]==pcr$`_index`)])
  es1$p2[i] <- ifelse(is.na(es1$pcr_index[i]), NA,
                      pcr$lab_pcr_para_results2[which(es1$pcr_index[i]==pcr$`_index`)])
  
  
  es1$fb_pcr[i] <- ifelse(is_empty(pcr$lab_id[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))]),
                          NA,
                          str_c(pcr$lab_id[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es1$fb_typhi_pos[i] <-ifelse(is_empty(pcr$typhi_positive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))]),
                               NA,
                               str_c(pcr$typhi_positive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es1$fb_typhi_pres[i] <-ifelse(is_empty(pcr$typhi_presumptive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                NA,
                                str_c(pcr$typhi_presumptive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es1$fb_paratyphi_pos[i] <-ifelse(is_empty(pcr$paratyphi_positive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                   NA,
                                   str_c(pcr$paratyphi_positive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  es1$fb_paratyphi_pres[i] <-ifelse(is_empty(pcr$paratyphi_presumptive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))]),
                                    NA,
                                    str_c(pcr$paratyphi_presumptive[which(grepl(paste(es1$fb[[i]], collapse="|"), pcr$lab_id))], sep=", ", collapse=", "))
  
  
  if (i==1) message("es1")
  progress(value=(((i-1)/nrow(es1))*100), progress.bar=TRUE)
  if (i==nrow(es1)) message("DONE!")
}


es1_final <- es1 %>% select(-c("mf_ward", "mf_hood", "mst_ward", "mst_hood", "dna_ward", "dna_hood", "enr_ward", "enr_hood",
                               "pcr_ward", "pcr_hood"))


save(spt1_final, file="~/Desktop/SaniPath/SPT/data/spt1_final.rda")
save(es1_final, file="~/Desktop/SaniPath/SPT/data/es1_final.rda")
save(so_final, file="~/Desktop/SaniPath/SPT/data/so_final.rda")




