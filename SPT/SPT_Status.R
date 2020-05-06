library(svMisc)

#Once you source the file below, the "raw_#.#" files have the duplicate ID's fixed
source("SPT/SPT_dupid_fix.R")


#Little fixes (making everything uppercase, replacing spaces)
# * also doing special fixes for trigger numbers for PLc and
#  making FB####UP to FB so I can match with raw sample for enrichment
raw_2.1$col_id <- toupper(sub(" ", "", raw_2.1$col_id))
raw_2.2$col_id <- toupper(sub(" ", "", raw_2.2$col_id))
raw_2.4$col_id <- toupper(sub(" ", "", raw_2.4$col_id))
raw_2.4$col_sw_trigger_nr <- gsub(" ' ' | & | ,", "", raw_2.4$col_sw_trigger_nr)
raw_2.4$col_sw_trigger_nr <- str_split(raw_2.4$col_sw_trigger_nr, "\\,")


raw_3.1$lab_id <- toupper(sub(" ", "", raw_3.1$lab_id))
raw_3.2$lab_id <- toupper(sub(" ", "", raw_3.2$lab_id))
raw_3.3$lab_id <- toupper(sub(" ", "", raw_3.3$lab_id))
raw_3.4$lab_id <- toupper(sub(" ", "", raw_3.4$lab_id))
raw_3.5$lab_id <- toupper(sub(" ", "", raw_3.5$lab_id))
raw_3.5$temp_id <- str_remove_all(raw_3.5$lab_id, "UP$")

#### SPT ####
# Setting up SPT version of wolfgang's status tab
spt <- raw_2.1 %>%
  mutate(form=2.1,
         date=as.character(as.Date(col_start)),
         sample_index=`_index`, sample_type=col_sample_type, sample_ward=col_ward, sample_hood=col_neighborhood,
         mf_date="", mf_index="", mf_type="", mf_ward="", mf_hood="",
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "ncec", "ncec_ec", "upnc", "upnc_tb"))

#The function below for SPT and the same for ES is duplicating Wolfgang's status page
# Essentially pulls in a date if the ID has a match in the lab forms
for(i in 1:nrow(spt)){
  #MF
  spt$mf_date[i] <- ifelse(is_empty(raw_3.1$lab_processing[which(spt$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.1$lab_processing[which(spt$col_id[i]==raw_3.1$lab_id)])))
  spt$mf_index[i] <- ifelse(is_empty(raw_3.1$`_index`[which(spt$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$`_index`[which(spt$col_id[i]==raw_3.1$lab_id)])
  spt$mf_type[i] <- ifelse(is_empty(raw_3.1$lab_sample_type[which(spt$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_sample_type[which(spt$col_id[i]==raw_3.1$lab_id)])
  spt$mf_ward[i] <- ifelse(is_empty(raw_3.1$lab_ward[which(spt$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_ward[which(spt$col_id[i]==raw_3.1$lab_id)])
  spt$mf_hood[i] <- ifelse(is_empty(raw_3.1$lab_neighborhood[which(spt$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_neighborhood[which(spt$col_id[i]==raw_3.1$lab_id)])
  
  #MST
  spt$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(spt$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.2$lab_processing[which(spt$col_id[i]==raw_3.2$lab_id)])))
  spt$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(spt$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$`_index`[which(spt$col_id[i]==raw_3.2$lab_id)])
  spt$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(spt$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_sample_type[which(spt$col_id[i]==raw_3.2$lab_id)])
  spt$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(spt$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_ward[which(spt$col_id[i]==raw_3.2$lab_id)])
  spt$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(spt$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_neighborhood[which(spt$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  spt$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(spt$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.3$lab_dna_date[which(spt$col_id[i]==raw_3.3$lab_id)])))
  spt$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(spt$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$`_index`[which(spt$col_id[i]==raw_3.3$lab_id)])
  spt$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(spt$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_sample_type[which(spt$col_id[i]==raw_3.3$lab_id)])
  spt$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(spt$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_ward[which(spt$col_id[i]==raw_3.3$lab_id)])
  spt$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(spt$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_neighborhood[which(spt$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  spt$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(spt$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(spt$col_id[i]==raw_3.4$lab_id)])))
  spt$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(spt$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$`_index`[which(spt$col_id[i]==raw_3.4$lab_id)])
  spt$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(spt$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_sample_type[which(spt$col_id[i]==raw_3.4$lab_id)])
  spt$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(spt$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_ward[which(spt$col_id[i]==raw_3.4$lab_id)])
  spt$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(spt$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_neighborhood[which(spt$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  spt$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(spt$col_id[i]==raw_3.5$temp_id)]),
                            NA, 
                            as.character(as.Date(raw_3.5$lab_processing[which(spt$col_id[i]==raw_3.5$temp_id)])))
  spt$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(spt$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$`_index`[which(spt$col_id[i]==raw_3.5$lab_id)])
  spt$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(spt$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_sample_type[which(spt$col_id[i]==raw_3.5$lab_id)])
  spt$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(spt$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_ward[which(spt$col_id[i]==raw_3.5$lab_id)])
  spt$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(spt$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_neighborhood[which(spt$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  spt$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(spt$date[i]==as.character(as.Date(raw_2.1$col_start)) & 
                                                      grepl("FB", raw_2.1$col_id)==TRUE)]),
                      NA,
                      strsplit(str_c(raw_2.1$col_id[which(spt$date[i]==as.character(as.Date(raw_2.1$col_start)) & 
                                                   grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  

  spt$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt$fb[[i]])]),
                      NA,
                      str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt$fb[[i]])], sep=", ", collapse=", "))
  
  spt$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(spt$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                        grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                      NA,
                      str_c(raw_3.1$lab_id[which(spt$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                   grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  spt$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt$ncec[[i]])]),
                         NA,
                         str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt$ncec[[i]])], sep=", ", collapse=", "))
  
  spt$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(spt$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                        grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                        NA,
                        str_c(raw_3.5$lab_id[which(spt$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                     grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  
  if (i==1) message("SPT")
  progress(value=(((i-1)/nrow(spt))*100), progress.bar=TRUE)
  if (i==nrow(spt)) message("DONE!")
}

spt$col_id[which(spt$date != spt$mf_date)]

spt$col_id[which(spt$sample_type != spt$mf_type | 
                   spt$sample_type !=spt$mst_type | 
                   spt$sample_type !=spt$dna_type | 
                   spt$sample_type !=spt$pcr_type | 
                   spt$sample_type !=spt$enr_type)]




#### ES ####
# Setting up ES version of wolfgang's status tab

es <- raw_2.4 %>%
  mutate(form=2.4,
         date=ifelse(col_sample_type !=12, as.character(as.Date(col_start)), as.character(as.Date(col_ms_in))),
         sample_index=`_index`, sample_type=col_sample_type, sample_ward=col_ward, sample_hood=NA,
         mf_date="", mf_index="", mf_type="", mf_ward="", mf_hood="",
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "ncec", "ncec_ec", "upnc", "upnc_tb"))

for(i in 1:nrow(es)){
  #MF
  es$mf_date[i] <- ifelse(is_empty(raw_3.1$lab_processing[which(es$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.1$lab_processing[which(es$col_id[i]==raw_3.1$lab_id)])))
  es$mf_index[i] <- ifelse(is_empty(raw_3.1$`_index`[which(es$col_id[i]==raw_3.1$lab_id)]),
                            NA, 
                            raw_3.1$`_index`[which(es$col_id[i]==raw_3.1$lab_id)])
  es$mf_type[i] <- ifelse(is_empty(raw_3.1$lab_sample_type[which(es$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_sample_type[which(es$col_id[i]==raw_3.1$lab_id)])
  es$mf_ward[i] <- ifelse(is_empty(raw_3.1$lab_ward[which(es$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_ward[which(es$col_id[i]==raw_3.1$lab_id)])
  es$mf_hood[i] <- ifelse(is_empty(raw_3.1$lab_neighborhood[which(es$col_id[i]==raw_3.1$lab_id)]),
                           NA, 
                           raw_3.1$lab_neighborhood[which(es$col_id[i]==raw_3.1$lab_id)])
  
  #MST
  es$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(es$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.2$lab_processing[which(es$col_id[i]==raw_3.2$lab_id)])))
  es$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(es$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             raw_3.2$`_index`[which(es$col_id[i]==raw_3.2$lab_id)])
  es$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(es$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_sample_type[which(es$col_id[i]==raw_3.2$lab_id)])
  es$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(es$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_ward[which(es$col_id[i]==raw_3.2$lab_id)])
  es$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(es$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_neighborhood[which(es$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  es$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(es$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.3$lab_dna_date[which(es$col_id[i]==raw_3.3$lab_id)])))
  es$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(es$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$`_index`[which(es$col_id[i]==raw_3.3$lab_id)])
  es$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(es$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_sample_type[which(es$col_id[i]==raw_3.3$lab_id)])
  es$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(es$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_ward[which(es$col_id[i]==raw_3.3$lab_id)])
  es$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(es$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_neighborhood[which(es$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  es$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(es$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(es$col_id[i]==raw_3.4$lab_id)])))
  es$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(es$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$`_index`[which(es$col_id[i]==raw_3.4$lab_id)])
  es$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(es$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_sample_type[which(es$col_id[i]==raw_3.4$lab_id)])
  es$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(es$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_ward[which(es$col_id[i]==raw_3.4$lab_id)])
  es$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(es$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_neighborhood[which(es$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  es$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(es$col_id[i]==raw_3.5$temp_id)]),
                            NA, 
                            as.character(as.Date(raw_3.5$lab_processing[which(es$col_id[i]==raw_3.5$temp_id)])))
  es$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(es$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$`_index`[which(es$col_id[i]==raw_3.5$lab_id)])
  es$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(es$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_sample_type[which(es$col_id[i]==raw_3.5$lab_id)])
  es$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(es$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_ward[which(es$col_id[i]==raw_3.5$lab_id)])
  es$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(es$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_neighborhood[which(es$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  es$fb[i] <- ifelse(is_empty(raw_2.4$col_id[which(es$date[i]==as.character(as.Date(raw_2.4$col_start)) & 
                                                      grepl("FB", raw_2.4$col_id)==TRUE)]),
                      NA,
                     strsplit(str_c(raw_2.4$col_id[which(es$date[i]==as.character(as.Date(raw_2.4$col_start)) & 
                                                   grepl("FB", raw_2.4$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  es$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es$fb[[i]])]),
                         NA,
                         str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es$fb[[i]])], sep=", ", collapse=", "))
  
  es$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(es$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                        grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                        NA,
                        str_c(raw_3.1$lab_id[which(es$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                     grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  es$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es$ncec[[i]])]),
                           NA,
                           str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es$ncec[[i]])], sep=", ", collapse=", "))
  
  es$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(es$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                        grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                        NA,
                        str_c(raw_3.5$lab_id[which(es$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                     grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  
  if (i==1) message("ES")
  progress(value=(((i-1)/nrow(es))*100), progress.bar=TRUE)
  if (i==nrow(es)) message("DONE!")
}




#### MISC ####
# 
# 
# spt_samples <- c(1,2,3,5,6,7,8,9,10,13,99,98,97)
# 
# spt_raw_3.1 <- raw_3.1 %>%
#   filter(lab_sample_type %in% spt_samples) %>%
# spt_raw_3.2 <- raw_3.2 %>%
#   filter(lab_sample_type %in% spt_samples)
# spt_raw_3.3 <- raw_3.3 %>%
#   filter(lab_sample_type %in% spt_samples)
# spt_raw_3.4 <- raw_3.4 %>%
#   filter(lab_sample_type %in% spt_samples)
# spt_raw_3.5 <- raw_3.5 %>%
#   filter(lab_sample_type %in% spt_samples)
# 

