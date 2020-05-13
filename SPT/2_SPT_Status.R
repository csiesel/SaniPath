library(svMisc)
library(tidyverse)

#### Getting raw data files ####
#    (with duplicate IDs already fixed) 
#Once you source the file below, the "raw_#.#" files have the duplicate ID's fixed
#UNCOMMENT TO REGENERATE FILES BELOW
# source("~/Desktop/SaniPath/SPT/1_SPT_Dupid_Fix.R")

# # Put files here if you want to read from csv instead of re-running SPT-dupid_fix.R
path <- "/Users/caseysiesel/Desktop/SaniPath/"
raw_2.1 <- read_csv(paste0(path, "SPT/data/raw_2.1_2020-05-11.csv"))
raw_2.2 <- read_csv(paste0(path, "SPT/data/raw_2.2_2020-05-11.csv"))
raw_2.4 <- read_csv(paste0(path, "SPT/data/raw_2.4_2020-05-11.csv"))
raw_3.1 <- read_csv(paste0(path, "SPT/data/raw_3.1_2020-05-11.csv"))
raw_3.2 <- read_csv(paste0(path, "SPT/data/raw_3.2_2020-05-11.csv"))
raw_3.3 <- read_csv(paste0(path, "SPT/data/raw_3.3_2020-05-11.csv"))
raw_3.4 <- read_csv(paste0(path, "SPT/data/raw_3.4_2020-05-11.csv"))
raw_3.5 <- read_csv(paste0(path, "SPT/data/raw_3.5_2020-05-11.csv"))


#### Little fixes ####
  #  - making everything uppercase, replacing spaces, small known issues
  #  - also doing special fixes for trigger numbers for PLc and
  #  - making FB####UP to FB so I can match with raw sample for enrichment
raw_2.1$col_id <- toupper(sub(" ", "", raw_2.1$col_id))
# raw_2.2$col_id <- toupper(sub(" ", "", raw_2.2$col_id))
raw_2.4$col_id <- toupper(sub(" ", "", raw_2.4$col_id))
raw_2.4$col_sw_trigger_nr <- toupper(gsub(" ' ' | & | ,", "", raw_2.4$col_sw_trigger_nr))
raw_2.4$col_sw_trigger_nr <- str_split(raw_2.4$col_sw_trigger_nr, "\\,")
raw_3.1$lab_id <- toupper(sub(" ", "", raw_3.1$lab_id))
raw_3.2$lab_id <- toupper(sub(" ", "", raw_3.2$lab_id))
raw_3.3$lab_id <- toupper(sub(" ", "", raw_3.3$lab_id))
raw_3.4$lab_id <- toupper(sub(" ", "", raw_3.4$lab_id))
raw_3.5$lab_id <- toupper(sub(" ", "", raw_3.5$lab_id))
raw_3.5$temp_id <- str_remove_all(raw_3.5$lab_id, "UP$")

#Fixing known issues
raw_3.1$lab_sample_type[which(raw_3.1$lab_id=="LS1001")] = 7
raw_3.2$lab_sample_type[which(raw_3.2$lab_id=="SF2001")] = 10
raw_3.4$lab_neighborhood[which(raw_3.4$lab_id=="DWA2003")] = 2
raw_3.4$lab_neighborhood[which(raw_3.4$lab_id=="DWA1003UF")] = 3

#### SPT initial run ####
spt <- raw_2.1 %>%
  mutate(form=2.1,
         date=as.character(as.Date(col_start)),
         sample_index=`_index`, sample_type=col_sample_type, sample_ward=col_ward, sample_hood=col_neighborhood,
         mf_date="", mf_index="", mf_type="", mf_ward="", mf_hood="",
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up", "ncec", "ncec_ec", "upnc", "upnc_tb"))

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
  spt$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt$fb[[i]])]),
                           NA,
                         str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt$fb[[i]])], sep=", ", collapse=", "))
  
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
  spt$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])]) | 
                             anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])]) |
                             isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])], ignore.case=TRUE)),
                           NA,
                           "Turbid")
  
  if (i==1) message("SPT Initial")
  progress(value=(((i-1)/nrow(spt))*100), progress.bar=TRUE)
  if (i==nrow(spt)) message("DONE!")
}

#### Fixing initial mismatched SPT data ####
#    (sample type, date, ward, hood)

#This shows which samples were collected on a day that differs from when MF was performed
spt$col_id[which(spt$date != spt$mf_date)]

#This shows which samples have different sample type codes for sampling vs analysis
spt$col_id[which(spt$sample_type != spt$mf_type | 
                   spt$sample_type !=spt$mst_type | 
                   spt$sample_type !=spt$dna_type | 
                   spt$sample_type !=spt$pcr_type | 
                   spt$sample_type !=spt$enr_type)]

#This shows which samples have different wards for sampling vs analysis
#   * sets WARD to sample_ward when different
mf_diff <- as.numeric(spt$mf_index[which(spt$sample_ward != spt$mf_ward)])
sample_ward_mf <- as.numeric(spt$sample_ward[which(as.numeric(spt$mf_index) %in% mf_diff)])
raw_3.1$lab_ward[which(raw_3.1$`_index` %in% mf_diff)] <- sample_ward_mf

mst_diff <- as.numeric(spt$mst_index[which(spt$sample_ward != spt$mst_ward)])
sample_ward_mst <- as.numeric(spt$sample_ward[which(as.numeric(spt$mst_index) %in% mst_diff)])
raw_3.2$lab_ward[which(raw_3.2$`_index` %in% mst_diff)] <- sample_ward_mst

dna_diff <- as.numeric(spt$dna_index[which(spt$sample_ward != spt$dna_ward)])
sample_ward_dna <- as.numeric(spt$sample_ward[which(as.numeric(spt$dna_index) %in% dna_diff)])
raw_3.3$lab_ward[which(raw_3.3$`_index` %in% dna_diff)] <- sample_ward_dna

pcr_diff <- as.numeric(spt$pcr_index[which(spt$sample_ward != spt$pcr_ward)])
sample_ward_pcr <- as.numeric(spt$sample_ward[which(as.numeric(spt$pcr_index) %in% pcr_diff)])
raw_3.4$lab_ward[which(raw_3.4$`_index` %in% pcr_diff)] <- sample_ward_pcr

enr_diff <- as.numeric(spt$enr_index[which(spt$sample_ward != spt$enr_ward)])
sample_ward_enr <- as.numeric(spt$sample_ward[which(as.numeric(spt$enr_index) %in% enr_diff)])
raw_3.5$lab_ward[which(raw_3.5$`_index` %in% enr_diff)] <- sample_ward_enr

#This shows which samples have different hoods for sampling vs analysis
mf_diff <- as.numeric(spt$mf_index[which(spt$sample_hood != spt$mf_hood)])
sample_hood_mf <- as.numeric(spt$sample_hood[which(as.numeric(spt$mf_index) %in% mf_diff)])
raw_3.1$lab_neighborhood[which(raw_3.1$`_index` %in% mf_diff)] <- sample_hood_mf

mst_diff <- as.numeric(spt$mst_index[which(spt$sample_hood != spt$mst_hood)])
sample_hood_mst <- as.numeric(spt$sample_hood[which(as.numeric(spt$mst_index) %in% mst_diff)])
raw_3.2$lab_neighborhood[which(raw_3.2$`_index` %in% mst_diff)] <- sample_hood_mst

dna_diff <- as.numeric(spt$dna_index[which(spt$sample_hood != spt$dna_hood)])
sample_hood_dna <- as.numeric(spt$sample_hood[which(as.numeric(spt$dna_index) %in% dna_diff)])
raw_3.3$lab_neighborhood[which(raw_3.3$`_index` %in% dna_diff)] <- sample_hood_dna

pcr_diff <- as.numeric(spt$pcr_index[which(spt$sample_hood != spt$pcr_hood)])
sample_hood_pcr <- as.numeric(spt$sample_hood[which(as.numeric(spt$pcr_index) %in% pcr_diff)])
raw_3.4$lab_neighborhood[which(raw_3.4$`_index` %in% pcr_diff)] <- sample_hood_pcr

enr_diff <- as.numeric(spt$enr_index[which(spt$sample_hood != spt$enr_hood)])
sample_hood_enr <- as.numeric(spt$sample_hood[which(as.numeric(spt$enr_index) %in% enr_diff)])
raw_3.5$lab_neighborhood[which(raw_3.5$`_index` %in% enr_diff)] <- sample_hood_enr

#### Re-Running SPT developer to get the final version ####
spt <- raw_2.1 %>%
  mutate(form=2.1,
         date=as.character(as.Date(col_start)),
         sample_index=`_index`, sample_type=col_sample_type, sample_ward=col_ward, sample_hood=col_neighborhood,
         mf_date="", mf_index="", mf_type="", mf_ward="", mf_hood="",
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up", "ncec", "ncec_ec", "upnc", "upnc_tb"))


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
  spt$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt$fb[[i]])]),
                         NA,
                         str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt$fb[[i]])], sep=", ", collapse=", "))
  
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
  spt$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])]) | 
                             anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])]) |
                             isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt$upnc[[i]])], ignore.case=TRUE)),
                           NA,
                           "Turbid")
  
  if (i==1) message("SPT Final")
  progress(value=(((i-1)/nrow(spt))*100), progress.bar=TRUE)
  if (i==nrow(spt)) message("DONE!")
}

#This shows which samples were collected on a day that differs from when MF was performed
spt$col_id[which(spt$date != spt$mf_date)]
#This shows which samples have different sample type codes for sampling vs analysis
spt$col_id[which(spt$sample_type != spt$mf_type | 
                   spt$sample_type !=spt$mst_type | 
                   spt$sample_type !=spt$dna_type | 
                   spt$sample_type !=spt$pcr_type | 
                   spt$sample_type !=spt$enr_type)]
spt$col_id[which(spt$sample_ward != spt$mf_ward | 
                   spt$sample_ward !=spt$mst_ward | 
                   spt$sample_ward !=spt$dna_ward | 
                   spt$sample_ward !=spt$pcr_ward | 
                   spt$sample_ward !=spt$enr_ward)]
spt$col_id[which(spt$sample_hood != spt$mf_hood | 
                   spt$sample_hood !=spt$mst_hood | 
                   spt$sample_hood !=spt$dna_hood | 
                   spt$sample_hood !=spt$pcr_hood | 
                   spt$sample_hood !=spt$enr_hood)]

#### ES initial run ####
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
         fb="", fb_ec="", fb_up="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up", "ncec", "ncec_ec", "upnc", "upnc_tb"))

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
  es$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es$fb[[i]])]),
                         NA,
                         str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es$fb[[i]])], sep=", ", collapse=", "))
  
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
  es$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])]) | 
                             anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])]) |
                             isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])], ignore.case=TRUE)),
                           NA,
                           "Turbid")
  
  if (i==1) message("ES Initial")
  progress(value=(((i-1)/nrow(es))*100), progress.bar=TRUE)
  if (i==nrow(es)) message("DONE!")
}

#### Fixing initial mismatched ES data ####
#    (sample type, date, ward, hood)

#This shows which samples were collected on a day that differs from when MF was performed
es$col_id[which(es$date != es$mf_date)]
#This shows which samples have different sample type codes for sampling vs analysis
es$col_id[which(es$sample_type != es$mf_type | 
                   es$sample_type !=es$mst_type | 
                   es$sample_type !=es$dna_type | 
                   es$sample_type !=es$pcr_type | 
                   es$sample_type !=es$enr_type)]

#The issue seems to be with PL and UF samples
      #  PL sample type should be 11 (lab puts it as 7 for some reason which is for latrine swabs...)
      mf_diff <- as.numeric(es$mf_index[which(grepl("^PL", es$col_id)==TRUE & grepl("^PLC", es$col_id)==FALSE)])
      raw_3.1$lab_sample_type[which(raw_3.1$`_index` %in% mf_diff)] <- 11
      mst_diff <- as.numeric(es$mst_index[which(grepl("^PL", es$col_id)==TRUE & grepl("^PLC", es$col_id)==FALSE)])
      raw_3.2$lab_sample_type[which(raw_3.2$`_index` %in% mst_diff)] <- 11
      dna_diff <- as.numeric(es$dna_index[which(grepl("^PL", es$col_id)==TRUE & grepl("^PLC", es$col_id)==FALSE)])
      raw_3.3$lab_sample_type[which(raw_3.3$`_index` %in% dna_diff)] <- 11
      pcr_diff <- as.numeric(es$pcr_index[which(grepl("^PL", es$col_id)==TRUE & grepl("^PLC", es$col_id)==FALSE)])
      raw_3.4$lab_sample_type[which(raw_3.4$`_index` %in% pcr_diff)] <- 11
      enr_diff <- as.numeric(es$enr_index[which(grepl("^PL", es$col_id)==TRUE & grepl("^PLC", es$col_id)==FALSE)])
      raw_3.5$lab_sample_type[which(raw_3.5$`_index` %in% enr_diff)] <- 11

      #  UF sample type should be 16 (sample team puts it as 11 for some reason which is pooled latrine sewage)
      sample_diff <- as.numeric(es$sample_index[which(grepl("^UF", es$col_id)==TRUE)])
      raw_2.4$col_sample_type[which(raw_2.4$`_index` %in% sample_diff)] <- 16
      
      
      #  a couple special cases that I noticed
      raw_3.5$lab_sample_type[which(raw_3.5$lab_id=="MO1098")] <- 12
      raw_3.3$lab_sample_type[which(raw_3.3$lab_id=="FB1066")] <- 99
      raw_3.3$lab_sample_type[which(raw_3.3$lab_id=="PLC1040")] <- 11
      raw_3.3$lab_sample_type[which(raw_3.3$lab_id=="MO1105")] <- 12
      raw_3.4$lab_sample_type[which(raw_3.4$lab_id=="MO1111")] <- 12
      raw_3.4$lab_sample_type[which(raw_3.4$lab_id=="MO1172")] <- 12


#This shows which samples have different wards for sampling vs analysis
es$col_id[which(es$sample_ward != es$mf_ward | 
                   es$sample_ward !=es$mst_ward | 
                   es$sample_ward !=es$dna_ward | 
                   es$sample_ward !=es$pcr_ward | 
                   es$sample_ward !=es$enr_ward)]

#This shows which samples have different wards for sampling vs analysis
#   * sets WARD to sample_ward when different
for(i in 1:nrow(es)){
  if(is.na(es$mf_ward[i])==FALSE & es$sample_ward[i] != es$mf_ward[i]){
    ifelse(is.na(es$mf_ward[i]), NA, raw_3.1$lab_ward[which(raw_3.1$`_index` == es$mf_index[i])]<-es$sample_ward[i])
  }
  if(is.na(es$mst_ward[i])==FALSE & es$sample_ward[i] != es$mst_ward[i]){
    ifelse(is.na(es$mst_ward[i]), NA, raw_3.2$lab_ward[which(raw_3.2$`_index` == es$mst_index[i])]<-es$sample_ward[i])
  }
  if(is.na(es$dna_ward[i])==FALSE & es$sample_ward[i] != es$dna_ward[i]){
    ifelse(is.na(es$dna_ward[i]), NA, raw_3.3$lab_ward[which(raw_3.3$`_index` == es$dna_index[i])]<-es$sample_ward[i])
  }
  if(is.na(es$pcr_ward[i])==FALSE & es$sample_ward[i] != es$pcr_ward[i]){
    ifelse(is.na(es$pcr_ward[i]), NA, raw_3.4$lab_ward[which(raw_3.4$`_index` == es$pcr_index[i])]<-es$sample_ward[i])
  }
  if(is.na(es$enr_ward[i])==FALSE & es$sample_ward[i] != es$enr_ward[i]){
    ifelse(is.na(es$enr_ward[i]), NA, raw_3.5$lab_ward[which(raw_3.5$`_index` == es$enr_index[i])]<-es$sample_ward[i])
  }
}


#### Re-Running ES developer to get the final version ####
es <- raw_2.4 %>%
  mutate(form=2.4,
         date=ifelse(col_sample_type !=12, as.character(as.Date(col_start)), as.character(as.Date(col_ms_in))),
         sample_index=`_index`, sample_type=col_sample_type, sample_ward=col_ward, sample_hood=NA,
         mf_date="", mf_index="", mf_type="", mf_ward="", mf_hood="",
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up", "ncec", "ncec_ec", "upnc", "upnc_tb"))

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
  es$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es$fb[[i]])]),
                        NA,
                        str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es$fb[[i]])], sep=", ", collapse=", "))
  
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
  es$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])]) | 
                            anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])]) |
                            isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es$upnc[[i]])], ignore.case=TRUE)),
                          NA,
                          "Turbid")
  
  if (i==1) message("ES Final")
  progress(value=(((i-1)/nrow(es))*100), progress.bar=TRUE)
  if (i==nrow(es)) message("DONE!")
}

#This shows which samples were collected on a day that differs from when MF was performed
es$col_id[which(es$date != es$mf_date)]
#This shows which samples have different sample type codes for sampling vs analysis
es$col_id[which(es$sample_type != es$mf_type | 
                  es$sample_type !=es$mst_type | 
                  es$sample_type !=es$dna_type | 
                  es$sample_type !=es$pcr_type | 
                  es$sample_type !=es$enr_type)]



#### Printing out any remaining issues ####
# SPT
message(paste0("SPT MF Date Issues: ", paste0(spt$col_id[which(spt$date != spt$mf_date)], collapse=", ")))
message(paste0("SPT Sample Type Issues: ", paste0(spt$col_id[which(spt$sample_type != spt$mf_type |
                                                         spt$sample_type !=spt$mst_type |
                                                         spt$sample_type !=spt$dna_type |
                                                         spt$sample_type !=spt$pcr_type |
                                                         spt$sample_type !=spt$enr_type)], collapse=", ")))


message(paste0("SPT Ward Issues: ", paste0(spt$col_id[which(spt$sample_ward != spt$mf_ward |
                   spt$sample_ward !=spt$mst_ward |
                   spt$sample_ward !=spt$dna_ward |
                   spt$sample_ward !=spt$pcr_ward |
                   spt$sample_ward !=spt$enr_ward)], collapse=", ")))
message(paste0("SPT Neighborhood Issues: ", paste0(spt$col_id[which(spt$sample_hood != spt$mf_hood |
                   spt$sample_hood !=spt$mst_hood |
                   spt$sample_hood !=spt$dna_hood |
                   spt$sample_hood !=spt$pcr_hood |
                   spt$sample_hood !=spt$enr_hood)], collapse=", ")))

# ES
message(paste0("ES MF Date Issues: ", paste0(es$col_id[which(es$date != es$mf_date)], collapse =", ")))
message(paste0("ES Sample Type Issues: ",
               paste0(es$col_id[which(
                 es$sample_type != es$mf_type |
                   es$sample_type != es$mst_type |
                   es$sample_type != es$dna_type |
                   es$sample_type != es$pcr_type |
                   es$sample_type != es$enr_type
               )], collapse = ", ")))
message(paste0("ES Ward Issues: ",
               paste0(es$col_id[which(
                 es$sample_ward != es$mf_ward |
                   es$sample_ward !=
                   es$mst_ward |
                   es$sample_ward !=
                   es$dna_ward |
                   es$sample_ward !=
                   es$pcr_ward |
                   es$sample_ward !=
                   es$enr_ward
               )], collapse = ", ")))
message(paste0("ES Neighborhood Issues: ",
               paste0(es$col_id[which(
                 es$sample_hood != es$mf_hood |
                   es$sample_hood !=
                   es$mst_hood |
                   es$sample_hood !=
                   es$dna_hood |
                   es$sample_hood !=
                   es$pcr_hood |
                   es$sample_hood !=
                   es$enr_hood
               )], collapse = ", ")))

