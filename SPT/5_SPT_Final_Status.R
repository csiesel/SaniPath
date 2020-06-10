path <- "/Users/caseysiesel/Desktop/SaniPath/"

source(paste0(path, "SPT/4c_SPT_PCR.R"))

'%!in%' <- function(x,y)!('%in%'(x,y))

fb_sample_match <- raw_3.1 %>% filter(lab_sample_type==99) %>% 
  mutate(fb_id = lab_id, other_id = toupper(str_extract(lab_notes, "[:alpha:]+\\d{4}"))) %>%
  select(c("fb_id", "other_id"))


#### SPT Status semi-final ####
spt1 <- ec_data_spt %>%
  mutate(col_id = sampleid,
         form=2.1,
         date=ifelse(!is.na(as.character(as.Date(col_start))), as.character(as.Date(col_start)), as.character(as.Date(col_start_dt))),
         sample_index=`_index.x`,
         lat=`_col_location_latitude`, long = `_col_location_longitude`,
         sample_type=sample_type,
         sample_ward=col_ward, 
         sample_hood=col_neighborhood,
         mf_date=as.character(as.Date(lab_processing)),
         mf_index=`_index.y`,
         mf_type=sample_type, 
         mf_ward=lab_ward, 
         mf_hood=lab_neighborhood,
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", 
         fb_pcr="", fb_typhi_pos="", fb_typhi_pres="", fb_paratyphi_pos="", fb_paratyphi_pres="", 
         ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "lat", "long", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up",
           "fb_pcr", "fb_typhi_pos", "fb_typhi_pres", "fb_paratyphi_pos", "fb_paratyphi_pres",
           "ncec", "ncec_ec", "upnc", "upnc_tb"))

#The function below for spt1 and the same for ES & spt1 is duplicating Wolfgang's status page
# Essentially pulls in a date if the ID has a match in the lab forms
for(i in 1:nrow(spt1)){
  #MF is covered above
  
  
  #MST
  spt1$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(spt1$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             as.character(as.Date(raw_3.2$lab_processing[which(spt1$col_id[i]==raw_3.2$lab_id)])))
  spt1$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(spt1$col_id[i]==raw_3.2$lab_id)]),
                              NA, 
                              raw_3.2$`_index`[which(spt1$col_id[i]==raw_3.2$lab_id)])
  spt1$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(spt1$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             raw_3.2$lab_sample_type[which(spt1$col_id[i]==raw_3.2$lab_id)])
  spt1$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(spt1$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             raw_3.2$lab_ward[which(spt1$col_id[i]==raw_3.2$lab_id)])
  spt1$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(spt1$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             raw_3.2$lab_neighborhood[which(spt1$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  spt1$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(spt1$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             as.character(as.Date(raw_3.3$lab_dna_date[which(spt1$col_id[i]==raw_3.3$lab_id)])))
  spt1$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(spt1$col_id[i]==raw_3.3$lab_id)]),
                              NA, 
                              raw_3.3$`_index`[which(spt1$col_id[i]==raw_3.3$lab_id)])
  spt1$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(spt1$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$lab_sample_type[which(spt1$col_id[i]==raw_3.3$lab_id)])
  spt1$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(spt1$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$lab_ward[which(spt1$col_id[i]==raw_3.3$lab_id)])
  spt1$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(spt1$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$lab_neighborhood[which(spt1$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  spt1$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(spt1$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(spt1$col_id[i]==raw_3.4$lab_id)])))
  spt1$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(spt1$col_id[i]==raw_3.4$lab_id)]),
                              NA, 
                              raw_3.4$`_index`[which(spt1$col_id[i]==raw_3.4$lab_id)])
  spt1$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(spt1$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$lab_sample_type[which(spt1$col_id[i]==raw_3.4$lab_id)])
  spt1$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(spt1$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$lab_ward[which(spt1$col_id[i]==raw_3.4$lab_id)])
  spt1$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(spt1$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$lab_neighborhood[which(spt1$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  spt1$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(spt1$col_id[i]==raw_3.5$temp_id)]),
                             NA, 
                             as.character(as.Date(raw_3.5$lab_processing[which(spt1$col_id[i]==raw_3.5$temp_id)])))
  spt1$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(spt1$col_id[i]==raw_3.5$lab_id)]),
                              NA, 
                              raw_3.5$`_index`[which(spt1$col_id[i]==raw_3.5$lab_id)])
  spt1$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(spt1$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$lab_sample_type[which(spt1$col_id[i]==raw_3.5$lab_id)])
  spt1$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(spt1$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$lab_ward[which(spt1$col_id[i]==raw_3.5$lab_id)])
  spt1$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(spt1$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$lab_neighborhood[which(spt1$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  spt1$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(spt1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                       grepl("FB", raw_2.1$col_id)==TRUE)]),
                       NA,
                       strsplit(str_c(raw_2.1$col_id[which(spt1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                             grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  
  spt1$fb[i] <- ifelse(is_empty(fb_sample_match$fb_id[which(spt1$col_id[i]==fb_sample_match$other_id)]),
                       spt1$fb[i],
                       fb_sample_match$fb_id[which(spt1$col_id[i]==fb_sample_match$other_id)])
  
  
  spt1$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$fb[[i]])]),
                          NA,
                          str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$fb[[i]])], sep=", ", collapse=", "))
  spt1$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt1$fb[[i]])]),
                          NA,
                          str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt1$fb[[i]])], sep=", ", collapse=", "))
  
  spt1$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                         grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                         NA,
                         str_c(raw_3.1$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                      grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  spt1$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$ncec[[i]])]),
                            NA,
                            str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$ncec[[i]])], sep=", ", collapse=", "))
  
  spt1$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                         grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                         NA,
                         str_c(raw_3.5$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                      grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  spt1$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])]) | 
                              anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])]) |
                              isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])], ignore.case=TRUE)),
                            NA,
                            "Turbid")
  
  if (i==1) message("spt1 Initial")
  progress(value=(((i-1)/nrow(spt1))*100), progress.bar=TRUE)
  if (i==nrow(spt1)) message("DONE!")
}

#Manual cleanup of some issues
spt1$col_id[which(is.na(spt1$lat))]
spt1[which(spt1$col_id=="RP2002B"), 3:9] <- spt1[which(spt1$col_id=="RP2002A"), 3:9]
spt1[which(spt1$col_id=="SF1012(A)"), 3:9] <- spt1[which(spt1$col_id=="SF1012"), 3:9]
spt1[which(spt1$col_id=="SF1012(B)"), 3:9] <- spt1[which(spt1$col_id=="SF1012"), 3:9]
spt1[which(spt1$col_id=="SF1014A"), 3:9] <- spt1[which(spt1$col_id=="SF1014"), 3:9]
spt1[which(spt1$col_id=="SF1014B"), 3:9] <- spt1[which(spt1$col_id=="SF1014"), 3:9]
spt1$col_id[which(is.na(spt1$lat))]

# Fixing missing mf (no data for "OD1006","RP1010","RP2005","RP2006","DW2002","DWA1001(UF)",
# "DWA1002(UF)","DWA1003UF","DWA2001OLD","DWA2002","DWA2003","DWB2001","SW2004","LS2002","LS2003","SL2005"     
# "BW2004","SF1008","SF1012","SF1014","SF2004")
spt1[which(is.na(spt1$mf_index) & spt1$sample_type !=99), c("col_id", "date")]

# Fixing missing MST data
# "RP1011","RP1012","DWA1001(UF)","DWA1002(UF)","DWA1003UF","DWA2001OLD","DWA2002",   
# "DWA2002OLD","DWA2003","DWB2001","SW1007","SW1009","SW2004","FW1002","LS1010"     
# "LS1011","LS1012","LS2002","LS2003","SL1011","SL1012","BW2004","SF1011",
# "SF1012","SF1012(A)","SF1012(B)","SF1013","SF1014","SF1014A","SF1014B","SF2002","SF2004"
spt1[which(is.na(spt1$mst_index) & spt1$sample_type !=99), c("col_id", "date")]

spt1 <- spt1 %>% filter(col_id %!in% c("DWA1001(UF)", "DWA1002(UF)", "DWA2001OLD","SF1012"))
spt1[which(spt1$col_id=="DWA1004"), 15:19] <- spt1[which(spt1$col_id=="DWA1004UF"), 15:19]

#Fixing missing DNA data
spt1[which(is.na(spt1$dna_index) & spt1$sample_type !=99), c("col_id", "date")]
spt1[which(is.na(spt1$pcr_index) & spt1$sample_type !=99), c("col_id", "date")]
spt1[which(is.na(spt1$enr_index) & spt1$sample_type !=99), c("col_id", "date")]


for(i in 1:nrow(spt1)){
  #FB
  spt1$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(spt1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                       grepl("FB", raw_2.1$col_id)==TRUE)]),
                       NA,
                       strsplit(str_c(raw_2.1$col_id[which(spt1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                             grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  
  spt1$fb[i] <- ifelse(is_empty(fb_sample_match$fb_id[which(spt1$col_id[i]==fb_sample_match$other_id)]),
                       spt1$fb[i],
                       fb_sample_match$fb_id[which(spt1$col_id[i]==fb_sample_match$other_id)])
  
  
  spt1$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$fb[[i]])]),
                          NA,
                          str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$fb[[i]])], sep=", ", collapse=", "))
  spt1$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt1$fb[[i]])]),
                          NA,
                          str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% spt1$fb[[i]])], sep=", ", collapse=", "))
  
  spt1$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                         grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                         NA,
                         str_c(raw_3.1$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                      grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  spt1$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$ncec[[i]])]),
                            NA,
                            str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% spt1$ncec[[i]])], sep=", ", collapse=", "))
  
  spt1$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                         grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                         NA,
                         str_c(raw_3.5$lab_id[which(spt1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                      grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  spt1$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])]) | 
                              anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])]) |
                              isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% spt1$upnc[[i]])], ignore.case=TRUE)),
                            NA,
                            "Turbid")
  
  if (i==1) message("SPT1 FINAL")
  progress(value=(((i-1)/nrow(spt1))*100), progress.bar=TRUE)
  if (i==nrow(spt1)) message("DONE!")
}


#### ES Status semi-final ####
es1 <- ec_data_es %>%
  mutate(col_id = sampleid,
         form=2.4,
         date=ifelse(sample_type ==12, as.character(as.Date(col_ms_in)), ifelse(!is.na(col_start), as.character(as.Date(col_start)), as.character(as.Date(col_start_dt)))),
         lat = `_col_location_latitude`, long = `_col_location_longitude`,
         sample_index=`_index.x`,
         lat=`_col_location_latitude`, long = `_col_location_longitude`,
         sample_type=sample_type,
         sample_ward=col_ward, 
         sample_hood=NA,
         mf_date=as.character(as.Date(lab_processing)),
         mf_index=`_index.y`,
         mf_type=sample_type, 
         mf_ward=lab_ward, 
         mf_hood=lab_neighborhood,
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", 
         fb_pcr="", fb_typhi_pos="", fb_typhi_pres="", fb_paratyphi_pos="", fb_paratyphi_pres="", 
         ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "form", "date", "lat", "long", "sample_index", "sample_type", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up",
           "fb_pcr", "fb_typhi_pos", "fb_typhi_pres", "fb_paratyphi_pos", "fb_paratyphi_pres",
           "ncec", "ncec_ec", "upnc", "upnc_tb"))

for(i in 1:nrow(es1)){
  #MF is covered above
  
  
  #MST
  es1$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(es1$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.2$lab_processing[which(es1$col_id[i]==raw_3.2$lab_id)])))
  es1$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(es1$col_id[i]==raw_3.2$lab_id)]),
                             NA, 
                             raw_3.2$`_index`[which(es1$col_id[i]==raw_3.2$lab_id)])
  es1$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(es1$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_sample_type[which(es1$col_id[i]==raw_3.2$lab_id)])
  es1$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(es1$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_ward[which(es1$col_id[i]==raw_3.2$lab_id)])
  es1$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(es1$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$lab_neighborhood[which(es1$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  es1$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(es1$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.3$lab_dna_date[which(es1$col_id[i]==raw_3.3$lab_id)])))
  es1$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(es1$col_id[i]==raw_3.3$lab_id)]),
                             NA, 
                             raw_3.3$`_index`[which(es1$col_id[i]==raw_3.3$lab_id)])
  es1$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(es1$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_sample_type[which(es1$col_id[i]==raw_3.3$lab_id)])
  es1$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(es1$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_ward[which(es1$col_id[i]==raw_3.3$lab_id)])
  es1$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(es1$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$lab_neighborhood[which(es1$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  es1$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(es1$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(es1$col_id[i]==raw_3.4$lab_id)])))
  es1$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(es1$col_id[i]==raw_3.4$lab_id)]),
                             NA, 
                             raw_3.4$`_index`[which(es1$col_id[i]==raw_3.4$lab_id)])
  es1$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(es1$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_sample_type[which(es1$col_id[i]==raw_3.4$lab_id)])
  es1$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(es1$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_ward[which(es1$col_id[i]==raw_3.4$lab_id)])
  es1$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(es1$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$lab_neighborhood[which(es1$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  es1$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(es1$col_id[i]==raw_3.5$temp_id)]),
                            NA, 
                            as.character(as.Date(raw_3.5$lab_processing[which(es1$col_id[i]==raw_3.5$temp_id)])))
  es1$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(es1$col_id[i]==raw_3.5$lab_id)]),
                             NA, 
                             raw_3.5$`_index`[which(es1$col_id[i]==raw_3.5$lab_id)])
  es1$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(es1$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_sample_type[which(es1$col_id[i]==raw_3.5$lab_id)])
  es1$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(es1$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_ward[which(es1$col_id[i]==raw_3.5$lab_id)])
  es1$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(es1$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$lab_neighborhood[which(es1$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  es1$fb[i] <- ifelse(is_empty(raw_2.4$col_id[which(es1$date[i]==as.character(as.Date(raw_2.4$col_start_dt)) & 
                                                      grepl("FB", raw_2.4$col_id)==TRUE)]),
                      NA,
                      strsplit(str_c(raw_2.4$col_id[which(es1$date[i]==as.character(as.Date(raw_2.4$col_start_dt)) & 
                                                            grepl("FB", raw_2.4$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  es1$fb[i] <- ifelse(is_empty(fb_sample_match$fb_id[which(es1$col_id[i]==fb_sample_match$other_id)]),
                       es1$fb[i],
                       fb_sample_match$fb_id[which(es1$col_id[i]==fb_sample_match$other_id)])

  
  es1$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$fb[[i]])]),
                         NA,
                         str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$fb[[i]])], sep=", ", collapse=", "))
  es1$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es1$fb[[i]])]),
                         NA,
                         str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es1$fb[[i]])], sep=", ", collapse=", "))
  
  es1$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                        grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                        NA,
                        str_c(raw_3.1$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                     grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  es1$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$ncec[[i]])]),
                           NA,
                           str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$ncec[[i]])], sep=", ", collapse=", "))
  
  es1$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                        grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                        NA,
                        str_c(raw_3.5$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                     grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  es1$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])]) | 
                             anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])]) |
                             isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])], ignore.case=TRUE)),
                           NA,
                           "Turbid")
  
  if (i==1) message("es1 Initial")
  progress(value=(((i-1)/nrow(es1))*100), progress.bar=TRUE)
  if (i==nrow(es1)) message("DONE!")
}

#Manual cleanup of some issues
es1[which(es1$col_id=="PLC1111"), c(10:14,20:24)] <- es1[which(es1$col_id=="PLC111"), c(10:14,20:24)]
es1 <- es1 %>% filter(col_id !="PLC111")

#Manual cleanup of some issues
es1$col_id[which(is.na(es1$lat))]

es1[which(is.na(es1$mf_index) & es1$sample_type %!in% c(99, 12)), c("col_id", "date")]

es1[which(is.na(es1$mst_index) & es1$sample_type %!in% c(99, 12)), c("col_id", "date")]

es1[which(is.na(es1$dna_index) & es1$sample_type !=99), c("col_id", "date")]

es1[which(is.na(es1$pcr_index) & es1$sample_type !=99), c("col_id", "date")]

es1[which(is.na(es1$enr_index) & es1$sample_type !=99), c("col_id", "date")]

# 
# for(i in 1:nrow(es1)){
#   #FB
#   es1$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(es1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
#                                                       grepl("FB", raw_2.1$col_id)==TRUE)]),
#                       NA,
#                       strsplit(str_c(raw_2.1$col_id[which(es1$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
#                                                             grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
#   
#   
#   
#   
#   
#   es1$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$fb[[i]])]),
#                          NA,
#                          str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$fb[[i]])], sep=", ", collapse=", "))
#   es1$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es1$fb[[i]])]),
#                          NA,
#                          str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% es1$fb[[i]])], sep=", ", collapse=", "))
#   
#   es1$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
#                                                         grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
#                         NA,
#                         str_c(raw_3.1$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
#                                                      grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
#   es1$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$ncec[[i]])]),
#                            NA,
#                            str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% es1$ncec[[i]])], sep=", ", collapse=", "))
#   
#   es1$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
#                                                         grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
#                         NA,
#                         str_c(raw_3.5$lab_id[which(es1$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
#                                                      grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
#   es1$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])]) | 
#                              anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])]) |
#                              isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% es1$upnc[[i]])], ignore.case=TRUE)),
#                            NA,
#                            "Turbid")
#   
#   if (i==1) message("es1 Initial")
#   progress(value=(((i-1)/nrow(es1))*100), progress.bar=TRUE)
#   if (i==nrow(es1)) message("DONE!")
# }



#### SO ####
so <- ec_data_so %>%
  mutate(col_id = sampleid, col_so_id = col_id,
    form=2.2, date=as.character(as.Date(col_start_dt)), sample_index=`_index`, 
    lat=`_col_location_latitude`, long = `_col_location_longitude`,
    sample_type=lab_sample_type,
    sample_type_so=ifelse(is.na(lab_sample_type_fp), lab_sample_type_co, lab_sample_type_fp),
    sample_ward=col_ward, 
    sample_hood=col_neighborhood,
    mf_date=as.character(as.Date(lab_processing)),
    mf_index=`_index.y`,
    mf_type=lab_sample_type, 
    mf_ward=lab_ward, 
    mf_hood=lab_neighborhood,
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
    fb="", fb_ec="", fb_up="", 
    fb_pcr="", fb_typhi_pos="", fb_typhi_pres="", fb_paratyphi_pos="", fb_paratyphi_pres="", 
    ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "col_so_id", "form", "date", "lat", "long", "sample_index", "sample_type", "sample_type_so", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up",
           "fb_pcr", "fb_typhi_pos", "fb_typhi_pres", "fb_paratyphi_pos", "fb_paratyphi_pres",
           "ncec", "ncec_ec", "upnc", "upnc_tb"))

#The function below for SPT and the same for ES & SO is duplicating Wolfgang's status page
# Essentially pulls in a date if the ID has a match in the lab forms
for(i in 1:nrow(so)){
  #MF is covered above
    
  
  #MST
  so$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.2$lab_processing[which(so$col_id[i]==raw_3.2$lab_id)])))
  so$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(so$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$`_index`[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_sample_type[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_ward[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_neighborhood[which(so$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  so$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.3$lab_dna_date[which(so$col_id[i]==raw_3.3$lab_id)])))
  so$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(so$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$`_index`[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_sample_type[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_ward[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_neighborhood[which(so$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  so$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(so$col_id[i]==raw_3.4$lab_id)])))
  so$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(so$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$`_index`[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_sample_type[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_ward[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_neighborhood[which(so$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  so$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(so$col_id[i]==raw_3.5$temp_id)]),
                           NA, 
                           as.character(as.Date(raw_3.5$lab_processing[which(so$col_id[i]==raw_3.5$temp_id)])))
  so$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(so$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$`_index`[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_sample_type[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_ward[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_neighborhood[which(so$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  so$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(so$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                     grepl("FB", raw_2.1$col_id)==TRUE)]),
                     NA,
                     strsplit(str_c(raw_2.1$col_id[which(so$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                           grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  temp_fb <- ifelse(is_empty(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_received)) &
                                                                     grepl("FB", raw_3.1$lab_id)==TRUE)]),
                                     NULL,
                                     strsplit(str_c(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_received)) &
                                                                           grepl("FB", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "), ", "))

  temp_fb2 <- Map(c, so$fb[i], temp_fb)
  un <- unlist(temp_fb2)
  so$fb[i] <- Map(`[`, temp_fb2, relist(!duplicated(un), skeleton = temp_fb2))

  so$fb[i] <- ifelse(is_empty(fb_sample_match$fb_id[which(so$col_id[i]==fb_sample_match$other_id)]),
                       so$fb[i],
                       fb_sample_match$fb_id[which(so$col_id[i]==fb_sample_match$other_id)])

  so$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$fb[[i]])]),
                        NA,
                        str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$fb[[i]])], sep=", ", collapse=", "))
  so$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% so$fb[[i]])]),
                        NA,
                        str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% so$fb[[i]])], sep=", ", collapse=", "))
  
  so$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                       grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                       NA,
                       str_c(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                    grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  so$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$ncec[[i]])]),
                          NA,
                          str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$ncec[[i]])], sep=", ", collapse=", "))
  
  so$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(so$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                       grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                       NA,
                       str_c(raw_3.5$lab_id[which(so$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                    grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  so$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])]) | 
                            anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])]) |
                            isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])], ignore.case=TRUE)),
                          NA,
                          "Turbid")
  
  if (i==1) message("SO Initial")
  progress(value=(((i-1)/nrow(so))*100), progress.bar=TRUE)
  if (i==nrow(so)) message("DONE!")
}


#### Fixing initial mismatched SPT data ####
#    (sample type, date, ward, hood)

#This shows which samples were collected on a day that differs from when MF was performed
so$col_id[which(so$date != so$mf_date)]

#This shows which samples have different sample type codes for sampling vs analysis
so$col_id[which(so$sample_type != so$mf_type | 
                   so$sample_type !=so$mst_type | 
                   so$sample_type !=so$dna_type | 
                   so$sample_type !=so$pcr_type | 
                   so$sample_type !=so$enr_type)]

#This shows which samples have different wards for sampling vs analysis
#   * sets WARD to sample_ward when different
mf_diff <- as.numeric(so$mf_index[which(so$sample_ward != so$mf_ward)])
sample_ward_mf <- as.numeric(so$sample_ward[which(as.numeric(so$mf_index) %in% mf_diff)])
raw_3.1$lab_ward[which(raw_3.1$`_index` %in% mf_diff)] <- sample_ward_mf
ec_data_so$lab_ward[which(ec_data_so$`_index.y` %in% mf_diff)] <- sample_ward_mf

mst_diff <- as.numeric(so$mst_index[which(so$sample_ward != so$mst_ward)])
sample_ward_mst <- as.numeric(so$sample_ward[which(as.numeric(so$mst_index) %in% mst_diff)])
raw_3.2$lab_ward[which(raw_3.2$`_index` %in% mst_diff)] <- sample_ward_mst

dna_diff <- as.numeric(so$dna_index[which(so$sample_ward != so$dna_ward)])
sample_ward_dna <- as.numeric(so$sample_ward[which(as.numeric(so$dna_index) %in% dna_diff)])
raw_3.3$lab_ward[which(raw_3.3$`_index` %in% dna_diff)] <- sample_ward_dna

pcr_diff <- as.numeric(so$pcr_index[which(so$sample_ward != so$pcr_ward)])
sample_ward_pcr <- as.numeric(so$sample_ward[which(as.numeric(so$pcr_index) %in% pcr_diff)])
raw_3.4$lab_ward[which(raw_3.4$`_index` %in% pcr_diff)] <- sample_ward_pcr

enr_diff <- as.numeric(so$enr_index[which(so$sample_ward != so$enr_ward)])
sample_ward_enr <- as.numeric(so$sample_ward[which(as.numeric(so$enr_index) %in% enr_diff)])
raw_3.5$lab_ward[which(raw_3.5$`_index` %in% enr_diff)] <- sample_ward_enr

#This shows which samples have different hoods for sampling vs analysis
mf_diff <- as.numeric(so$mf_index[which(so$sample_hood != so$mf_hood)])
sample_hood_mf <- as.numeric(so$sample_hood[which(as.numeric(so$mf_index) %in% mf_diff)])
raw_3.1$lab_neighborhood[which(raw_3.1$`_index` %in% mf_diff)] <- sample_hood_mf
ec_data_so$lab_neighborhood[which(ec_data_so$`_index.y` %in% mf_diff)] <- sample_hood_mf

mst_diff <- as.numeric(so$mst_index[which(so$sample_hood != so$mst_hood)])
sample_hood_mst <- as.numeric(so$sample_hood[which(as.numeric(so$mst_index) %in% mst_diff)])
raw_3.2$lab_neighborhood[which(raw_3.2$`_index` %in% mst_diff)] <- sample_hood_mst

dna_diff <- as.numeric(so$dna_index[which(so$sample_hood != so$dna_hood)])
sample_hood_dna <- as.numeric(so$sample_hood[which(as.numeric(so$dna_index) %in% dna_diff)])
raw_3.3$lab_neighborhood[which(raw_3.3$`_index` %in% dna_diff)] <- sample_hood_dna

pcr_diff <- as.numeric(so$pcr_index[which(so$sample_hood != so$pcr_hood)])
sample_hood_pcr <- as.numeric(so$sample_hood[which(as.numeric(so$pcr_index) %in% pcr_diff)])
raw_3.4$lab_neighborhood[which(raw_3.4$`_index` %in% pcr_diff)] <- sample_hood_pcr

enr_diff <- as.numeric(so$enr_index[which(so$sample_hood != so$enr_hood)])
sample_hood_enr <- as.numeric(so$sample_hood[which(as.numeric(so$enr_index) %in% enr_diff)])
raw_3.5$lab_neighborhood[which(raw_3.5$`_index` %in% enr_diff)] <- sample_hood_enr





so <- ec_data_so %>%
  mutate(col_id = sampleid, col_so_id = col_id,
         form=2.2, date=as.character(as.Date(col_start_dt)), sample_index=`_index`, 
         lat=`_col_location_latitude`, long = `_col_location_longitude`,
         sample_type=lab_sample_type,
         sample_type_so=ifelse(is.na(lab_sample_type_fp), lab_sample_type_co, lab_sample_type_fp),
         sample_ward=col_ward, 
         sample_hood=col_neighborhood,
         mf_date=as.character(as.Date(lab_processing)),
         mf_index=`_index.y`,
         mf_type=lab_sample_type, 
         mf_ward=lab_ward, 
         mf_hood=lab_neighborhood,
         mst_date="", mst_index="", mst_type="", mst_ward="", mst_hood="",
         dna_date="", dna_index="", dna_type="", dna_ward="", dna_hood="",
         pcr_date="", pcr_index="", pcr_type="", pcr_ward="", pcr_hood="",
         enr_date="", enr_index="", enr_type="", enr_ward="", enr_hood="",
         fb="", fb_ec="", fb_up="", 
         fb_pcr="", fb_typhi_pos="", fb_typhi_pres="", fb_paratyphi_pos="", fb_paratyphi_pres="", 
         ncec="", ncec_ec="", upnc="", upnc_tb="") %>%
  select(c("col_id", "col_so_id", "form", "date", "lat", "long", "sample_index", "sample_type", "sample_type_so", "sample_ward", "sample_hood",
           "mf_date", "mf_index", "mf_type", "mf_ward", "mf_hood",
           "mst_date", "mst_index", "mst_type", "mst_ward", "mst_hood",
           "dna_date", "dna_index", "dna_type", "dna_ward", "dna_hood",
           "pcr_date", "pcr_index", "pcr_type", "pcr_ward", "pcr_hood",
           "enr_date", "enr_index", "enr_type", "enr_ward", "enr_hood",
           "fb", "fb_ec", "fb_up",
           "fb_pcr", "fb_typhi_pos", "fb_typhi_pres", "fb_paratyphi_pos", "fb_paratyphi_pres",
           "ncec", "ncec_ec", "upnc", "upnc_tb"))

#The function below for SPT and the same for ES & SO is duplicating Wolfgang's status page
# Essentially pulls in a date if the ID has a match in the lab forms
for(i in 1:nrow(so)){
  #MF is covered above
  
  
  #MST
  so$mst_date[i] <- ifelse(is_empty(raw_3.2$lab_processing[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.2$lab_processing[which(so$col_id[i]==raw_3.2$lab_id)])))
  so$mst_index[i] <- ifelse(is_empty(raw_3.2$`_index`[which(so$col_id[i]==raw_3.2$lab_id)]),
                            NA, 
                            raw_3.2$`_index`[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_type[i] <- ifelse(is_empty(raw_3.2$lab_sample_type[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_sample_type[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_ward[i] <- ifelse(is_empty(raw_3.2$lab_ward[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_ward[which(so$col_id[i]==raw_3.2$lab_id)])
  so$mst_hood[i] <- ifelse(is_empty(raw_3.2$lab_neighborhood[which(so$col_id[i]==raw_3.2$lab_id)]),
                           NA, 
                           raw_3.2$lab_neighborhood[which(so$col_id[i]==raw_3.2$lab_id)])
  
  #DNA
  so$dna_date[i] <- ifelse(is_empty(raw_3.3$lab_dna_date[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.3$lab_dna_date[which(so$col_id[i]==raw_3.3$lab_id)])))
  so$dna_index[i] <- ifelse(is_empty(raw_3.3$`_index`[which(so$col_id[i]==raw_3.3$lab_id)]),
                            NA, 
                            raw_3.3$`_index`[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_type[i] <- ifelse(is_empty(raw_3.3$lab_sample_type[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_sample_type[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_ward[i] <- ifelse(is_empty(raw_3.3$lab_ward[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_ward[which(so$col_id[i]==raw_3.3$lab_id)])
  so$dna_hood[i] <- ifelse(is_empty(raw_3.3$lab_neighborhood[which(so$col_id[i]==raw_3.3$lab_id)]),
                           NA, 
                           raw_3.3$lab_neighborhood[which(so$col_id[i]==raw_3.3$lab_id)])
  
  #PCR
  so$pcr_date[i] <- ifelse(is_empty(raw_3.4$lab_pcr_ty_date[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           as.character(as.Date(raw_3.4$lab_pcr_ty_date[which(so$col_id[i]==raw_3.4$lab_id)])))
  so$pcr_index[i] <- ifelse(is_empty(raw_3.4$`_index`[which(so$col_id[i]==raw_3.4$lab_id)]),
                            NA, 
                            raw_3.4$`_index`[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_type[i] <- ifelse(is_empty(raw_3.4$lab_sample_type[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_sample_type[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_ward[i] <- ifelse(is_empty(raw_3.4$lab_ward[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_ward[which(so$col_id[i]==raw_3.4$lab_id)])
  so$pcr_hood[i] <- ifelse(is_empty(raw_3.4$lab_neighborhood[which(so$col_id[i]==raw_3.4$lab_id)]),
                           NA, 
                           raw_3.4$lab_neighborhood[which(so$col_id[i]==raw_3.4$lab_id)])
  
  #ENR
  so$enr_date[i] <- ifelse(is_empty(raw_3.5$lab_processing[which(so$col_id[i]==raw_3.5$temp_id)]),
                           NA, 
                           as.character(as.Date(raw_3.5$lab_processing[which(so$col_id[i]==raw_3.5$temp_id)])))
  so$enr_index[i] <- ifelse(is_empty(raw_3.5$`_index`[which(so$col_id[i]==raw_3.5$lab_id)]),
                            NA, 
                            raw_3.5$`_index`[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_type[i] <- ifelse(is_empty(raw_3.5$lab_sample_type[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_sample_type[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_ward[i] <- ifelse(is_empty(raw_3.5$lab_ward[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_ward[which(so$col_id[i]==raw_3.5$lab_id)])
  so$enr_hood[i] <- ifelse(is_empty(raw_3.5$lab_neighborhood[which(so$col_id[i]==raw_3.5$lab_id)]),
                           NA, 
                           raw_3.5$lab_neighborhood[which(so$col_id[i]==raw_3.5$lab_id)])
  
  #FB
  so$fb[i] <- ifelse(is_empty(raw_2.1$col_id[which(so$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                     grepl("FB", raw_2.1$col_id)==TRUE)]),
                     NA,
                     strsplit(str_c(raw_2.1$col_id[which(so$date[i]==as.character(as.Date(raw_2.1$col_start_dt)) & 
                                                           grepl("FB", raw_2.1$col_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  temp_fb <- ifelse(is_empty(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_received)) &
                                                    grepl("FB", raw_3.1$lab_id)==TRUE)]),
                    NULL,
                    strsplit(str_c(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_received)) &
                                                          grepl("FB", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "), ", "))
  
  temp_fb2 <- Map(c, so$fb[i], temp_fb)
  un <- unlist(temp_fb2)
  so$fb[i] <- Map(`[`, temp_fb2, relist(!duplicated(un), skeleton = temp_fb2))
  
  so$fb[i] <- ifelse(is_empty(fb_sample_match$fb_id[which(so$col_id[i]==fb_sample_match$other_id)]),
                     so$fb[i],
                     fb_sample_match$fb_id[which(so$col_id[i]==fb_sample_match$other_id)]) 
  
  so$fb_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$fb[[i]])]),
                        NA,
                        str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$fb[[i]])], sep=", ", collapse=", "))
  so$fb_up[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% so$fb[[i]])]),
                        NA,
                        str_c(raw_3.5$lab_notes[which(raw_3.5$temp_id %in% so$fb[[i]])], sep=", ", collapse=", "))
  
  so$ncec[i] <- ifelse(is_empty(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                       grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)]),
                       NA,
                       str_c(raw_3.1$lab_id[which(so$date[i]==as.character(as.Date(raw_3.1$lab_processing)) & 
                                                    grepl("NEGATIVE|NCEC", raw_3.1$lab_id)==TRUE)], sep=", ", collapse=", "))
  so$ncec_ec[i] <- ifelse(is_empty(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$ncec[[i]])]),
                          NA,
                          str_c(raw_3.1$lab_1_ecoli_membrane[which(raw_3.1$lab_id %in% so$ncec[[i]])], sep=", ", collapse=", "))
  
  so$upnc[i] <- ifelse(is_empty(raw_3.5$lab_id[which(so$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                       grepl("UPNC", raw_3.5$lab_id)==TRUE)]),
                       NA,
                       str_c(raw_3.5$lab_id[which(so$date[i]==as.character(as.Date(raw_3.5$lab_processing)) & 
                                                    grepl("UPNC", raw_3.5$lab_id)==TRUE)], sep=", ", collapse=", "))
  so$upnc_tb[i] <- ifelse(is_empty(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])]) | 
                            anyNA(raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])]) |
                            isTRUE(grepl("not|no", raw_3.5$lab_notes[which(raw_3.5$lab_id %in% so$upnc[[i]])], ignore.case=TRUE)),
                          NA,
                          "Turbid")
  
  if (i==1) message("SO Final")
  progress(value=(((i-1)/nrow(so))*100), progress.bar=TRUE)
  if (i==nrow(so)) message("DONE!")
}


so$col_id[which(so$date != so$mf_date)]
#This shows which samples have different sample type codes for sampling vs analysis
so$col_id[which(so$sample_type != so$mf_type | 
                   so$sample_type !=so$mst_type | 
                   so$sample_type !=so$dna_type | 
                   so$sample_type !=so$pcr_type | 
                   so$sample_type !=so$enr_type)]
so$col_id[which(so$sample_ward != so$mf_ward | 
                   so$sample_ward !=so$mst_ward | 
                   so$sample_ward !=so$dna_ward | 
                   so$sample_ward !=so$pcr_ward | 
                   so$sample_ward !=so$enr_ward)]
so$col_id[which(so$sample_hood != so$mf_hood | 
                   so$sample_hood !=so$mst_hood | 
                   so$sample_hood !=so$dna_hood | 
                   so$sample_hood !=so$pcr_hood | 
                   so$sample_hood !=so$enr_hood)]





