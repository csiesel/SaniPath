# Check if PCR data (and also E. coli and MST) is complete for all samples, so that we can ask NICED to upload the missing data.
# Send a list of SO and SPT samples that are PCR positive (to select for sequencing) or PCR presumptive (to select for re-PCR)
# Get the total number of PCR positive, and PCR presumptive samples for each sample type by neighborhood. 
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))


# SPT ---------------------------------------------------------------------
load("~/Desktop/SaniPath/SPT/data/spt1_final_2020-06-22.rda")
spt1_final <- spt1_final %>%
  mutate(sample_type = ifelse(sample_type==1, "Open Drain",
                              ifelse(sample_type==2, "Raw Produce",
                                     ifelse(sample_type==3, "Drinking Water",
                                            ifelse(sample_type==5, "Surface Water",
                                                   ifelse(sample_type==6, "Floodwater",
                                                          ifelse(sample_type==7, "Public Latrine",
                                                                 ifelse(sample_type==8, "Soil",
                                                                        ifelse(sample_type==9, "Bathing Water",
                                                                               ifelse(sample_type==10, "Street Food",
                                                                                      ifelse(sample_type==99, "Field Blank",
                                                                                             "??")))))))))))


#SPT Samples w/ PCR issues in FB
spt_pcr_fb <- spt1_final$col_id[which((grepl("1", spt1_final$fb_typhi_pos) | 
                                         grepl("1", spt1_final$fb_typhi_pres) |
                                         grepl("1", spt1_final$fb_paratyphi_pos) |
                                         grepl("1", spt1_final$fb_paratyphi_pres)) & 
                                        spt1_final$sample_type!="Field Blank")]

#SPT Samples w/ EC issues in FB
spt_ec_fb <- ""
for(i in 1:nrow(spt1_final)){
  spt_ec_fb <- append(spt_ec_fb, 
                      ifelse(unique(unlist(strsplit(str_replace_all(spt1_final$fb_ec[i], ", ", ""), "")))!="0"
                             & spt1_final$sample_type[i]!="Field Blank", spt1_final$col_id[i], NA))
}
spt_ec_fb <- unique(spt_ec_fb[spt_ec_fb %!in% c("", NA)])

#SPT Samples w/ NO FB
spt_no_fb <- spt1_final$col_id[which(is.na(spt1_final$fb) & spt1_final$sample_type!="Field Blank")]

#A+ data
spt_a_plus <- spt1_final %>% filter(col_id %!in% c(spt_no_fb, spt_ec_fb, spt_pcr_fb))

#A- data
spt_a_minus <- spt_a_plus
for(i in 1:nrow(spt_a_minus)){
  spt_a_minus$typhi_positive_a_minus[i] <- ifelse(spt_a_minus$t1[i] <=33 & spt_a_minus$t2[i] <=33,
                                  1, 0)
  spt_a_minus$paratyphi_positive_a_minus[i] <- ifelse(spt_a_minus$p1[i] <=33 & spt_a_minus$p2[i] <=33,
                                      1, 0)
}



spt_no_ec <- spt1_final %>% filter(is.na(mf_index)) %>% select(c("col_id", "date", "sample_type"))
# spt1_final$col_id[which(is.na(spt1_final$mf_index))]

spt_no_pcr <- spt1_final %>% filter(is.na(pcr_index)) %>% select(c("col_id", "date", "sample_type"))
# spt1_final$col_id[which(is.na(spt1_final$pcr_index))]

spt_typhi_presumptive_pcr <- spt1_final %>% filter(typhi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# spt1_final$col_id[which(spt1_final$typhi_presumptive==1)]

spt_paratyphi_presumptive_pcr <- spt1_final %>% filter(paratyphi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# spt1_final$col_id[which(spt1_final$paratyphi_presumptive==1)]

spt_positive_pcr <- spt1_final %>% filter(typhi_positive == 1 | paratyphi_positive ==1) %>% 
  select(c("col_id", "date", "sample_type", "sample_ward", "sample_hood", "typhi_positive", "paratyphi_positive"))

summary_spt_positive_pcr <- spt1_final %>% group_by(sample_type, sample_hood) %>% 
  dplyr::summarise('typhi +' = sum(typhi_positive==1, na.rm=T), 'para +' = sum(paratyphi_positive==1, na.rm=T),
                   'typhi pres' = sum(typhi_presumptive==1, na.rm=T), 'para pres' = sum(paratyphi_presumptive==1, na.rm=T))

# SO ----------------------------------------------------------------------
load("~/Desktop/SaniPath/SPT/data/so_final_2020-06-22.rda")
so_final <- so_final %>% mutate(sample_type = ifelse(sample_type_so==141, "Cutting /Grinding Surface (FPa)",
                                                     ifelse(sample_type_so==142, "Storage/Preparation Bowl (FPb)",
                                                            ifelse(sample_type_so==143, "Food Preparation Area (FPc)",
                                                                   ifelse(sample_type_so==144, "Cooking/Preparation Water (FPd)",
                                                                          ifelse(sample_type_so==145, "Cooking/Preparation Utensil (FPe)",
                                                                                 ifelse(sample_type_so==151, "Child Obs - Floor",
                                                                                        ifelse(sample_type_so==152, "Child Obs - Off Ground",
                                                                                               "??"))))))))

# SO Data w/ diff FB issues -----------------------------------------------
#SO Samples w/ PCR issues in FB
so_pcr_fb <- so_final$col_id[which((grepl("1", so_final$fb_typhi_pos) | 
                                      grepl("1", so_final$fb_typhi_pres) |
                                      grepl("1", so_final$fb_paratyphi_pos) |
                                      grepl("1", so_final$fb_paratyphi_pres)) & 
                                     so_final$sample_type!="Field Blank")]
#SO Samples w/ EC issues in FB
so_ec_fb <- ""
for(i in 1:nrow(so_final)){
  so_ec_fb <- append(so_ec_fb, 
                     ifelse(unique(unlist(strsplit(str_replace_all(so_final$fb_ec[i], ", ", ""), "")))!="0"
                            & so_final$sample_type[i]!="Field Blank", so_final$col_id[i], NA))
}
so_ec_fb <- unique(so_ec_fb[so_ec_fb %!in% c("", NA)])

#SO Samples w/ NO FB
so_no_fb <- so_final$col_id[which(is.na(so_final$fb) & so_final$sample_type!="Field Blank")]


#A+ data
so_a_plus <- so_final %>% filter(col_id %!in% c(so_no_fb, so_ec_fb, so_pcr_fb))

#A- data
so_a_minus <- so_a_plus
for(i in 1:nrow(so_a_minus)){
  so_a_minus$typhi_positive_a_minus[i] <- ifelse(so_a_minus$t1[i] <=33 & so_a_minus$t2[i] <=33,
                                                  1, 0)
  so_a_minus$paratyphi_positive_a_minus[i] <- ifelse(so_a_minus$p1[i] <=33 & so_a_minus$p2[i] <=33,
                                                      1, 0)
}


so_no_ec <- so_final %>% filter(is.na(mf_index)) %>% select(c("col_id", "date", "sample_type"))
# so_final$col_id[which(is.na(so_final$mf_index))]

so_no_pcr <- so_final %>% filter(is.na(pcr_index)) %>% select(c("col_id", "date", "sample_type"))
# so_final$col_id[which(is.na(so_final$pcr_index))]

so_typhi_presumptive_pcr <- so_final %>% filter(typhi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# so_final$col_id[which(so_final$typhi_presumptive==1)]

so_paratyphi_presumptive_pcr <- so_final %>% filter(paratyphi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# so_final$col_id[which(so_final$paratyphi_presumptive==1)]

so_positive_pcr <- so_final %>% filter(typhi_positive == 1 | paratyphi_positive ==1) %>% 
  select(c("col_id", "date", "sample_type", "sample_ward", "sample_hood", "typhi_positive", "paratyphi_positive"))

summary_so_positive_pcr <- so_final %>% group_by(sample_type, sample_hood) %>% 
  dplyr::summarise('typhi +' = sum(typhi_positive==1, na.rm=T), 'para +' = sum(paratyphi_positive==1, na.rm=T),
                   'typhi pres' = sum(typhi_presumptive==1, na.rm=T), 'para pres' = sum(paratyphi_presumptive==1, na.rm=T))

# ES ----------------------------------------------------------------------
load("~/Desktop/SaniPath/SPT/data/es1_final_2020-06-22.rda")
es1_final <- es1_final %>%  mutate(sample_type = ifelse(sample_type==11, ifelse(grepl("PLC", col_id),"Pooled Latrine Case", "Pooled Latrine"),
                                                        ifelse(sample_type==12, "Moore Swab",
                                                               ifelse(sample_type==16, "Pumping Station - UF",
                                                                      ifelse(sample_type==99, "Field Blank", "??"))))) %>% 
  filter(col_id %!in% c("UF1005", "UF1006", "UF1007"))



# ES Data w/ diff FB issues -----------------------------------------------
#ES Samples w/ PCR issues in FB
es_pcr_fb <- es1_final$col_id[which((grepl("1", es1_final$fb_typhi_pos) | 
                                       grepl("1", es1_final$fb_typhi_pres) |
                                       grepl("1", es1_final$fb_paratyphi_pos) |
                                       grepl("1", es1_final$fb_paratyphi_pres)) & 
                                      es1_final$sample_type!="Field Blank")]
#ES Samples w/ EC issues in FB
es_ec_fb <- ""
for(i in 1:nrow(es1_final)){
  es_ec_fb <- append(es_ec_fb, 
                     ifelse(unique(unlist(strsplit(str_replace_all(es1_final$fb_ec[i], ", ", ""), "")))!="0"
                            & es1_final$sample_type[i]!="Field Blank", es1_final$col_id[i], NA))
}
es_ec_fb <- unique(es_ec_fb[es_ec_fb %!in% c("", NA)])

#ES Samples w/ NO FB
es_no_fb <- es1_final$col_id[which(is.na(es1_final$fb) & es1_final$sample_type!="Field Blank")]


#A+ data
es_a_plus <- es1_final %>% filter(col_id %!in% c(es_no_fb, es_ec_fb, es_pcr_fb))

#A- data
es_a_minus <- es_a_plus
for(i in 1:nrow(es_a_minus)){
  es_a_minus$typhi_positive_a_minus[i] <- ifelse(es_a_minus$t1[i] <=33 & es_a_minus$t2[i] <=33,
                                                 1, 0)
  es_a_minus$paratyphi_positive_a_minus[i] <- ifelse(es_a_minus$p1[i] <=33 & es_a_minus$p2[i] <=33,
                                                     1, 0)
}



es_no_ec <- es1_final %>% filter(is.na(mf_index) & sample_type %in% c("Pooled Latrine","Pooled Latrine Case")) %>% select(c("col_id", "date", "sample_type"))
# es1_final$col_id[which(is.na(es1_final$mf_index))]

es_no_pcr <- es1_final %>% filter(is.na(pcr_index)) %>% select(c("col_id", "date", "sample_type"))
# es1_final$col_id[which(is.na(es1_final$pcr_index))]

es_typhi_presumptive_pcr <- es1_final %>% filter(typhi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# es1_final$col_id[which(es1_final$typhi_presumptive==1)]

es_paratyphi_presumptive_pcr <- es1_final %>% filter(paratyphi_presumptive==1) %>% select(c("col_id", "date", "sample_type"))
# es1_final$col_id[which(es1_final$paratyphi_presumptive==1)]

es_positive_pcr <- es1_final %>% filter(typhi_positive == 1 | paratyphi_positive ==1) %>% 
  select(c("col_id", "date", "sample_type", "sample_ward", "sample_hood", "typhi_positive", "paratyphi_positive"))

summary_es_positive_pcr <- es1_final %>% group_by(sample_type) %>% 
  dplyr::summarise('typhi +' = sum(typhi_positive==1, na.rm=T), 'para +' = sum(paratyphi_positive==1, na.rm=T),
                   'typhi pres' = sum(typhi_presumptive==1, na.rm=T), 'para pres' = sum(paratyphi_presumptive==1, na.rm=T))

