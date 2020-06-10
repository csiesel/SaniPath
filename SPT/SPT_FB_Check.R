path <- "/Users/caseysiesel/Desktop/SaniPath/"

source(paste0(path, "SPT/6_SPT_ALLDATA_Combine.R"))


# SPT Data w/ diff FB issues ----------------------------------------------
#SPT Samples w/ PCR issues in FB
spt_pcr_fb <- spt1_final$col_id[which((grepl("1", spt1_final$fb_typhi_pos) | 
                                         grepl("1", spt1_final$fb_typhi_pres) |
                                         grepl("1", spt1_final$fb_paratyphi_pos) |
                                         grepl("1", spt1_final$fb_paratyphi_pres)) & spt1_final$sample_type!=99)]

#SPT Samples w/ EC issues in FB
spt_ec_fb <- ""
for(i in 1:nrow(spt1_final)){
  spt_ec_fb <- append(spt_ec_fb, 
                     ifelse(unique(unlist(strsplit(str_replace_all(spt1_final$fb_ec[i], ", ", ""), "")))!="0"
                            & spt1_final$sample_type[i]!=99, spt1_final$col_id[i], NA))
}
spt_ec_fb <- unique(spt_ec_fb[spt_ec_fb %!in% c("", NA)])

#SPT Samples w/ NO FB
spt_no_fb <- spt1_final$col_id[which(is.na(spt1_final$fb) & spt1_final$sample_type!=99)]



# ES Data w/ diff FB issues -----------------------------------------------
#ES Samples w/ PCR issues in FB
es_pcr_fb <- es1_final$col_id[which((grepl("1", es1_final$fb_typhi_pos) | 
                                         grepl("1", es1_final$fb_typhi_pres) |
                                         grepl("1", es1_final$fb_paratyphi_pos) |
                                         grepl("1", es1_final$fb_paratyphi_pres)) & es1_final$sample_type!=99)]
#ES Samples w/ EC issues in FB
es_ec_fb <- ""
for(i in 1:nrow(es1_final)){
  es_ec_fb <- append(es_ec_fb, 
                     ifelse(unique(unlist(strsplit(str_replace_all(es1_final$fb_ec[i], ", ", ""), "")))!="0"
    & es1_final$sample_type[i]!=99, es1_final$col_id[i], NA))
}
es_ec_fb <- unique(es_ec_fb[es_ec_fb %!in% c("", NA)])

#ES Samples w/ NO FB
es_no_fb <- es1_final$col_id[which(is.na(es1_final$fb) & es1_final$sample_type!=99)]



# SO Data w/ diff FB issues -----------------------------------------------
#SO Samples w/ PCR issues in FB
so_pcr_fb <- so_final$col_id[which((grepl("1", so_final$fb_typhi_pos) | 
                                         grepl("1", so_final$fb_typhi_pres) |
                                         grepl("1", so_final$fb_paratyphi_pos) |
                                         grepl("1", so_final$fb_paratyphi_pres)) & so_final$sample_type!=99)]
#SO Samples w/ EC issues in FB
so_ec_fb <- ""
for(i in 1:nrow(so_final)){
  so_ec_fb <- append(so_ec_fb, 
                     ifelse(unique(unlist(strsplit(str_replace_all(so_final$fb_ec[i], ", ", ""), "")))!="0"
                            & so_final$sample_type[i]!=99, so_final$col_id[i], NA))
}
so_ec_fb <- unique(so_ec_fb[so_ec_fb %!in% c("", NA)])

#SO Samples w/ NO FB
so_no_fb <- so_final$col_id[which(is.na(so_final$fb) & so_final$sample_type!=99)]




# Saving final files ------------------------------------------------------


save(spt1_final, file="SPT/spt_final.rda")
save(es1_final, file="SPT/es_final.rda")
save(so_final, file="SPT/so_final.rda")



#### DUP ID - CURRENTLY IN 4b and 4c ####
# mst_conc$raw_id <- str_extract(mst_conc$lab_id, "[:alpha:]+\\d{4}")
# mst_conc$raw_id_char <- str_extract(mst_conc$lab_id, "^[:alpha:]+")
# mst_conc$raw_end_char <- str_extract(mst_conc$lab_id, "[:alpha:]+$")
# #Match samples with same sample type and raw_id and pull lab_id to show which are duplicates
# mst_conc$dups <- ""
# mst_conc$dup_wg5<-""
# mst_conc$dup_gb124<-""
# for(i in 1:nrow(mst_conc)){
#   mst_conc$dups[i]<- ifelse(is_empty(mst_conc$lab_id[which(mst_conc$raw_id == mst_conc$raw_id[i] & 
#                                                              mst_conc$raw_id_char==mst_conc$raw_id_char[i])]),
#                             NA,
#                       strsplit(str_c(
#                         mst_conc$lab_id[which(mst_conc$raw_id == mst_conc$raw_id[i] & 
#                                                 mst_conc$raw_id_char==mst_conc$raw_id_char[i])], 
#                         sep=", ", collapse=", "), ", "))
#   
#   
#   # templist <- mst_conc$dups[[i]]
#   # mst_conc$dups[[i]] <- templist[templist != mst_conc$lab_id[i]]
#   
#   mst_conc$dup_wg5[i] <- ifelse(is.na(mst_conc$dups[i]),
#                                 NA,
#                                 str_c(mst_conc$calc_wg5_conc[which(mst_conc$lab_id %in% mst_conc$dups[[i]])], 
#                                 sep=", ", collapse=", "))
#   mst_conc$dup_gb124[i] <- ifelse(is.na(mst_conc$dups[i]),
#                                 NA,
#                                 str_c(mst_conc$calc_gb124_conc[which(mst_conc$lab_id %in% mst_conc$dups[[i]])], 
#                                       sep=", ", collapse=", "))
# 
# }
# 
# mst_repeat2 <- mst_conc %>% select("lab_id", "dups", "dup_wg5", "dup_gb124") %>% filter(grepl(",", dup_gb124))
# 
# 
# 
# # sum(mst_repeat[which(strsplit(mst_repeat$dup_gb124, ", "))])
# 
# 
# 
# 
# 
# pcr$raw_id <- str_extract(pcr$lab_id, "\\d{4}")
# pcr$raw_id_char <- str_extract(pcr$lab_id, "^[:alpha:]+")
# pcr$raw_end_char <- str_extract(pcr$lab_id, "[:alpha:]+$")
# #Match samples with same sample type and raw_id and pull lab_id to show which are duplicates
# pcr$dups <- ""
# pcr$dup_typhi_pos<-""
# pcr$dup_typhi_pres<-""
# pcr$dup_para_pos<-""
# pcr$dup_para_pres<-""
# for(i in 1:nrow(pcr)){
#   pcr$dups[i]<- ifelse(is_empty(pcr$lab_id[which(pcr$raw_id == pcr$raw_id[i] & 
#                                                              pcr$raw_id_char==pcr$raw_id_char[i])]),
#                             NA,
#                             strsplit(str_c(
#                               pcr$lab_id[which(pcr$raw_id == pcr$raw_id[i] & 
#                                                       pcr$raw_id_char==pcr$raw_id_char[i])], 
#                               sep=", ", collapse=", "), ", "))
#   pcr$dup_typhi_pos[i] <- ifelse(is.na(pcr$dups[i]),
#                                 NA,
#                                 str_c(pcr$typhi_positive[which(pcr$lab_id %in% pcr$dups[[i]])], 
#                                       sep=", ", collapse=", "))
#   pcr$dup_typhi_pres[i] <- ifelse(is.na(pcr$dups[i]),
#                                   NA,
#                                   str_c(pcr$typhi_presumptive[which(pcr$lab_id %in% pcr$dups[[i]])], 
#                                         sep=", ", collapse=", "))
#   pcr$dup_para_pos[i] <- ifelse(is.na(pcr$dups[i]),
#                                  NA,
#                                  str_c(pcr$paratyphi_positive[which(pcr$lab_id %in% pcr$dups[[i]])], 
#                                        sep=", ", collapse=", "))
#   pcr$dup_para_pres[i] <- ifelse(is.na(pcr$dups[i]),
#                                   NA,
#                                   str_c(pcr$paratyphi_presumptive[which(pcr$lab_id %in% pcr$dups[[i]])], 
#                                         sep=", ", collapse=", "))
# }
# 
# pcr_repeat <- pcr %>% select("lab_id", "dups", "dup_typhi_pos", "dup_typhi_pres",
#                              "dup_para_pos", "dup_para_pres") %>% filter(grepl(",", dup_typhi_pos))
# 
# 


#### OLD FB CHECKS ####
# #Typhi pos FB
# ty_pos <- pcr$lab_id[which(pcr$lab_sample_type==99 & pcr$typhi_positive==1)]
# #Typhi pos | pres FB
# ty_pos_pres <- pcr$lab_id[which(pcr$lab_sample_type==99 & (pcr$typhi_positive==1 |pcr$typhi_presumptive==1))]
# #Para pos FB
# pt_pos <- pcr$lab_id[which(pcr$lab_sample_type==99 & pcr$paratyphi_positive==1)]
# #Para pos | pres FB
# pt_pos_pres <- pcr$lab_id[which(pcr$lab_sample_type==99 & (pcr$paratyphi_positive==1 |pcr$paratyphi_presumptive==1))]
# 
# #FB with any PCR flag of contamination (unique of two lists)
# any_pcr_FB <- unique(c(pt_pos_pres, pt_pos, ty_pos_pres, ty_pos))
# 
# paste0("FB with PCR contamination: ", paste0(any_pcr_FB, collapse=", "))
# 

# #### FB with any EC contamination ###
# any_ec_FB_spt <- spt_final$col_id[which(spt_final$fb_ec>=1 & grepl("FB", spt_final$col_id)==FALSE)]
# any_ec_FB_es <- es_final$col_id[which(es_final$fb_ec>=1 & grepl("FB", es_final$col_id)==FALSE)]
# any_ec_FB_so <- so_final$col_id[which(so_final$fb_ec>=1 )]
# 
# paste0("SPT FB with any EC: ", paste0(any_ec_FB_spt, collapse=", "), " (",length(any_ec_FB_spt),")")
# paste0("ES FB with any EC: ", paste0(any_ec_FB_es, collapse=", "), " (",length(any_ec_FB_es),")")
# paste0("SO FB with any EC: ", paste0(any_ec_FB_so, collapse=", "), " (",length(any_ec_FB_so),")")
# 
# 
# any_ec_FB_spt1 <- spt_final$col_id[which(spt_final$fb_ec>=1 & grepl("FB", spt_final$col_id)==TRUE)]
# any_ec_FB_es1 <- es_final$col_id[which(sum(
#   as.numeric(
#     unlist(
#       strsplit(
#         str_replace_all(es1_final$fb_ec[which(es1_final$col_id=="PL1023")], ", ", ""), ""))))>0 & grepl("FB", es_final$col_id)==TRUE)]
# 
# 

