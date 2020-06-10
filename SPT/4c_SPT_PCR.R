path <- "/Users/caseysiesel/Desktop/SaniPath/"

source(paste0(path, "SPT/4b_SPT_MST.R"))



pcr$typhi_positive <- NA
pcr$typhi_presumptive <- NA

pcr$paratyphi_positive <- NA
pcr$paratyphi_presumptive <- NA

for(i in 1:nrow(pcr)){
  pcr$typhi_positive[i] <- ifelse(pcr$lab_pcr_ty_results1[i] <=33 & pcr$lab_pcr_ty_results2[i] <=33 & 
                                    abs(pcr$lab_pcr_ty_results1[i] - pcr$lab_pcr_ty_results2[i]) <=1.5,
                                  1, 0)
  pcr$paratyphi_positive[i] <- ifelse(pcr$lab_pcr_para_results1[i] <=33 & pcr$lab_pcr_para_results2[i] <=33 & 
                                    abs(pcr$lab_pcr_para_results1[i] - pcr$lab_pcr_para_results2[i]) <=1.5,
                                  1, 0)
  
  pcr$typhi_presumptive[i] <- ifelse((pcr$lab_pcr_ty_results1[i] <=33 | pcr$lab_pcr_ty_results2[i] <=33) & pcr$typhi_positive[i] != 1,
                                     1, 0)
  pcr$paratyphi_presumptive[i] <- ifelse((pcr$lab_pcr_para_results1[i] <=33 | pcr$lab_pcr_para_results2[i] <=33) & pcr$paratyphi_positive[i] != 1,
                                     1, 0)
}


count(pcr$typhi_positive==1)





# Checking repeat PCR values and fixing values ----------------------------
pcr$raw_id <- str_extract(pcr$lab_id, "\\d{4}")
pcr$raw_id_char <- str_extract(pcr$lab_id, "^[:alpha:]+")
pcr$raw_end_char <- str_extract(pcr$lab_id, "[:alpha:]+$")
#Match samples with same sample type and raw_id and pull lab_id to show which are duplicates
pcr$dups <- ""
pcr$dup_typhi_pos<-""
pcr$dup_typhi_pres<-""
pcr$dup_para_pos<-""
pcr$dup_para_pres<-""
for(i in 1:nrow(pcr)){
  pcr$dups[i]<- ifelse(is_empty(pcr$lab_id[which(pcr$raw_id == pcr$raw_id[i] & 
                                                   pcr$raw_id_char==pcr$raw_id_char[i])]),
                       NA,
                       strsplit(str_c(
                         pcr$lab_id[which(pcr$raw_id == pcr$raw_id[i] & 
                                            pcr$raw_id_char==pcr$raw_id_char[i])], 
                         sep=", ", collapse=", "), ", "))
  pcr$dup_typhi_pos[i] <- ifelse(is.na(pcr$dups[i]),
                                 NA,
                                 str_c(pcr$typhi_positive[which(pcr$lab_id %in% pcr$dups[[i]])], 
                                       sep=", ", collapse=", "))
  pcr$dup_typhi_pres[i] <- ifelse(is.na(pcr$dups[i]),
                                  NA,
                                  str_c(pcr$typhi_presumptive[which(pcr$lab_id %in% pcr$dups[[i]])], 
                                        sep=", ", collapse=", "))
  pcr$dup_para_pos[i] <- ifelse(is.na(pcr$dups[i]),
                                NA,
                                str_c(pcr$paratyphi_positive[which(pcr$lab_id %in% pcr$dups[[i]])], 
                                      sep=", ", collapse=", "))
  pcr$dup_para_pres[i] <- ifelse(is.na(pcr$dups[i]),
                                 NA,
                                 str_c(pcr$paratyphi_presumptive[which(pcr$lab_id %in% pcr$dups[[i]])], 
                                       sep=", ", collapse=", "))
}

pcr_repeat <- pcr %>% select("lab_id", "dups", "dup_typhi_pos", "dup_typhi_pres",
                             "dup_para_pos", "dup_para_pres") %>% filter(grepl(",", dup_typhi_pos))

#removing duplicate or filtered base ID's since they are covered in the "dups" column
pcr_repeat2 <- pcr_repeat %>% filter(grepl("D|F|FL$", lab_id)==FALSE)

pcr_repeat2$diff_value <- "yes"
for(i in 1:nrow(pcr_repeat2)){
  if(length(unique(unlist(strsplit(str_replace_all(pcr_repeat2$dup_typhi_pos[i], ", ", ""), ""))))==1 &
     length(unique(unlist(strsplit(str_replace_all(pcr_repeat2$dup_typhi_pres[i], ", ", ""), ""))))==1 &
     length(unique(unlist(strsplit(str_replace_all(pcr_repeat2$dup_para_pos[i], ", ", ""), ""))))==1 &
     length(unique(unlist(strsplit(str_replace_all(pcr_repeat2$dup_para_pres[i], ", ", ""), ""))))==1){
    pcr_repeat2$diff_value[i] <- "no"
  }
}

pcr_repeat_diff <- pcr_repeat2 %>% filter(diff_value=="yes")
