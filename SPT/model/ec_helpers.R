# Functions specifically for ec calculations

ec_prepare_mf <- function(lab_data, reading, value) {
  lab_data$lab_1_ecoli<-factor_to_numeric(lab_data$lab_1_ecoli_membrane)
  lab_data$lab_2_ecoli<-factor_to_numeric(lab_data$lab_2_ecoli_membrane)
  lab_data$lab_3_ecoli<-factor_to_numeric(lab_data$lab_3_ecoli_membrane)
  lab_data$lab_1_ecoli[which(lab_data$lab_1_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC #should we differentiate TNTC and TDTC?
  lab_data$lab_1_ecoli[which(lab_data$lab_1_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  lab_data$lab_2_ecoli[which(lab_data$lab_2_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC
  lab_data$lab_2_ecoli[which(lab_data$lab_2_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  lab_data$lab_3_ecoli[which(lab_data$lab_3_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC
  lab_data$lab_3_ecoli[which(lab_data$lab_3_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  
  return(lab_data)
}

ec_merge <- function(collection_data, lab_data) {
  #Capitalized and remove missings in ID and sample types for merge purpose;
  # collection_data[,c("col_sample_type","col_id")] <- apply(collection_data[,c("col_sample_type","col_id")], 2, function(x) toupper(x))
  # lab_data[,c("lab_sample_type","lab_id")] <- apply(lab_data[,c("lab_sample_type","lab_id")], 2, function(x) toupper(x))
  collection_data$col_id<-gsub(" ","",collection_data$col_id)
  lab_data$lab_id<-gsub(" ","",lab_data$lab_id)
  
  #Casey - changed to all.x=TRUE instead of all=TRUE (that was giving all ES samples as well)
  ec_data<-merge(collection_data,lab_data,by.x=c("col_sample_type","col_id"),by.y=c("lab_sample_type","lab_id"), all=TRUE)
  names(ec_data)[which(names(ec_data)=="col_sample_type")]<-"sample_type"
  names(ec_data)[which(names(ec_data)=="col_id")]<-"sampleid"
  
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  names(ec_data) %<>% gsub('lab_\\d{1,}_group.', "", .)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume #If lab protocol won't change, this won't change. We can ask Suraja about this part;
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli)
  
  return(ec_data)
}

ec_merge_so <- function(collection_data, lab_data) {

  lab_data <- lab_data %>% filter(lab_sample_type %in% c(15, 14))
  lab_data <- lab_data[-c(1:7),]
  # merging the files by the SO index with sampling info
  lab_data$so_index=NA
  for(i in 1:nrow(lab_data)){
    lab_data$so_index[i]<-collection_data$`_index`[which(
      lab_data$lab_id[i] == collection_data$col_id_so |
        lab_data$lab_id[i] == collection_data$col_coa_id |
        lab_data$lab_id[i] == collection_data$col_cob_ids |
        lab_data$lab_id[i] == collection_data$col_cob_ids2 |
        lab_data$lab_id[i] == collection_data$col_cob_ids3 |
        lab_data$lab_id[i] == collection_data$col_cob_ids4)]
  }
  
  
  ec_data <- merge(collection_data, lab_data, by.y="so_index", by.x="_index")
  names(ec_data)[which(names(ec_data)=="lab_id")]<-"sampleid"  
  ec_data <- ec_data %>%
    mutate(sample_type = ifelse(lab_sample_type==14,
                                lab_sample_type_fp,
                                lab_sample_type_co))
  
 
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  names(ec_data) %<>% gsub('lab_\\d{1,}_group.', "", .)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume #If lab protocol won't change, this won't change. We can ask Suraja about this part;
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli)
  
  return(ec_data)
}


ec_add_denoms <- function(ec_data, denoms, sample_type_code = configure$sample_type_code) {
  ec_data$lab_sf_weight = factor_to_numeric(ec_data$lab_sf_weight)
  # set default denominator
  ec_data$ec_denom= denoms$default
  # set any other denominators
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$p)] = denoms$p
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$l)] = denoms$l
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$pa)] = denoms$pa
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$coa)] = denoms$coa
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$cob)] = denoms$cob
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$fpa)] = denoms$fpa
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$fpc)] = denoms$fpc
  ec_data$ec_denom[which(ec_data$sample_type == sample_type_code$sf)] = denoms$sf*ec_data$lab_sf_weight[which(ec_data$sample_type== sample_type_code$sf)]
  # street food has a sepcial calculation
  #street food used WASH benefit protocol 10 grams into 100 mL, serving size was defined using weight of street food sampled.
  
  # set any blanks to NA
  ec_data$ec_denom[is.na(ec_data$sample_type)]=NA
  
  return(ec_data)
}

ec_calc_swaps <- function(ec_data) {
  # the if_else pipe at the end will revalue any NA with F or pass the original T/F value
  # this is necessary because of the sample data uploaded using the Kobo csv_import tool
  # which revalued the lab dilution values to all be na
  swap1 = (ec_data$ec_dil1 >= ec_data$ec_dil2 & is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap2 = (ec_data$ec_dil1 < ec_data$ec_dil2 & is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap3 = (ec_data$ec_dil1 >= ec_data$ec_dil2 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap4 = (ec_data$ec_dil1 < ec_data$ec_dil2 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap5 = (ec_data$ec_dil2 >= ec_data$ec_dil3 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap6 = (ec_data$ec_dil2 < ec_data$ec_dil3 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap7 = (ec_data$ec_dil3 >= ec_data$ec_dil1 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  swap8 = (ec_data$ec_dil3 < ec_data$ec_dil1 & !is.na(ec_data$ec_dil3)) %>% if_else(is.na(.), F, .)
  
  ec_data$count1[swap1]<-ec_data$ec_ecnt1[swap1]
  ec_data$count2[swap1]<-ec_data$ec_ecnt2[swap1]
  ec_data$dil1[swap1]<-ec_data$ec_dil1[swap1]
  ec_data$dil2[swap1]<-ec_data$ec_dil2[swap1]
  
  ec_data$count2[swap2]<-ec_data$ec_ecnt1[swap2]
  ec_data$count1[swap2]<-ec_data$ec_ecnt2[swap2]
  ec_data$dil2[swap2]<-ec_data$ec_dil1[swap2]
  ec_data$dil1[swap2]<-ec_data$ec_dil2[swap2]
  
  ec_data$count1[swap3 & swap5 & swap8]<-ec_data$ec_ecnt1[swap3 & swap5 & swap8]
  ec_data$count2[swap3 & swap5 & swap8]<-ec_data$ec_ecnt2[swap3 & swap5 & swap8]
  ec_data$count3[swap3 & swap5 & swap8]<-ec_data$ec_ecnt3[swap3 & swap5 & swap8]
  ec_data$dil1[swap3 & swap5 & swap8]<-ec_data$ec_dil1[swap3 & swap5 & swap8]
  ec_data$dil2[swap3 & swap5 & swap8]<-ec_data$ec_dil2[swap3 & swap5 & swap8]
  ec_data$dil3[swap3 & swap5 & swap8]<-ec_data$ec_dil3[swap3 & swap5 & swap8]
  
  ec_data$count1[swap3 & swap6 & swap7]<-ec_data$ec_ecnt3[swap3 & swap6 & swap7]
  ec_data$count2[swap3 & swap6 & swap7]<-ec_data$ec_ecnt1[swap3 & swap6 & swap7]
  ec_data$count3[swap3 & swap6 & swap7]<-ec_data$ec_ecnt2[swap3 & swap6 & swap7]
  ec_data$dil1[swap3 & swap6 & swap7]<-ec_data$ec_dil3[swap3 & swap6 & swap7]
  ec_data$dil2[swap3 & swap6 & swap7]<-ec_data$ec_dil1[swap3 & swap6 & swap7]
  ec_data$dil3[swap3 & swap6 & swap7]<-ec_data$ec_dil2[swap3 & swap6 & swap7]
  
  ec_data$count1[swap4 & swap5 & swap7]<-ec_data$ec_ecnt2[swap4 & swap5 & swap7]
  ec_data$count2[swap4 & swap5 & swap7]<-ec_data$ec_ecnt3[swap4 & swap5 & swap7]
  ec_data$count3[swap4 & swap5 & swap7]<-ec_data$ec_ecnt1[swap4 & swap5 & swap7]
  ec_data$dil1[swap4 & swap5 & swap7]<-ec_data$ec_dil2[swap4 & swap5 & swap7]
  ec_data$dil2[swap4 & swap5 & swap7]<-ec_data$ec_dil3[swap4 & swap5 & swap7]
  ec_data$dil3[swap4 & swap5 & swap7]<-ec_data$ec_dil1[swap4 & swap5 & swap7]
  
  ec_data$count1[swap3 & swap6 & swap8]<-ec_data$ec_ecnt1[swap3 & swap6 & swap8]
  ec_data$count2[swap3 & swap6 & swap8]<-ec_data$ec_ecnt3[swap3 & swap6 & swap8]
  ec_data$count3[swap3 & swap6 & swap8]<-ec_data$ec_ecnt2[swap3 & swap6 & swap8]
  ec_data$dil1[swap3 & swap6 & swap8]<-ec_data$ec_dil1[swap3 & swap6 & swap8]
  ec_data$dil2[swap3 & swap6 & swap8]<-ec_data$ec_dil3[swap3 & swap6 & swap8]
  ec_data$dil3[swap3 & swap6 & swap8]<-ec_data$ec_dil2[swap3 & swap6 & swap8]
  
  ec_data$count1[swap4 & swap5 & swap8]<-ec_data$ec_ecnt2[swap4 & swap5 & swap8]
  ec_data$count2[swap4 & swap5 & swap8]<-ec_data$ec_ecnt1[swap4 & swap5 & swap8]
  ec_data$count3[swap4 & swap5 & swap8]<-ec_data$ec_ecnt3[swap4 & swap5 & swap8]
  ec_data$dil1[swap4 & swap5 & swap8]<-ec_data$ec_dil2[swap4 & swap5 & swap8]
  ec_data$dil2[swap4 & swap5 & swap8]<-ec_data$ec_dil1[swap4 & swap5 & swap8]
  ec_data$dil3[swap4 & swap5 & swap8]<-ec_data$ec_dil3[swap4 & swap5 & swap8]
  
  ec_data$count1[swap4 & swap6 & swap7]<-ec_data$ec_ecnt3[swap4 & swap6 & swap7]
  ec_data$count2[swap4 & swap6 & swap7]<-ec_data$ec_ecnt2[swap4 & swap6 & swap7]
  ec_data$count3[swap4 & swap6 & swap7]<-ec_data$ec_ecnt1[swap4 & swap6 & swap7]
  ec_data$dil1[swap4 & swap6 & swap7]<-ec_data$ec_dil3[swap4 & swap6 & swap7]
  ec_data$dil2[swap4 & swap6 & swap7]<-ec_data$ec_dil2[swap4 & swap6 & swap7]
  ec_data$dil3[swap4 & swap6 & swap7]<-ec_data$ec_dil1[swap4 & swap6 & swap7]
  
  return(ec_data)
  
}

ec_check_dilutions <- function(ec_data) {
  dils <- list(
    dilution3=!is.na(ec_data$ec_dil3),
    no_dilution3=is.na(ec_data$ec_dil3),
    
    #check whether threre is a dilution jumping.
    dil_jump1_1=abs(ec_data$dil1/ec_data$dil2-10)<0.0001,
    dil_jump1_2=abs(ec_data$dil1/ec_data$dil2-100)<0.0001,
    dil_jump2_1=abs(ec_data$dil2/ec_data$dil3-10)<0.0001,
    dil_jump2_2=abs(ec_data$dil2/ec_data$dil3-100)<0.0001
  )
  
  return(dils)
}

ec_mf_conditions <- function(ec_data, value) {
  
  dils <- ec_check_dilutions(ec_data)
  
  lapply(names(dils), function(x) assign(x, dils[x]))
  
  #two dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition1=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & dils$no_dilution3)
  condition2=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & dils$no_dilution3)
  condition3=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition4=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & dils$no_dilution3 & dils$dil_jump1_1)
  condition5=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition6=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition7=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3 & dils$dil_jump1_1)
  condition8=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition9=which(ec_data$count1== value$lower_limit & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition10=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2== value$lower_limit & dils$no_dilution3 & dils$dil_jump1_2)
  
  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & (ec_data$count3== value$TNTC | ec_data$count3== value$TDTC) & dils$dilution3)
  condition12=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit & dils$dilution3)
  condition13=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3)
  condition14=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3== value$lower_limit & dils$dilution3 & dils$dil_jump2_2)
  condition15=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit & dils$dilution3)
  condition16=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3)
  condition17=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3== value$lower_limit & dils$dilution3)
  condition18=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3 & dils$dil_jump2_1)
  condition19=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3 & dils$dil_jump2_2)
  condition20=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit & dils$dilution3)
  condition21=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit & dils$dilution3 & dils$dil_jump1_2)
  condition22=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit & dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition23=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition24=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3 & dils$dil_jump1_2 & dils$dil_jump2_2)
  condition25=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3== value$lower_limit & dils$dilution3)
  condition26=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point & dils$dilution3)
  condition27=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit & dils$dilution3)
  condition28=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit & dils$dilution3)
  condition29=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit & dils$dilution3 & dils$dil_jump1_1)
  condition30=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit & dils$dilution3 & dils$dil_jump1_2)
  condition31=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit & dils$dilution3)
  condition32=which(ec_data$count1== value$lower_limit & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit & dils$dilution3)
  
  conditionFB=which(ec_data$sample_type==99)
  
  # membrane specific ----
  
  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]= value$upper_limit/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=ec_data$count1[condition5]/ec_data$dil1[condition5]*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  #since jumping dilution
  #ec_con[condition9]=0.5/(ec_data$dil1[condition9]+ec_data$dil2[condition9])*ec_data$ec_denom[condition9]
  ec_con[condition9]= value$negative/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]= value$upper_limit/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]
  
  ec_con[condition11]= value$upper_limit/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
  ec_con[condition12]=ec_data$count3[condition12]/ec_data$dil3[condition12]*ec_data$ec_denom[condition12]
  ec_con[condition13]=ec_data$count3[condition13]/ec_data$dil3[condition13]*ec_data$ec_denom[condition13]
  ec_con[condition14]= value$upper_limit/ec_data$dil2[condition14]*ec_data$ec_denom[condition14]
  ec_con[condition15]=(ec_data$count2[condition15]/ec_data$dil2[condition15]+ec_data$count3[condition15]/ec_data$dil3[condition15])/2*ec_data$ec_denom[condition15]
  ec_con[condition16]=ec_data$count2[condition16]/ec_data$dil2[condition16]*ec_data$ec_denom[condition16]
  ec_con[condition17]=ec_data$count2[condition17]/ec_data$dil2[condition17]*ec_data$ec_denom[condition17]
  ec_con[condition18]=ec_data$count3[condition18]/ec_data$dil3[condition18]*ec_data$ec_denom[condition18]
  ec_con[condition19]=ec_data$count2[condition19]/ec_data$dil2[condition19]*ec_data$ec_denom[condition19]
  ec_con[condition20]=ec_data$count2[condition20]/ec_data$dil2[condition20]*ec_data$ec_denom[condition20]
  ec_con[condition21]= value$upper_limit/ec_data$dil1[condition21]*ec_data$ec_denom[condition21]
  ec_con[condition22]=(ec_data$count1[condition22]/ec_data$dil1[condition22]+ec_data$count2[condition22]/ec_data$dil2[condition22]+ec_data$count3[condition22]/ec_data$dil3[condition22])/3*ec_data$ec_denom[condition22]
  ec_con[condition23]=(ec_data$count1[condition23]/ec_data$dil1[condition23]+ec_data$count2[condition23]/ec_data$dil2[condition23])/2*ec_data$ec_denom[condition23]
  ec_con[condition24]=ec_data$count2[condition24]/ec_data$dil2[condition24]*ec_data$ec_denom[condition24]
  ec_con[condition25]=(ec_data$count1[condition25]/ec_data$dil1[condition25]+ec_data$count2[condition25]/ec_data$dil2[condition25])/2*ec_data$ec_denom[condition25]
  ec_con[condition26]=ec_data$count1[condition26]/ec_data$dil1[condition26]*ec_data$ec_denom[condition26]
  ec_con[condition27]=ec_data$count1[condition27]/ec_data$dil1[condition27]*ec_data$ec_denom[condition27]
  ec_con[condition28]=ec_data$count1[condition28]/ec_data$dil1[condition28]*ec_data$ec_denom[condition28]
  ec_con[condition29]=(ec_data$count1[condition29]/ec_data$dil1[condition29]+ec_data$count2[condition29]/ec_data$dil2[condition29])/2*ec_data$ec_denom[condition29]
  ec_con[condition30]=ec_data$count2[condition30]/ec_data$dil2[condition30]*ec_data$ec_denom[condition30]
  ec_con[condition31]=ec_data$count1[condition31]/ec_data$dil1[condition31]*ec_data$ec_denom[condition31]
  ec_con[condition32]= value$negative/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  
  ec_con[conditionFB]= ec_data$ec_ecnt1[conditionFB]/ec_data$ec_dil1[conditionFB]*ec_data$ec_denom[conditionFB]
  
  ec_binary<-rep(NA,length(ec_data$ec_ecnt1))
  ec_binary[c(condition1, condition2, condition3, condition4, condition5, condition6, condition7, condition8, condition10, condition11,
              condition12, condition13, condition14, condition15, condition16, condition17, condition18, condition19, condition20,
              condition21, condition22, condition23, condition24, condition25, condition26, condition27, condition28, condition29,
              condition30, condition31)] <- 1
  ec_binary[c(condition9, condition32)] <- 0
  
  ec_data$ec_conc<-ec_con
  ec_data$ec_binary <- ec_binary
  
  return(ec_data)
}
