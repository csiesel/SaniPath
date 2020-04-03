# ****************************************************************************************
# Data Cleaning for SaniPath Dhaka Deployment
# ****************************************************************************************

setwd("~/stat/SaniPath Dhaka/")

collection_data <- read.csv("data/Sample_2017_09_05_14_26_21.csv", header=TRUE)
lab_data <- read.csv("data/Lab_Final_2017_09_05_14_25_06.csv", header=TRUE)
load("~/stat/Sanipath Dhaka/data/mpn_tbl.rda")

# ****************************************************************************************
factor_to_numeric <- function(x) {
        # convert factor or character data to numeric
        return(as.numeric(as.character(x)))
}

# ****************************************************************************************

#ajusting the sample type
lab_data$lab_sample_type[which(lab_data$lab_sample_type>=4)] = lab_data$lab_sample_type[which(lab_data$lab_sample_type>=4)]+1

#investigate duplicate entries
test1 <- lab_data[which(lab_data$lab_id %in% as.numeric(names(table(lab_data$lab_id)[table(lab_data$lab_id)>=2]))),]
test2 <- collection_data[which(collection_data$col_id %in% as.numeric(names(table(collection_data$col_id)[table(collection_data$col_id)>=2]))),]

#remove duplicated rows;
# col_data_rm_dup <- collection_data[!duplicated(collection_data[, c(5:8,15:80)] ), ]
# lab_data_rm_dup <- lab_data[!duplicated(lab_data[, c(5:7,11:37)]), ]

# table(col_data_rm_dup$col_sample_type)
# table(col_data_rm_dup$col_neighborhood) 
# # neighborhood 11 has 99 samples, while neighborhood 88 has 101 samples.
# # checked the scanned forms: sample #1089 is supposed to be in neighborhood 11, not 88.
# table(lab_data_rm_dup$lab_sample_type)

# ****************************************************************************************
lab_data1 <- lab_data
collection_data1 <- collection_data

# lab data
# fixing IDs
lab_data$lab_id[lab_data$start_dt=="2017-06-19T12:05:04.681+06"] <- 1963
lab_data$lab_id_val[lab_data$start_dt=="2017-06-19T12:05:04.681+06"] <- 1963
lab_data$lab_id[lab_data$start_dt=="2017-06-19T12:06:50.568+06"] <- 1964
lab_data$lab_id_val[lab_data$start_dt=="2017-06-19T12:06:50.568+06"] <- 1964

# removing duplicates
# 1007, 1014, 1119, 1133, 1137, 1220
lab_data_rm_dup <- lab_data[!duplicated(lab_data$lab_id), ]

# col data
collection_data$col_neighborhood[collection_data$col_id == 1089] <- 11
collection_data$col_id[collection_data$start_dt=="2017-05-08T14:00:58.259+06"] <- 1366
collection_data$col_id_val[collection_data$start_dt=="2017-05-08T14:00:58.259+06"] <- 1366

# remove duplicates:
# 1386, 1391, 1396, 1401, 1402, 1406, 1466, 1471, 1473, 1795, 1796, 1906, 1948,
# 1952, 1953, 1954, 1961, 1964, 1965, 1966, 1967, 1981, 1991, 1992, 1996, 1999
col_data_rm_dup <- collection_data[!duplicated(collection_data$col_id), ]
# ****************************************************************************************

table(col_data_rm_dup$col_sample_type)
table(col_data_rm_dup$col_neighborhood) 
table(lab_data_rm_dup$lab_sample_type)

# all data problems fixed
col_data_rm_dup[duplicated(col_data_rm_dup$lab_id)]
lab_data_rm_dup[, duplicated(lab_data_rm_dup$lab_id)]

# #check missing values;
# lab_data_rm_dup$lab_p_weight[which(lab_data_rm_dup$lab_sample_type==2)]
# lab_data_rm_dup$lab_sf_weight[which(lab_data_rm_dup$lab_sample_type==11)]
# #hist(factor_to_numeric(lab_data_rm_dup$lab_sf_weight[which(lab_data_rm_dup$lab_sample_type==11)]))
# lab_data_rm_dup$lab_pa_weight[which(lab_data_rm_dup$lab_sample_type==8)]

lab_data_rm_dup$lab_1_ecoli<-factor_to_numeric(lab_data_rm_dup$lab_1_ecoli)
lab_data_rm_dup$lab_2_ecoli<-factor_to_numeric(lab_data_rm_dup$lab_2_ecoli)
lab_data_rm_dup$lab_3_ecoli<-factor_to_numeric(lab_data_rm_dup$lab_3_ecoli)

lab_data_rm_dup$lab_1_ecoli_big<-factor_to_numeric(lab_data_rm_dup$lab_1_ecoli_big)
lab_data_rm_dup$lab_2_ecoli_big<-factor_to_numeric(lab_data_rm_dup$lab_2_ecoli_big)
lab_data_rm_dup$lab_3_ecoli_big<-factor_to_numeric(lab_data_rm_dup$lab_3_ecoli_big)
lab_data_rm_dup$lab_1_ecoli_small<-factor_to_numeric(lab_data_rm_dup$lab_1_ecoli_small)
lab_data_rm_dup$lab_2_ecoli_small<-factor_to_numeric(lab_data_rm_dup$lab_2_ecoli_small)
lab_data_rm_dup$lab_3_ecoli_small<-factor_to_numeric(lab_data_rm_dup$lab_3_ecoli_small)

# define upper limit for reading status 1, TNTC
lab_data_rm_dup$lab_1_ecoli_big[which(lab_data_rm_dup$lab_1_ecoli_reading==1)]<-49
lab_data_rm_dup$lab_1_ecoli_small[which(lab_data_rm_dup$lab_1_ecoli_reading==1)]<-48
lab_data_rm_dup$lab_2_ecoli_big[which(lab_data_rm_dup$lab_2_ecoli_reading==1)]<-49
lab_data_rm_dup$lab_2_ecoli_small[which(lab_data_rm_dup$lab_2_ecoli_reading==1)]<-48
lab_data_rm_dup$lab_3_ecoli_big[which(lab_data_rm_dup$lab_3_ecoli_reading==1)]<-49
lab_data_rm_dup$lab_3_ecoli_small[which(lab_data_rm_dup$lab_3_ecoli_reading==1)]<-48

#no IDEXX result missing
which(is.na(lab_data_rm_dup$lab_1_ecoli_big))
which(is.na(lab_data_rm_dup$lab_1_ecoli_small))
which(is.na(lab_data_rm_dup$lab_2_ecoli_big))
which(is.na(lab_data_rm_dup$lab_2_ecoli_small))
which(is.na(lab_data_rm_dup$lab_3_ecoli_big) & lab_data_rm_dup$lab_3_dilution_performed==1)
which(is.na(lab_data_rm_dup$lab_3_ecoli_small) & lab_data_rm_dup$lab_3_dilution_performed==1)

# IDEXX method to calculate the concentration-----------------------------
create_ecData_Idexx <- function(collection_data, lab_data){
  #function to prepare the ec_data for IDEXX;
  for (i in 1:length(lab_data$lab_id)){
    lab_data$lab_1_ecoli[i]<-mpn_tbl[lab_data$lab_1_ecoli_big[i]+1,lab_data$lab_1_ecoli_small[i]+1]
    lab_data$lab_2_ecoli[i]<-mpn_tbl[lab_data$lab_2_ecoli_big[i]+1,lab_data$lab_2_ecoli_small[i]+1]
    lab_data$lab_3_ecoli[i]<-mpn_tbl[lab_data$lab_3_ecoli_big[i]+1,lab_data$lab_3_ecoli_small[i]+1]
  }
  
  collection_data[,c("col_sample_type","col_id")] <- apply(collection_data[,c("col_sample_type","col_id")], 2, function(x) toupper(x))
  lab_data[,c("lab_sample_type","lab_id")] <- apply(lab_data[,c("lab_sample_type","lab_id")], 2, function(x) toupper(x))
  collection_data$col_id<-gsub(" ","",collection_data$col_id)
  lab_data$lab_id<-gsub(" ","",lab_data$lab_id)
  
  ec_data<-merge(collection_data,lab_data,by.x=c("col_sample_type","col_id"),by.y=c("lab_sample_type","lab_id"))
  names(ec_data)[which(names(ec_data)=="col_sample_type")]<-"sample_type"
  names(ec_data)[which(names(ec_data)=="col_id")]<-"sampleid"
  ec_data$ec_denom=100
  ec_data$ec_denom[which(ec_data$sample_type==2)]=500
  ec_data$ec_denom[which(ec_data$sample_type==2)]=14
  ec_data$ec_denom[which(ec_data$sample_type==8)]=2
  ec_data$ec_denom[is.na(ec_data$sample_type)]=NA
  
  #ec_data$neighbor<-factor_to_numeric(ec_data$neighbor)
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli)
  
  ec_data$ec_ecnt1[which(ec_data$lab_1_ecoli_reading==1)]<-9999
  ec_data$ec_ecnt2[which(ec_data$lab_2_ecoli_reading==1)]<-9999
  ec_data$ec_ecnt3[which(ec_data$lab_3_ecoli_reading==1)]<-9999
  
  swap1<-(ec_data$ec_dil1>=ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap2<-(ec_data$ec_dil1<ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap3<-(ec_data$ec_dil1>=ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap4<-(ec_data$ec_dil1<ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap5<-(ec_data$ec_dil2>=ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap6<-(ec_data$ec_dil2<ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap7<-(ec_data$ec_dil3>=ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))
  swap8<-(ec_data$ec_dil3<ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))
  
  dilution3<-!is.na(ec_data$ec_dil3)
  no_dilution3<-is.na(ec_data$ec_dil3)
  
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
  
  #check whether threre is a dilution jumping.
  dil_jump1_1<-abs(ec_data$dil1/ec_data$dil2-10)<0.0001
  dil_jump1_2<-abs(ec_data$dil1/ec_data$dil2-100)<0.0001
  dil_jump2_1<-abs(ec_data$dil2/ec_data$dil3-10)<0.0001
  dil_jump2_2<-abs(ec_data$dil2/ec_data$dil3-100)<0.0001
  
  #two dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition1=which(ec_data$count1==9999 & ec_data$count2==9999 & no_dilution3)
  condition2=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & no_dilution3)
  condition3=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3)
  condition4=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=200 & ec_data$count2<2419.6 & no_dilution3 & dil_jump1_1)
  condition5=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3)
  condition6=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2<1 & no_dilution3)
  condition7=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3 & dil_jump1_1)
  condition8=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3 & dil_jump1_2)
  condition9=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2<1 & no_dilution3)
  condition10=which(ec_data$count1<1 & ec_data$count2<1 & no_dilution3)
  
  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3==9999 & dilution3)
  condition12=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3>=200 & ec_data$count3<=2419.6 & dilution3)
  condition13=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3>=1 & ec_data$count3<200 & dilution3)
  condition15=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=200 & ec_data$count3<=2419.6 & dilution3 & dil_jump2_1)
  condition16=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=1 & ec_data$count3<200 & dilution3)
  condition17=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3<1 & dilution3)
  condition18=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump2_1)
  condition19=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump2_2)
  condition20=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3)
  
  condition23=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition25=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition26=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_2 & dil_jump2_2)
  condition27=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3)
  condition29=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3 & dil_jump1_1)
  condition30=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3 & dil_jump1_2)
  condition31=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2<1 & ec_data$count3<1 & dilution3)
  condition32=which(ec_data$count1<1 & ec_data$count2<1 & ec_data$count3<1 & dilution3)
  
  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]=2419.6/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=(ec_data$count1[condition5]/ec_data$dil1[condition5]+ec_data$count2[condition5]/ec_data$dil2[condition5])/2*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  ec_con[condition9]=ec_data$count1[condition9]/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]=0.5/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]
  ec_con[condition11]=2419.6/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
  ec_con[condition12]=ec_data$count3[condition12]/ec_data$dil3[condition12]*ec_data$ec_denom[condition12]
  ec_con[condition13]=ec_data$count3[condition13]/ec_data$dil3[condition13]*ec_data$ec_denom[condition13]
  ec_con[condition15]=(ec_data$count2[condition15]/ec_data$dil2[condition15]+ec_data$count3[condition15]/ec_data$dil3[condition15])/2*ec_data$ec_denom[condition15]
  ec_con[condition16]=(ec_data$count2[condition16]/ec_data$dil2[condition16]+ec_data$count3[condition16]/ec_data$dil3[condition16])/2*ec_data$ec_denom[condition16]
  ec_con[condition17]=ec_data$count2[condition17]/ec_data$dil2[condition17]*ec_data$ec_denom[condition17]
  ec_con[condition18]=(ec_data$count2[condition18]/ec_data$dil2[condition18]+ec_data$count3[condition18]/ec_data$dil3[condition18])/2*ec_data$ec_denom[condition18]
  ec_con[condition19]=ec_data$count2[condition19]/ec_data$dil2[condition19]*ec_data$ec_denom[condition19]
  ec_con[condition20]=ec_data$count2[condition20]/ec_data$dil2[condition20]*ec_data$ec_denom[condition20]
  ec_con[condition23]=(ec_data$count1[condition23]/ec_data$dil1[condition23]+ec_data$count2[condition23]/ec_data$dil2[condition23]+ec_data$count3[condition23]/ec_data$dil3[condition23])/3*ec_data$ec_denom[condition23]
  ec_con[condition25]=(ec_data$count1[condition25]/ec_data$dil1[condition25]+ec_data$count2[condition25]/ec_data$dil2[condition25]+ec_data$count3[condition25]/ec_data$dil3[condition25])/3*ec_data$ec_denom[condition25]
  ec_con[condition26]=(ec_data$count1[condition26]/ec_data$dil1[condition26]+ec_data$count2[condition26]/ec_data$dil2[condition26])/2*ec_data$ec_denom[condition26]
  ec_con[condition27]=(ec_data$count1[condition27]/ec_data$dil1[condition27]+ec_data$count2[condition27]/ec_data$dil2[condition27])/2*ec_data$ec_denom[condition27]
  ec_con[condition29]=(ec_data$count1[condition29]/ec_data$dil1[condition29]+ec_data$count2[condition29]/ec_data$dil2[condition29])/2*ec_data$ec_denom[condition29]
  ec_con[condition30]=ec_data$count1[condition30]/ec_data$dil1[condition30]*ec_data$ec_denom[condition30]
  ec_con[condition31]=ec_data$count1[condition31]/ec_data$dil1[condition31]*ec_data$ec_denom[condition31]
  ec_con[condition32]=0.5/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  ec_data$ec_conc<-ec_con
  ec_data$neighbor <- as.factor(ec_data$col_neighborhood)
  
  return(ec_data)
}

ec_data <- create_ecData_Idexx(col_data_rm_dup, lab_data_rm_dup)

# # fixed:
# col_data_rm_dup$col_id[(!col_data_rm_dup$col_id %in% ec_data$sampleid[ec_data$neighbor==66]) & col_data_rm_dup$col_neighborhood==66]
# lab_data_rm_dup$lab_id[(!lab_data_rm_dup$lab_id %in% ec_data$sampleid)]



