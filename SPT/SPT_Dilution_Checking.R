r_sample_type <- 
          c(1,      2,    3,   5,   6,   7,   8,   9,  10,  99,    141,  142,   143,  144,  145,   151,  152,    11,  12,    16)
r_dil1 <- c(0.001, 10,  100,   1,  .1,  10,  .1, 100,  .1, 100,      1,  100,     1,  100,  100,     1,    1, 0.001,  NA, 0.001)
r_dil2 <- c(0.0001, 1,   10, 0.1, .01,   1, .01,  10, .01,  NA,    0.1,   10,   0.1,   10,   10,   0.1,  0.1, 0.0001,  NA, 0.0001)
r_dil3 <- c(NA,    .1,   NA,0.01,.001,  NA,.001,  NA,.001,  NA,   0.01,  0.1,  0.01,  0.1,  0.1,  0.01, 0.01, 0.00001,  NA, 0.00001)

renuka_dil <- data.frame(r_sample_type, r_dil1, r_dil2, r_dil3)

add_renuka <- function(ec_data, renuka_dil){
  for(i in 1:nrow(ec_data)){
    ec_data$r1[i]=renuka_dil$r_dil1[which(renuka_dil$r_sample_type==ec_data$sample_type[i])]
    ec_data$r2[i]=renuka_dil$r_dil2[which(renuka_dil$r_sample_type==ec_data$sample_type[i])]
    ec_data$r3[i]=renuka_dil$r_dil3[which(renuka_dil$r_sample_type==ec_data$sample_type[i])]
  }
  
  ec_data <- ec_data %>%
    mutate(check1=ifelse(ec_data$dil1==ec_data$r1, "Good", "Bad"),
           check2=ifelse(ec_data$dil2==ec_data$r2, "Good", "Bad"),
           check3=ifelse(ec_data$dil3==as.character(ec_data$r3), "Good", "Bad"),
           any_diff=ifelse(!is.na(check3),
             ifelse(check1=="Bad" | check2=="Bad" | check3=="Bad", "Bad", "Good"),
             ifelse(check1=="Bad" | check2=="Bad", "Bad", "Good"))) %>%
    filter(any_diff != "Good")

}

ec_data_spt_check <- ec_data_spt %>%
  select(c("sample_type", "sampleid", "col_start_dt", "ec_conc", "dil1", "dil2", "dil3")) %>%
  filter(sample_type != 99) %>%
  mutate(col_start_dt = as.Date(col_start_dt))

ec_data_spt_check <- add_renuka(ec_data_spt_check, renuka_dil)


ec_data_so_check <- ec_data_so %>%
  select(c("sample_type", "sampleid", "col_start_dt", "ec_conc", "dil1", "dil2", "dil3")) %>%
  filter(sample_type != 99) %>%
  mutate(col_start_dt = as.Date(col_start_dt))

ec_data_so_check <- add_renuka(ec_data_so_check, renuka_dil)


ec_data_es_check <- ec_data_es %>%
  select(c("sample_type", "sampleid", "col_start_dt", "ec_conc", "dil1", "dil2", "dil3")) %>%
  filter(sample_type != 12 & sample_type != 99) %>%
  mutate(col_start_dt = as.Date(col_start_dt))

ec_data_es_check <- add_renuka(ec_data_es_check, renuka_dil)



write_excel_csv(ec_data_spt_check, paste0("SPT/data/checks/ec_data_spt_check", Sys.Date(), ".csv"))
write_excel_csv(ec_data_es_check, paste0("SPT/data/checks/ec_data_es_check", Sys.Date(), ".csv"))
write_excel_csv(ec_data_so_check, paste0("SPT/data/checks/ec_data_so_check", Sys.Date(), ".csv"))
