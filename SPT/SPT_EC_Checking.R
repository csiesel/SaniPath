library(tidyverse)
library(readxl)
path <- "/Users/caseysiesel/Desktop/SaniPath/"
ec_data_es <- read_csv(paste0(path, "SPT/data/ec_data_es_2020-05-18.csv"))
ec_data_es <- ec_data_es[c(1:3, 67, 102:109)]

ec_data_so <- read_csv(paste0(path, "SPT/data/ec_data_so_2020-05-18.csv"))
ec_data_so <- ec_data_so[c(2, 162 ,169 ,198 ,205 ,206 ,207, 208 ,209 ,210, 211 ,212)]

ec_data_spt <- read_csv(paste0(path, "SPT/data/ec_data_spt_2020-05-18.csv"))
ec_data_spt <- ec_data_spt[c(1 ,  2  , 3 ,105, 140, 141, 142 ,143, 144, 145, 146 ,147)]

renuka_spt <- read_xlsx(paste0(path, "SPT/data/checks/renuka_ec.xlsx"), sheet="SPT")
renuka_spt <- renuka_spt %>%
  mutate(sampleid=toupper(`Sample ID:`), ecoli=`(CFU/ml)`) %>%
  select(c("sampleid", "ecoli"))

renuka_pl <- read_xlsx(paste0(path, "SPT/data/checks/renuka_ec.xlsx"), sheet="PL")
renuka_pl <- renuka_pl %>%
  mutate(sampleid=toupper(`Sample ID:`), ecoli=`CFU/ml`) %>%
  select(c("sampleid", "ecoli"))

renuka_plc <- read_xlsx(paste0(path, "SPT/data/checks/renuka_ec.xlsx"), sheet="PLc")
renuka_plc <- renuka_plc %>%
  mutate(sampleid=toupper(`Sample ID:`), ecoli=`CFU/ml`) %>%
  select(c("sampleid", "ecoli"))

renuka_uf <- read_xlsx(paste0(path, "SPT/data/checks/renuka_ec.xlsx"), sheet="UF")


ec_data_es_pl <- merge(ec_data_es, renuka_pl, by.x="sampleid", by.y="sampleid", all.x=TRUE)
ec_data_es_pl_plc <- merge(ec_data_es_pl, renuka_plc, by.x="sampleid", by.y="sampleid", all.x=TRUE)
ec_data_es_eccheck <- merge(ec_data_es_pl_plc, renuka_uf, by.x="sampleid", by.y="Sample ID:", all.x=TRUE)

ec_data_es_eccheck <- ec_data_es_eccheck %>%
  filter(sample_type != 12 & sample_type != 99) %>%
  mutate(ec=ifelse(!is.na(ecoli.x), ecoli.x,
                   ifelse(!is.na(ecoli.y), ecoli.y,
                          ifelse(!is.na(`CFU/ml`), `CFU/ml`, NA)))) %>%
  select(-c("ecoli.y", "ecoli.x", "CFU/ml"))

ec_data_es_eccheck$ec[which(ec_data_es_eccheck$ec=="ND")] <- NA

ec_data_es_eccheck <- ec_data_es_eccheck %>%
  mutate(renuka_ec=ifelse(!is.na(ec), as.numeric(ec)*ec_denom, NA)) %>%
  mutate(log_ec=log10(ec_conc),
         log_renuka_ec=log10(renuka_ec)) %>%
  select(-c("ec"))
ec_data_es_eccheck$log_renuka_ec[which(ec_data_es_eccheck$log_renuka_ec==-Inf)] <- NA


ec_data_spt_eccheck <- merge(ec_data_spt, renuka_spt, by.x="sampleid", by.y="sampleid", all.x=TRUE)
ec_data_spt_eccheck <- ec_data_spt_eccheck %>%
  filter(sample_type != 99) %>%
  mutate(renuka_ec=ifelse(!is.na(ecoli), as.numeric(ecoli)*ec_denom, NA)) %>%
  mutate(log_ec=log10(ec_conc),
         log_renuka_ec=log10(renuka_ec)) %>%
  select(-c("ecoli"))
ec_data_spt_eccheck$log_renuka_ec[which(ec_data_spt_eccheck$log_renuka_ec==-Inf)] <- NA

write_excel_csv(ec_data_es_eccheck, paste0("SPT/data/checks/ec_data_es_eccheck", Sys.Date(), ".csv"))
write_excel_csv(ec_data_spt_eccheck, paste0("SPT/data/checks/ec_data_spt_eccheck", Sys.Date(), ".csv"))

