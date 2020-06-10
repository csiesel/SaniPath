source("~/Desktop/SaniPath/SPT/3_SPT_Merge.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

# Loading in the model files and config.yaml
sapply(grep('.R$', list.files('SPT/model', full.names = T), value=T), source)
configure <- yaml::yaml.load_file('SPT/model/configure.yaml')

#### SPT EC ####
ec_data_spt <- create_ecData(col_spt, mf, mpn_tbl = NULL, configure$membrane_reading, configure$membrane_value, configure$denoms, MF=T)

ec_data_spt <- ec_data_spt %>% filter(sample_type %in% c(1:10) | (sample_type==99 & !is.na(col_start_dt))) 
                                  


pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                     'pa' = 8, 'bw' = 9, 'sf' = 10, 'pl'=11, 'ms'=12, 'fb'=99, 'nc'=98, 'pc'=97, 'coa'=151, 'cob'=152, 'cofb'=159,
                     'fpa'=141, 'fpb'=142, 'fpc'=143, 'fpd'=144, 'fpe'=145, 'fpfb'=149)
pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                     's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                     'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food',
                     'pl' = 'Pooled Latrine', 'ms' = 'Moore Swab', 'fb' = 'Field Blank',
                     'nc' = 'Negative Control', 'pc' = 'Positive Control', 'coa'='Child Obs Floor', 
                     'cob'='Child Obs Off Grnd', 'cofb'='Child Obs FB',
                     'fpa'='Food Prep Cut/Grind Surface', 'fpb'='Food Prep Storage/Prep Bowl', 'fpc'='Food Prep Area', 
                     'fpd'='Cooking/Prep Water','fpe'='Cooking/Prep Utensils', 'fpfb'='Food Prep Field Blank')

neighbs <- list(unique(col_spt$col_neighborhood))
# conc_data_spt <- compute_concentrations(col_spt, mf, configure=configure, pathway_codes = configure$sample_type_code,
#                                     pathway_labels = pathway_labels,
#                                     neighborhood_mapping = neighbs)

# make_plots(conc_data_spt, type='hist', output_dir="SPT/plots/spt/", width=NA, height=NA, units='in', dpi=72, convert_px=T, parallel=F, nc=detectCores(), lab_MF=T, save=T, .return_plots=T)


#### ES EC ####
ec_data_es <- create_ecDataES(col_es, mf, mpn_tbl = NULL, configure$membrane_reading, configure$membrane_value, configure$denoms, MF=T)
ec_data_es$col_sw_trigger_nr <- as.character(ec_data_es$col_sw_trigger_nr)

ec_data_es <- ec_data_es %>% filter(sample_type %in% c(11,12,16) | (sample_type==99 & !is.na(col_start_dt))) 



wards <- list(unique(col_es$col_ward))
# conc_data_es <- compute_concentrationsES(col_es, mf, configure=configure, pathway_codes = configure$sample_type_code,
#                                        pathway_labels = pathway_labels,
#                                        neighborhood_mapping = wards)

# make_plots(conc_data_es, type='hist', output_dir="SPT/plots/es/", width=NA, height=NA, units='in', dpi=72, convert_px=T, parallel=F, nc=detectCores(), lab_MF=T, save=T, .return_plots=T)


#### SO EC ####
ec_data_so <- create_ecData_so(col_so, mf, mpn_tbl=NULL, configure$membrane_reading, configure$membrane_value, configure$denoms, MF=T)





#### Checking the dilution errors ####
ec_data_spt %>% select(sample_type, ec_conc, count1) %>% 
  group_by(factor(sample_type)) %>% 
  summarise(omitted = sum(is.na(ec_conc) & !is.na(count1))) -> spt_error

paste0("Dilution errors for SPT: ",spt_error$omitted)

ec_data_es %>% select(sample_type, ec_conc, count1) %>% 
  group_by(factor(sample_type)) %>% 
  summarise(omitted = sum(is.na(ec_conc) & !is.na(count1))) -> es_error

paste0("Dilution errors for ES: ",es_error$omitted)

ec_data_so %>% select(sample_type, ec_conc, count1) %>% 
  group_by(factor(sample_type)) %>% 
  summarise(omitted = sum(is.na(ec_conc) & !is.na(count1))) -> so_error

paste0("Dilution errors for SO: ",so_error$omitted)

#### Writing out CSV files of ec_data_XX ####
write_excel_csv(ec_data_spt, paste0("SPT/data/ec_data_spt_", Sys.Date(), ".csv"))
write_excel_csv(ec_data_es, paste0("SPT/data/ec_data_es_", Sys.Date(), ".csv"))
write_excel_csv(ec_data_so, paste0("SPT/data/ec_data_so_", Sys.Date(), ".csv"))
