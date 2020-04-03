# ec calc 

source("analysis/ec_calculator_helper.R")
source("helper_dataload_all.R")

pathway_codes$odw <- 33
pathway_labels$odw <- "Other Drinking Water"
# setup for calculations
ec_data1 <- data.frame()


# for (i in meta_dply$id) {
for (i in 1:13) {
        
        deply_num <- i
        
        lab <- df.lab %>% filter(dply_num == deply_num)
        col <- df.col %>% filter(dply_num == deply_num)
        
        # get negihborhood names
        meta_neighb1 <- meta_neighb %>% filter(deployment_id == deply_num) %>% select(neighborhood, neighborhood_id)
        neighborhood_mapping <- as.list(meta_neighb1$neighborhood_id)
        names(neighborhood_mapping) <- meta_neighb1$neighborhood
        
        # get lab method
        lab_MF1 <- as.logical(meta_dply$lab_membrane[meta_dply$id == deply_num])
        
        # calculation
        conc_data <- compute_concentrations(col, lab, 
                                            config = config,
                                            pathway_codes = pathway_codes,
                                            pathway_labels = pathway_labels,
                                            neighborhood_mapping = neighborhood_mapping,
                                            lab_MF = lab_MF1)
        
        conc_data$mf <- lab_MF1
        conc_data <- select(conc_data, "UID" = lab_UID, "dply_num" = dply_num.y, lab_date, sample_type, col_sample_type_alt,
                            sampleid, mf, "neighborhood" = neighbor, neighb_UID, lab_1_ecoli, lab_2_ecoli, lab_3_ecoli,
                            ec_dil1, ec_dil2, ec_dil3, ec_ecnt1, ec_ecnt2, ec_ecnt3, count1, count2, dil1, dil2, ec_conc)
        
        ec_data1 <- bind_rows(ec_data1, conc_data)
        # ec_data <- rbind(ec_data, conc_data)
        
}

# mean(conc_data$ec_conc[conc_data$sample_type == 7], na.rm = T)
# log10(mean(conc_data$ec_conc[conc_data$sample_type == 7], na.rm = T))
# 
# conc_data$ec_conc <- case_when(conc_data$ec_conc == 2000000 ~ 0,
#                                TRUE ~ conc_data$ec_conc)
# 
# hist(log10(conc_data$ec_conc[conc_data$sample_type == 7 & conc_data$neighb_UID == 1101]))
# hist(log10(conc_data$ec_conc[conc_data$sample_type == 7 ]))

# log10(ec_data1$ec_conc)
# sum(is.na(ec_data1$ec_conc))


write.csv(ec_data1, paste0(getwd(), "/data/", "ec_data_", Sys.Date(), ".csv"), row.names = F, na="")



# # # data check
# ec_data1 %>% filter(dply_num == 8, neighborhood == 2, sample_type == 7) %>%
        # .$ec_conc %>% na.omit(.) %>% log10() %>%
        # hist(., breaks=seq(-2,10,by=1), freq=FALSE, ylim = c(0,1))




















