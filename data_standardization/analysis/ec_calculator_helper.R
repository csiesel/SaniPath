# ec calc functions


# setup

library(readr)
library(magrittr)
source("analysis/ec_helpers.R")
source("analysis/utilities.R")
# source("analysis/analysis_helpers.R")


config <- yaml::yaml.load_file("analysis/config.yaml")
config$mpn_tbl <- read_csv('analysis/mpn_tbl.csv') %>% as.matrix()
config$intake <- read_csv('analysis/intake.csv') %>% as.matrix()

# source("helper_dataload_all.R")


# lab <- df.lab
# col <- df.col
# 
# lab_data <- lab %>% filter(dply_num == 2) %>% filter(lab_sample_type == 1) 
# collection_data <- col %>% filter(dply_num == 2) %>% filter(col_sample_type == 1)
# meta_neighb1 <- meta_neighb %>% filter(deployment_id == 2) %>% select(neighborhood, neighborhood_id)

pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                            'pa' = 8, 'bw' = 9, 'sf' = 10)
pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                            's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                            'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food')

# add pathway info to config:
# config$pathway_codes = pathway_codes
# config$pathway_labels =pathway_labels

# col <- collection_data
# lab <- lab_data




###
compute_concentrations <- function(collection_data, lab_data,
                                   config = config,
                                   pathway_codes = config$pathway_codes,
                                   pathway_labels = config$pathway_labels,
                                   neighborhood_mapping = list(), 
                                   lab_MF= F) {
        if (is.null(config)) stop('Missing config object!')
        if (length(pathway_codes) < 1 | length(pathway_labels) < 1) stop('Pathway data missing!')
        if (length(neighborhood_mapping) < 1) stop('Neighborhood data map missing!')
        
        # lab_analysis_method <- unique(lab_data$lab_analysis)
        if (!lab_MF) {
                reading = config$idexx_reading
                value = config$idexx_value
                MF = F
        }
        else {
                reading = config$membrane_reading
                value = config$membrane_value
                MF = T
        }
        denoms = config$denoms
        
        # Calculate the e coli combined dataframe
        ec_data <- create_ecData(collection_data = collection_data,
                                 lab_data = lab_data,
                                 mpn_tbl = config$mpn_tbl,
                                 reading = reading,
                                 value = value,
                                 denoms = denoms,
                                 MF = MF)
        
        return(ec_data)
}


# master create_ecData
create_ecData <- function(collection_data, lab_data, mpn_tbl,
                          reading = config$idexx_reading, 
                          value = config$idexx_value,
                          denoms = config$denoms,
                          MF = F # defaults to IDEXX method
) {
        #logic to decide whether the function recieves IDEXX data or MF data;
        #This is assuming all the samples will be tested in one of the method: either IDEXX or MF.
        #This field will be filled based on configuration of the project.
        
        if (!MF) {
                # idexx specific value manipulation
                lab_data %<>% ec_prepare_idexx(reading, mpn_tbl)
        }
        else {
                lab_data %<>% ec_prepare_mf(reading, value)
        }
        
        # These steps are the same for both methods
        ec_data <- ec_merge(collection_data, lab_data)
        
        # add denominators
        ec_data %<>% ec_add_denoms(denoms)
        
        # calculate the swaps
        ec_data %<>% ec_calc_swaps()
        
        # calculate the conditions
        cond_func <- if (MF) ec_mf_conditions else ec_idexx_conditions
        
        ec_data %<>% cond_func(value)
        
        ec_data$neighbor <- as.factor(ec_data$col_neighborhood)
        
        return(ec_data)
        
        
}





