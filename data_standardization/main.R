# database file generation 
# run this file to execute all files sources below at once

version <- 29
# source separate programs
source(paste0("ds_col_v", version, ".R"))
source(paste0("ds_lab_v", version, ".R"))
source(paste0("ds_community_v", version, ".R"))
source(paste0("ds_household_v", version, ".R"))
source(paste0("ds_school_v", version, ".R"))

