# **********************************************************************************
# SaniPath Deployment Data Standardization
# SaniPath Cross Deployment Database Creation
# **********************************************************************************
# Deployments:
#        0 - master forms [this is the "template" for the new database]
#        1 - Cambodia deployment
#        2 - Bangladesh deployment
#        3 - Ghana deployment
#        4 - Zambia deployment
# **********************************************************************************
# Last modified: June 13, 2018
# By: Wolfgang
# **********************************************************************************

# load libraries
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)

# load meta data
meta_dply <- read.table(paste0(getwd(), "/data/meta_data/", "meta_deployments", ".csv"), 
                        sep=",", stringsAsFactors=F, header=T)

meta_neighb <- read.table(paste0(getwd(), "/data/meta_data/", "meta_neighborhoods", ".csv"), 
                          sep=",", stringsAsFactors=F, header=T)

meta_produce <- read.table(paste0(getwd(), "/data/meta_data/", "meta_produce", ".csv"), 
                           sep=",", stringsAsFactors=F, header=T)

meta_sampleID <- read.table(paste0(getwd(), "/data/meta_data/", "meta_sampleID", ".csv"), 
                            sep=",", stringsAsFactors=F, header=T)





# example
# with(df.h, table(h_l_a, dply_num))



# ghana neighborhoods
# neigh <- data.frame("h_neighborhood" = c(1:5),
#                     "h_neigh" = c("Shiabu","Chorkor","Kokomlemle","Ringway","Adabraka"),
#                     stringsAsFactors=FALSE)



# order/ structure of script-parts:
#---- DEPLOYMENT -----
##### rename variables #####
##### recode #####
##### create new variables #####
##### deployment-specific info #####
##### merge datasets #####

