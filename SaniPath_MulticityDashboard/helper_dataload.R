# data load
library(readxl)

# load
meta_dply <- read.csv( "data/meta_deployments.csv", stringsAsFactors = F)
meta_neighb <- read.csv( "data/meta_neighborhoods.csv", stringsAsFactors = F)
meta_sampleID <- read.csv( "data/meta_sampleID.csv", stringsAsFactors = F)

df.behav <- read.csv( "data/behavior_all_city_percent_09232019.csv", stringsAsFactors = F)
df.ecdata <- read.csv( "data/ec_data_2019-09-10.csv", stringsAsFactors = F)
df.col <- read.csv( "data/col_merged_2019-09-11.csv", stringsAsFactors = F)
df.exposure <- read.csv("data/multicity_exposure_2019-09-16.csv", stringsAsFactors = F)

# **************************************************************************************************
# modify data
colnames(df.behav) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")
df.behav$sum <- NULL
df.behav$pop <- factor(df.behav$pop)
df.behav[ is.na(df.behav) ] <- NA

# df.behav[ is.na(df.behav) ] <- NA
# df.behav <- df.behav[complete.cases(df.behav), ]
# df.behav <- df.behav %>% mutate(citylabel = factor(.$citylabel, levels = factor(meta_dply$citylabel)))

# **************************************************************************************************
# rearranging order and formatting date
meta_dply$date <- as.Date(meta_dply$date, format="%m/%d/%Y")
meta_dply <- meta_dply %>% arrange((date))
meta_dply <- meta_dply %>% mutate(citylabel = factor(.$citylabel, levels = factor(meta_dply$citylabel)))
meta_dply <- meta_dply %>% mutate(country = factor(.$country, levels = factor(unique(meta_dply$country))))

# # input selector for deployment profile
# dplyinput <- as.numeric(unlist(meta_dply[,1]))
# names(dplyinput) <- as.character(unlist(meta_dply[,5]))

# **************************************************************************************************
meta_sampleID <- meta_sampleID %>% 
        mutate(sample_type_name = factor(.$sample_type_name, levels = factor(meta_sampleID$sample_type_name)))

# **************************************************************************************************
df.ecdata <- df.ecdata %>% 
        left_join(., meta_sampleID, by = c("sample_type" = "id")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("dply_num" = "id"))

# df.ecdata$sample_type[df.ecdata$sample_type == 3 & df.ecdata$col_sample_type_alt != "Drinking Water" 
                      # & df.ecdata$col_sample_type_alt != "" ] <- 33

# **************************************************************************************************
# df.exposure <- left_join(df.exposure, meta_dply[c("citylabel", "date")], by = "citylabel")
# df.exposure <- plyr::arrange(df.exposure, date)
# reorder levels
# df.exposure$citylabel <-  factor(df.exposure$citylabel, levels = unique(df.exposure$citylabel))

# table(df.exposure$pathway)

# df.exposure$pathway[df.exposure$pathway == "drain"] <- "Open Drains" #1 
# df.exposure$pathway[df.exposure$pathway == "produce"] <- "Raw Produce" #2
# df.exposure$pathway[df.exposure$pathway == "municipal"] <- "Drinking Water" #3
# df.exposure$pathway[df.exposure$pathway == "ocean"] <- "Oceans" #4
# df.exposure$pathway[df.exposure$pathway == "surface"] <- "Surface Water" #5
# df.exposure$pathway[df.exposure$pathway == "flood"] <- "Floodwater" #6
# df.exposure$pathway[df.exposure$pathway == "latrine"] <- "Public Latrine" #7
# df.exposure$pathway[df.exposure$pathway == "bathing"] <- "Bathing Water" #9
# df.exposure$pathway[df.exposure$pathway == "streetfood"] <- "Street Food" #10
# df.exposure$pathway[df.exposure$pathway == "otherdrinking"] <- "DW, other"  #33
# 
# pathwayfactors <- c("Open Drains", "Raw Produce", "Drinking Water", "DW, other", "Oceans", "Surface Water",
#                     "Floodwater","Public Latrine", "Bathing Water", "Street Food")
# 
# df.exposure$pathway <-  factor(df.exposure$pathway, levels = pathwayfactors)
# 
# df.exposure$age[df.exposure$age == "a"] <- "Adults"
# df.exposure$age[df.exposure$age == "c"] <- "Children"

# table(df.ecdata$sample_type)
# table(df.ecdata$col_sample_type_alt)

# df.ecdata$col_sample_type_alt <- factor(df.ecdata$col_sample_type_alt, 
#                                         levels=levels(df.ecdata$col_sample_type_alt))
# df.ecdata$col_sample_type_alt[df.ecdata$sample_type == 3 & df.ecdata$col_sample_type_alt == ""] <- "Drinking Water"



