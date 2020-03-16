# data load
library(readxl)
library(dplyr)

# load
meta_dply <- read.csv( "data/meta_deployments.csv", stringsAsFactors = F)
meta_neighb <- read.csv( "data/meta_neighborhoods.csv", stringsAsFactors = F)
meta_sampleID <- read.csv( "data/meta_sampleID.csv", stringsAsFactors = F)

df.behav <- read.csv( "data/behavior_all_city_percent_02122020.csv", stringsAsFactors = F) #done
df.ecdata <- read.csv( "data/ec_data_2020-02-12.csv", stringsAsFactors = F) #done
df.col <- read.csv( "data/col_merged_2020-02-12.csv", stringsAsFactors = F) #done
df.exposure <- read.csv("data/multicity_exposure_2020-03-05.csv", stringsAsFactors = F) #done

#Casey added
meta_full <- merge(meta_dply, meta_neighb, by.x="id", by.y="deployment_id")
cities <- unique(meta_dply$city)
hoods <- unique(meta_neighb$neighborhood)
cities2 <- factor(cities)
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

# Casey is doing this to standardize E. coli data (x-min/max-min)
df.ecdata$std_ec_conc <- 0
max_min <- df.ecdata %>% group_by(sample_type) %>% summarise(max=max(ec_conc, na.rm=TRUE), min=min(ec_conc, na.rm=TRUE))
for(i in 1:nrow(df.ecdata)){
  if(is.na(df.ecdata$ec_conc[i])){
    df.ecdata$std_ec_conc[i]=NA
  }
  else{
    max<-log10(max_min$max[which(max_min$sample_type==df.ecdata$sample_type[i])])
    min<-log10(max_min$min[which(max_min$sample_type==df.ecdata$sample_type[i])])
    df.ecdata$std_ec_conc[i]=(log10(df.ecdata$ec_conc[i])-min)/(max-min) 
  }
}

df.ecdata$hood <- ""
for(i in 1:nrow(df.ecdata)){
  if(is.na(df.ecdata$neighb_UID[i])){
    df.ecdata$hood[i]=""
  }
  else{
    df.ecdata$hood[i] = meta_neighb$neighborhood[which(meta_neighb$neighb_UID==df.ecdata$neighb_UID[i])]
  }
}


