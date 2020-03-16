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
df.hh <- read.csv( "data/h_merged_2020-02-10.csv", stringsAsFactors = F)
df.sc <- read.csv( "data/s_merged_2020-02-10.csv", stringsAsFactors = F)
df.cc <- read.csv( "data/c_merged_2020-02-10.csv", stringsAsFactors = F)
meta_full <- merge(meta_dply, meta_neighb, by.x="id", by.y="deployment_id")
cities <- unique(meta_dply$city)
hoods <- unique(meta_neighb$neighborhood)
colourCount = length(unique(meta_dply$city))
getPalette = colorRampPalette(brewer.pal(9, "Set3"))
colScale <- scale_fill_manual(values=getPalette(colourCount))


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



# **************************************************************************************************
# determining dominant pathways
df.exposure$citylabel <-  factor(df.exposure$citylabel, levels = unique(df.exposure$citylabel))

df.exposure <- df.exposure %>% 
  mutate(sample_type_name = factor(.$sample_type_name, levels = factor(unique(sample_type_name))))
df.exposure <- df.exposure %>%
  group_by(neighborhood, age) %>%
  mutate(sum = sum(dose)) %>%
  mutate(perc = (dose / sum) * 100) %>%
  mutate(logexp = log10(popDose)) %>%
  mutate(dominantcount=0) %>%
  mutate(dominant="")

multiFinal = list()
sites <- unique(df.exposure$citylabel)
neighborhoods <- unique(df.exposure$neighborhood)


for(q in 1:length(neighborhoods)){
  tempmulti1 <- filter(df.exposure, neighborhood == neighborhoods[q], pop=='a')
  tempmulti2 <- filter(df.exposure, neighborhood == neighborhoods[q], pop=='c')
  for(a in 1:nrow(tempmulti1)){
    maxExpA = max(tempmulti1$logexp)
    botRangeA = maxExpA - 1
    if(tempmulti1$logexp[a]>= botRangeA){
      tempmulti1$dominant[a] = "Yes"
      tempmulti1$dominantcount[a] = 1
    }
    else{
      tempmulti1$dominant[a] = "No"
    }
  }
  
  for(c in 1:nrow(tempmulti2)){
    maxExpC = max(tempmulti2$logexp)
    botRangeC = maxExpC - 1
    if(tempmulti2$logexp[c]>= botRangeC){
      tempmulti2$dominant[c] = "Yes"
      tempmulti2$dominantcount[c] = 1
    }
    else{
      tempmulti2$dominant[c] = "No"
    }
  }
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti1
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti2
}

df.dominant = do.call(rbind, multiFinal)

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



