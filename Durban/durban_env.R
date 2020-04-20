
sapply(grep('.R$', list.files('model', full.names = T), value=T), source)
library(readr)
library(flexdashboard)
library(DT)
library(ggplot2)
library(ggcal)
library(readxl)
library(dplyr)
library(bindr)
library(plotly)
library(expss)
library(parallel)
library(knitr)
library(kableExtra)
library(cowplot)
library(purrr)
library(leaflet)
library(ggridges)
configure <- yaml::yaml.load_file('configure.yaml')
configure$mpn_tbl <- read_csv('rsrc/mpn_tbl.csv') %>% as.matrix()
configure$intake <- read_csv('rsrc/intake.csv') %>% as.matrix()
options(scipen=999999999)



col <- read_excel("~/Downloads/Sample_Survey-xlsform_spdurban_final_2020_04_02_18_27_30.xlsx")
lab <- read_excel("~/Downloads/IDEXX_Lab-xlsform_spdurban_final_2020_04_02_18_31_28.xlsx")


pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                     'pa' = 8, 'bw' = 9, 'sf' = 10)
pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                     's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                     'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food')
neighbs = list("Quarry Road" = 421, "Point Precinct"=422, "Hammonds Farm" =423, "Mzinyathi"=424)

##### creating SPT e. coli results and conc_data for plotting ####
df.ecdata <- create_ecData(collection_data=col, lab_data=lab, mpn_tbl=configure$mpn_tbl, reading=configure$idexx_reading,
                         value=configure$idexx_value, denoms=configure$denoms, MF=F)


df.ecdata$std_ec_conc <- 0

#change group_by(sample_type, 2ND VARIABLE HERE) depending on what is used to standardize (city right now)

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

df.ecdata$sample_type <- factor(df.ecdata$sample_type, levels=c("1", "2", "3",
                                                  "4", "5", "6", "7", "8",
                                                  "9","10"), labels=c("1"="Open Drains", "2"="Raw Produce", "3"="Drinking Water",
                                                                      "4"="Ocean Water", "5"="Surface Water", "6"="Floodwater",
                                                                      "7"="Public Latrines", "8"="Soil",
                                                                      "9"="Bathing Water","10"="Street Food"))

df <- df.ecdata

colnames(df)[colnames(df)=="_col_location_latitude"] <- "lat"
colnames(df)[colnames(df)=="_col_location_longitude"] <- "lon"

df <- df %>% filter_all(all_vars(!is.na(std_ec_conc)))

#Setting standard cutoffs
cutoff <- c(0, .25, .50, 0.75, 1)
df <- df %>% mutate(dot_label = case_when(std_ec_conc <= cutoff[[2]] ~ 1,
                                          std_ec_conc > cutoff[[2]] & std_ec_conc <= cutoff[[3]] ~ 2,
                                          std_ec_conc > cutoff[[3]] & std_ec_conc <= cutoff[[4]] ~ 3,
                                          std_ec_conc > cutoff[[4]] & std_ec_conc <= cutoff[[5]] ~ 4))
df <- df %>% mutate(dot_color = case_when(dot_label == 1 ~ "green",
                                          dot_label == 2 ~ "yellow",
                                          dot_label == 3 ~ "orange",
                                          dot_label == 4 ~ "red"))
cutoff1 <- c("Minimal", "Low", "Medium", "High")
# cutoff1 <- sprintf("%.2f", round(cutoff1, 2))
color1 <- c("green", "yellow", "orange", "red")


# df$sample_type <- factor(df$sample_type, levels=c("1", "2", "3",
#                                                   "4", "5", "6", "7", "8",
#                                                   "9","10"), labels=c("1"="Open Drains", "2"="Raw Produce", "3"="Drinking Water",
#                                                                       "4"="Ocean Water", "5"="Surface Water", "6"="Floodwater",
#                                                                       "7"="Public Latrines", "8"="Soil",
#                                                                       "9"="Bathing Water","10"="Street Food"))
# 

leaflet() %>% 
  addTiles() %>%
  # addProviderTiles(providers$MtbMap) %>%
  # addProviderTiles(providers$Stamen.TonerLines,
  #                  options = providerTileOptions(opacity = 0.35)) %>%
  # addProviderTiles(providers$Stamen.TonerLabels) %>%
  # addCircleMarkers(data=df11, lng = ~long,
  #                  lat = ~lat, 
  #                  label = paste0(df11$neighborhood, ", ", df11$city, " (", df11$country, ")"),
  #                  popup = ~deployment,
  #                  options = markerOptions(draggable = F, riseOnHover = TRUE),
  #                  color = ~factpal(deployment)) %>%
  addCircles(data=df, lng=~lon, lat=~lat,
             popup = paste0(df$sample_type, ", ", sprintf("%.2f", round(df$std_ec_conc, 2)), " Normalized E.coli", " (log10)"),
             weight = 3, radius=5, color=~dot_color, stroke = TRUE, fillOpacity = 1) %>%
  addLegend("bottomright", colors = color1, labels = cutoff1,
            title = "Fecal Contamination Risk Scale") %>%
  addScaleBar("bottomleft")



df.ecdata$neighbor <- factor(df.ecdata$neighbor, labels=c("421"="Quarry Road", "422"="Point Precinct",
                                                          "423"="Hammonds Farm", "424"= "Mzinyathi"))


df2 <- df.ecdata %>%
  group_by(sample_type) %>%
  summarise(mean.cont = mean(log10(ec_conc), na.rm=TRUE))

# df2$sample_type <- factor(df2$sample_type, levels=c("1", "2", "3",
#                                                   "4", "5", "6", "7", "8",
#                                                   "9","10"), labels=c("1"="Open Drains", "2"="Raw Produce", "3"="Drinking Water",
#                                                                       "4"="Ocean Water", "5"="Surface Water", "6"="Floodwater",
#                                                                       "7"="Public Latrines", "8"="Soil",
#                                                                       "9"="Bathing Water","10"="Street Food"))
# 


df.ecdata %>%
  ggplot(., aes(y=factor(neighbor, levels=(unique(neighbor))), x=log10(ec_conc))) +
  geom_density_ridges(aes(fill=neighbor), quantile_lines=TRUE, quantiles=2, panel_scaling=FALSE
                      #comment this chunk out to get rid of lines: from here
                      ,
                      jittered_points = TRUE,
                      # position = position_points_jitter(width = 0.05, height = 0),
                      # point_shape = '|',
                      point_size = 0.75,
                      point_alpha = 0.5,
                      alpha = 0.7
                      #to here
  ) +
  geom_vline(data=df2, aes(xintercept=mean.cont, color="red"), show.legend=FALSE) +
  # geom_point(aes(color=city) ) +
  # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
  facet_wrap( ~ sample_type, scales = "fixed", nrow=4, ncol=3) +
  labs(fill = "City",
       x = "E. coli (Log10)",
       y = "") +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_ridges() +
  theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=8),
        strip.background = element_rect(fill="white"),
        legend.position="bottom",
        legend.justification="center")
