
#### Gender analysis ####
library(tidyverse)
library(jsonlite)
library(ggh4x)
source('main.R')

ss <- read.csv("data_standardization/data/s_merged_2020-02-10.csv", stringsAsFactors = F)
ss <- ss %>% filter(city == "Dakar")
hh <- read.csv("data_standardization/data/h_merged_2020-02-10.csv", stringsAsFactors = F)
hh <- hh %>% filter(city == "Dakar")
for(i in 1:nrow(hh)){
  if(hh$h_neighborhood[i]==409){
    hh$h_neighborhood[i]=1
  }
  else if(hh$h_neighborhood[i]==410){
    hh$h_neighborhood[i]=2
  }
  else if(hh$h_neighborhood[i]==411){
    hh$h_neighborhood[i]=3
  }
  else if(hh$h_neighborhood[i]==412){
    hh$h_neighborhood[i]=4
  }
  else if(hh$h_neighborhood[i]==413){
    hh$h_neighborhood[i]=5
  }
}
cc <- read.csv("data_standardization/data/c_merged_2020-02-10.csv", stringsAsFactors = F)
cc <- cc %>% filter(city == "Dakar")


col <- read.csv("data_standardization/data/col_merged_2020-02-12.csv", stringsAsFactors = F)
col <- col %>% filter(city == "Dakar")
lab <- read.csv("data_standardization/data/lab_merged_2020-02-12.csv", stringsAsFactors = F)
lab <- lab %>% filter(dply_num == 12)

#0 = all male, 1= all female
ss_m <- ss %>% filter(s_participant_gender==0)
ss_f <- ss %>% filter(s_participant_gender==1)

# hh_m <- 
# hh_f <- 

cc_m <- cc %>% filter(c_participant_gender==0)
cc_f <- cc %>% filter(c_participant_gender==1)

pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                     'pa' = 8, 'bw' = 9, 'sf' = 10)
pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                     's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                     'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food', 'odw' = 'Other Drinking Water')
neighbs = list(
  'WakhinaneNimzatt' = 1,
  'MedinaGounass' = 2,
  'DTK' = 3,
  'RufisqueEst' = 4,
  'SicapLiberte' = 5)

hh <- hh[0,]


freq_m <- compute_frequencies(hh, ss_m, cc_m, type='ppl',
                            analysis_type = 'combined',
                            config=config,
                            pathway_labels = pathway_labels,
                            pathway_codes = pathway_codes,
                            neighborhood_mapping = neighbs)
conc <- compute_concentrations(col, lab,
                               config=config,
                               pathway_codes = config$pathway_codes,
                               pathway_labels = config$pathway_labels,
                               neighborhood_mapping = neighbs,
                               lab_MF = T)
ps_freq_m <- compute_exposure(freq_m, conc, neighbs, config=config,
                            pathway_codes = pathway_codes,
                            pathway_labels = pathway_labels,
                            neighborhood_mapping = neighbs,
                            parallel = F)


save(ps_freq_m, file="Dakar/ps_freq_m.RDA")

freq_f <- compute_frequencies(hh, ss_f, cc_f, type='ppl',
                            analysis_type = 'combined',
                            config=config,
                            pathway_labels = pathway_labels,
                            pathway_codes = pathway_codes,
                            neighborhood_mapping = neighbs)

ps_freq_f <- compute_exposure(freq_f, conc, neighbs, config=config,
                                pathway_codes = pathway_codes,
                                pathway_labels = pathway_labels,
                                neighborhood_mapping = neighbs,
                                parallel = F)


save(ps_freq_f, file="Dakar/ps_freq_f.RDA")

load("Dakar/ps_freq_f.RDA")
load("Dakar/ps_freq_m.RDA")

library(plyr)
male <- ldply(ps_freq_m, data.frame) %>% select(-c("data"))
male <- distinct(male)
male$gender <- "male"

female <- ldply(ps_freq_f, data.frame) %>% select(-c("data"))
female <- distinct(female)
female$gender <- "female"
detach("package:plyr", unload = TRUE)




mf <- rbind(male, female)



###################################################################
#### Start Here ####
library(treemap)
library(treemapify)
library(gtable)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(readxl)
library(ggh4x)
library(plyr)


load("Dakar/ps_freq_f.RDA")
load("Dakar/ps_freq_m.RDA")

library(plyr)
male <- ldply(ps_freq_m, data.frame) %>% select(-c("data"))
male <- distinct(male)
male$gender <- "male"

female <- ldply(ps_freq_f, data.frame) %>% select(-c("data"))
female <- distinct(female)
female$gender <- "female"
detach("package:plyr", unload = TRUE)




mf <- rbind(male, female)




ff.multi <- mf


#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi$citylabel <- "Dakar"

df.multi <- df.multi %>% 
  mutate(sample = factor(.$sample, levels = factor(unique(sample))))
df.multi$sample

df.multi$dose[which(df.multi$neighborhood=="RufisqueEst" & df.multi$sample=="Drain Water")] <- 0

df.multi$Dose <- 10^(df.multi$dose)


asdf <- df.multi %>%
  group_by(neighborhood, gender, age) %>%
  mutate(sum = sum(Dose)) %>%
  mutate(perc = (Dose / sum) * 100)




{
  asdf %>% filter(age == "Adults") %>% {
    ggplot(., aes(area = Dose,
                  fill = log10(Dose),
                  label = paste(sample, "\n", paste0(round(perc, 0), "%"), "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
      scale_fill_gradient(
        low = "yellow", high = "darkred",
        breaks = log10(range(.$Dose)),
        labels = round(range(log10(.$Dose)))
      ) + theme_bw() +
      facet_nested(gender~neighborhood) +
      theme(legend.position = "none") +
      labs(fill = "Exposure",
           title = "Total Exposure in Dakar, Senegal",
           subtitle = "Adults") 
  } -> plot.adults
  
  asdf %>% filter(age == "Children") %>% {
    ggplot(., aes(area = Dose,
                  fill = log10(Dose),
                  label = paste(sample, "\n", paste0(round(perc, 0), "%"), "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
      scale_fill_gradient(
        low = "yellow", high = "darkred",#,
        breaks = log10(range(.$Dose)),
        labels = round(range(log10(.$Dose)),0)
      ) + theme_bw() +
      facet_nested(gender~neighborhood) +
      theme(legend.position = "bottom") +
      labs(fill = "Exposure",
           title = "Total Exposure in Dakar, Senegal",
           subtitle = "Children") 
  } -> plot.children
  
  legend <- gtable_filter(ggplot_gtable(ggplot_build(plot.children)), "guide-box")
  
  grid.arrange(plot.adults, plot.children + theme(legend.position = "none"), legend,
               nrow = 3,
               heights = c(1.1, 1.1, 0.5))
  
}


# save
plot <- arrangeGrob(plot.adults, plot.children + theme(legend.position = "none"), legend,
                    nrow = 3,
                    heights = c(1.1, 1.1, 0.3)
)


ggsave(plot = plot, paste0("Dakar/treeplot_gender", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")
