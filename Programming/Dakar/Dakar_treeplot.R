# script to create tree maps
# March 16, 2020
library(treemap)
library(treemapify)
library(gtable)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(readxl)


# read data
setwd("/Users/caseysiesel/Desktop/SaniPath/Programming/Dakar")
ff.multi <- read.csv(paste0(getwd(), "/expo_Dakar.csv"), stringsAsFactors = F)


#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi <- plyr::arrange(ff.multi, date)
df.multi$citylabel <- "Dakar"
df.multi$citylabel <-  factor(df.multi$citylabel, levels = unique(df.multi$citylabel))

df.multi <- df.multi %>% 
  mutate(sample = factor(.$sample, levels = factor(unique(sample))))
df.multi$sample


# data used df.multi

# ***********************************************************************************************
# indiviual graphs ----
# ***********************************************************************************************

# df.multi <- subset(df.multi, neighborhood=="Central")

df.multi %>%
  group_by(neighborhood, age) %>%
  mutate(sum = sum(Dose)) %>%
  mutate(perc = (Dose / sum) * 100) -> df.kampala



#THIS IS REMOVING SF from DTK neighborhood!!!!!!
df.kampala$Dose[which(df.kampala$neighborhood=="DTK" & df.kampala$sample=="Street Food")] <- 0

{
  df.kampala %>% filter(age == "Adults") %>% {
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
      facet_wrap(~neighborhood, nrow = 1) +
      theme(legend.position = "none") +
      labs(fill = "Exposure",
           title = "Total Exposure in Dakar, Senegal",
           subtitle = "Adults") 
  } -> plot.adults
  
  df.kampala %>% filter(age == "Children") %>% {
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
      facet_wrap(~neighborhood, nrow = 1) +
      theme(legend.position = "bottom") +
      labs(fill = "Exposure",
           title = "Total Exposure in Dakar, Senegal",
           subtitle = "Children") 
  } -> plot.children
  
  legend <- gtable_filter(ggplot_gtable(ggplot_build(plot.children)), "guide-box")
  
  grid.arrange(plot.adults, plot.children + theme(legend.position = "none"), legend,
               nrow = 3,
               heights = c(1.1, 1.1, 0.5)
  )
}


# save
plot <- arrangeGrob(plot.adults, plot.children + theme(legend.position = "none"), legend,
                    nrow = 3,
                    heights = c(1.1, 1.1, 0.3)
)

ggsave(plot = plot, paste0("exposure_treemap_dakar_NOSF", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")

