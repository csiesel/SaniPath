# script to create tree maps
# 9/26/19
library(treemap)
library(treemapify)
library(gtable)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(readxl)


# read data
ff.multi <- read_excel("Kampala_Exposure_data_18Feb2020.xlsx")
ff.multi$perExposed = ff.multi$perExposed/100
ff.multi$Dose = 10^ff.multi$Dose


#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi <- plyr::arrange(ff.multi, date)
df.multi$citylabel <- "Kampala"
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
        labels = round(range(log10(.$Dose)),)
      ) + theme_bw() +
      facet_wrap(~neighborhood, nrow = 1) +
      theme(legend.position = "none") +
      labs(fill = "Exposure",
           title = "Total Exposure in Kampala, Uganda",
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
           title = "Total Exposure in Kampala, Uganda",
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

ggsave(plot = plot, paste0("exposure_treemap_kampala_noremoval", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")

