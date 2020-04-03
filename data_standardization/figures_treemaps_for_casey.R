# script to create tree maps
# 9/26/19
library(treemap)
library(treemapify)
library(gtable)
library(gridExtra)
library(tidyverse)


# read data
ff.multi <- read.csv(paste0(getwd(), "/data/", "multicity_exposure_2019-09-30.csv"), stringsAsFactors = F)

df.multi <- plyr::arrange(df.multi, date)
df.multi$citylabel <-  factor(df.multi$citylabel, levels = unique(df.multi$citylabel))

df.multi <- df.multi %>% 
        mutate(sample_type_name = factor(.$sample_type_name, levels = factor(meta_sampleID$sample_type_name)))
df.multi$sample_type_name


# data used df.multi

# ***********************************************************************************************
# indiviual graphs ----
# ***********************************************************************************************

df.multi %>%
        filter(city == "Kampala") %>%
        group_by(neighborhood, age) %>%
        mutate(sum = sum(dose)) %>%
        mutate(perc = (dose / sum) * 100) -> df.kampala


{
        df.kampala %>% filter(age == "Adults") %>% {
                ggplot(., aes(area = dose,
                              fill = log10(dose),
                              label = paste(pathway, "\n", paste0(round(perc, 0), "%"), "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
                        scale_fill_gradient(
                                low = "yellow", high = "darkred",
                                breaks = log10(range(.$dose)),
                                labels = round(range(log10(.$dose)),)
                        ) + theme_bw() +
                        facet_wrap(~neighborhood, nrow = 1) +
                        theme(legend.position = "none") +
                        labs(fill = "Exposure",
                             title = "Total Exposure in Kampala, Uganda",
                             subtitle = "Adults") 
        } -> plot.adults
        
        df.kampala %>% filter(age == "Children") %>% {
                ggplot(., aes(area = dose,
                              fill = log10(dose),
                              label = paste(pathway, "\n", paste0(round(perc, 0), "%"), "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
                        scale_fill_gradient(
                                low = "yellow", high = "darkred",#,
                                breaks = log10(range(.$dose)),
                                labels = round(range(log10(.$dose)),0)
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

ggsave(plot = plot, paste0("exposure_treemap_kampala_", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")

