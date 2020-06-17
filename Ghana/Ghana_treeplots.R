# script to create tree maps
# 9/26/19
library(treemap)
library(treemapify)
library(gtable)
library(gridExtra)
library(tidyverse)



df.exposure %>%
        filter(city == "Accra") %>%
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
                             title = "Total Exposure in Accra, Ghana",
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
                             title = "Total Exposure in Accra, Ghana",
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

ggsave(plot = plot, paste0("~/Desktop/SaniPath/Ghana/Accra_exp_", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")

df.exposure %>%
        filter(city == "Kumasi") %>%
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
                             title = "Total Exposure in Kumasi, Ghana",
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
                             title = "Total Exposure in Kumasi, Ghana",
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

ggsave(plot = plot, paste0("~/Desktop/SaniPath/Ghana/Kumasi_exp_", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")


