# script to create tree maps
# 9/26/19
library(treemap)
library(treemapify)
library(gtable)
library(gridExtra)
library(tidyverse)

source("helper_dataload_all.R")

# data used df.multi
# data nomenclature:
#       exposed -> percent exposed
#       dose    -> dose of exposure
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



# ***********************************************************************************************
# loop it ----
# ***********************************************************************************************

#define width of output based on this data frame:
df.width <- structure(list(city = structure(1:9, .Label = c("Accra", "Atlanta", "Dhaka", "Kampala", 
                                                            "Kumasi", "Lusaka", "Maputo", "Siem Reap", 
                                                            "Vellore"), class = "factor"), 
                           width = c(7L, 2L, 10L, 7L, 7L, 2L, 3L, 7L, 3L)), 
                      class = "data.frame", row.names = c(NA, -9L))


for (i in unique(meta_dply$city)){
        print(i)
        
        df.multi %>%
                filter(city == i) %>%
                group_by(neighborhood, age) %>%
                mutate(sum = sum(dose)) %>%
                mutate(perc = (dose / sum) * 100) -> df
        
        
        {
                df %>% filter(age == "Adults") %>% {
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
                                     title = paste0("Total Exposure in ", i),
                                     subtitle = "Adults") 
                } -> plot.adults
                
                df %>% filter(age == "Children") %>% {
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
                                     title = paste0("Total Exposure in ", i),
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
        
        # width (was 7 before, should be a minimum of 3, max of ...)
        # width <- 1 * sum(meta_dply$n_neighb[meta_dply$city == i])
        width <- df.width$width[df.width$city == i]
        
        ggsave(plot = plot, paste0(getwd(), "/images/treemaps/", "exposure_treemap_", i, " ", Sys.Date(), ".png"), 
               dpi = 300, width = width, height = 6, units = "in")
        
        
}
