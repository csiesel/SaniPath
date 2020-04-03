# **********************************************************************************
# **********************************************************************************
# exposure figures 
# bar graphs for total exposure by neighborhood and city
# for adults and children
# **********************************************************************************
# dominant pathways are the talles bar graphs
# **********************************************************************************
library(tidyverse)

source("helper_dataload_all.R")

##### Exposure Dominant Pathway Figure ####

df.multi %>%
        ggplot(., aes(x=factor(neighb_id), perDose, fill=sample_type_name) ) +
        geom_bar(stat="identity") +
        facet_grid(age ~ city, scales = "free_x", space = "free_x") +
        labs(subtitle = "Adults",
             fill = "Pathway",
             x = "Neighborhood",
             y = "Total Exposure (log10)") +
        theme_bw() +
        theme(legend.position="bottom")


colors <- c("Open Drain Water" = 'indianred3',
            "Municipal Drinking Water" = 'deepskyblue2',
            "Raw Produce" = 'mediumseagreen',
            "Floodwater" = 'mediumpurple2',
            "Bathing Water" = '#ff7f00',
            "Ocean" = '#ffff33',
            "Surface Water" = '#a65628',
            "Public Latrine" = '#f781bf',
            "Street Food" = 'grey68')


# df.multi <- df.multi %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")])

df.multi %>% filter(age == "Adults") %>% filter(neighb_id != 501 & neighb_id != 502) %>%
        mutate(city = replace(city, city == "Atlanta", "Atl"),
               city = replace(city, city == "Lusaka", "Lka")) %>%
        ggplot(., aes(x=factor(neighb_id), y=perDose, fill=sample_type_name) ) +
        geom_bar(stat="identity") +
        facet_grid(~ city, scales = "free_x", space = "free_x") +
        labs(fill = "Pathway",
             x = "Neighborhood",
             y = "Total Exposure (log10)") +
        theme_bw() +
        theme(legend.position="bottom",
              strip.text.x = element_text(size = 12),
              # strip.text.y = element_text(size = 12),
              strip.background = element_rect(fill="white"),
              legend.text=element_text(size=12)) +
        # scale_fill_brewer(palette="Dark2") 
        scale_fill_manual(values = colors) -> p

ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_adults ", Sys.Date(), ".png"), 
       plot = p, width = 12, height = 6, units="in", dpi = 300)


df.multi %>% filter(age == "Adults") %>% filter(neighb_id != 501 & neighb_id != 502) %>%
        mutate(city = replace(city, city == "Atlanta", "Atl"),
               city = replace(city, city == "Lusaka", "Lka")) %>%
        ggplot(., aes(x=factor(neighb_id), y=perDose) ) +
        geom_bar(stat="identity") +
        facet_grid(~ city, scales = "free_x", space = "free_x") +
        labs(fill = "Pathway",
             x = "Neighborhood",
             y = "Total Exposure (log10)") +
        theme_bw() +
        theme(legend.position="bottom",
              strip.text.x = element_text(size = 12),
              # strip.text.y = element_text(size = 12),
              strip.background = element_rect(fill="white")) -> p

ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_adults_grey ", Sys.Date(), ".png"), 
       plot = p, width = 12, height = 5.2, units="in", dpi = 300)

### all ----
df.multi  %>% filter(neighb_id != 501 & neighb_id != 502) %>%
        mutate(city = replace(city, city == "Atlanta", "Atl"),
               city = replace(city, city == "Lusaka", "Lusaka")) %>%
        ggplot(., aes(x=factor(neighb_id), y=perDose, fill=sample_type_name) ) +
        geom_bar(stat="identity") +
        facet_grid(age ~ city, scales = "free_x", space = "free_x") +
        labs(fill = "Pathway",
             x = "Neighborhood",
             y = "Total Exposure (log10)") +
        theme_bw() +
        theme(legend.position="bottom",
              strip.text.x = element_text(size = 12),
              # strip.text.y = element_text(size = 12),
              strip.background = element_rect(fill="white"),
              legend.text=element_text(size=12)) +
        # scale_fill_brewer(palette="Dark2") 
        scale_fill_manual(values = colors) -> p

ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_all ", Sys.Date(), ".png"), 
       plot = p, width = 12, height = 8, units="in", dpi = 300)

ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_all ", Sys.Date(), ".png"), 
       plot = p, width = 16, height = 8, units="in", dpi = 300)


### kampala ----
df.multi %>% filter(city == "Kampala") %>%
        ggplot(., aes(x=neighborhood, y=perDose, fill=sample_type_name) ) +
        geom_bar(stat="identity") +
        facet_grid(age ~ city, scales = "free_x", space = "free_x") +
        labs(fill = "Pathway",
             x = "Neighborhood",
             y = "Total Exposure (log10)") +
        theme_bw() +
        theme(legend.position="bottom",
              strip.text.x = element_text(size = 12),
              # strip.text.y = element_text(size = 12),
              strip.background = element_rect(fill="white")) +
        # scale_fill_brewer(palette="Dark2") 
        scale_fill_manual(values = colors) -> p
p

ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_city_kampala ", Sys.Date(), ".png"), 
       plot = p, width = 6, height = 7, units="in", dpi = 300)



##### Exposure Pathway Dots, Neighborhood Fig. ####
# for dashboard use
# each dot represents one neighborhood

# df.multi %>% filter(pop == "a") %>% filter(sample_id %in% c(1,2,3,6,7,9)) -> df.a
# df.multi %>% filter(pop == "c") -> df.c

df.multi %>%
        ggplot(., aes(x=log10(dose), y=exposure)) +
        # geom_point(data = transform(df.a, pathway = NULL), colour = "grey85") +
        geom_point(aes(color=city), size = 3) +
        facet_grid(sample_type_name ~ age) +
        theme_bw() +
        labs(title = "Exposure by Pathway - by City",
             color = "Deployment",
             x = "E.coli Dose (log10)",
             y = "Percent") +
        scale_y_continuous(breaks = c(0,.5,1),
                           labels = scales::percent) +
        theme(
              strip.text.y = element_text(size = 8),
              strip.background = element_rect(fill = "white"),
              legend.position = "bottom") -> p

ggsave(filename = paste0(getwd(), "/images/new_exposure/", "exposure_neighborhoods", ".png"), 
       plot = p, width = 8, height = 9, units="in", dpi = 300)


df.multi %>% filter(!sample_id %in% c(4,5)) %>%
        ggplot(., aes(x=log10(dose), y=exposure)) +
        # geom_point(data = transform(df.a, pathway = NULL), colour = "grey85") +
        geom_point(aes(color=city), size = 3) +
        facet_grid(sample_type_name ~ age) +
        theme_bw() +
        labs(title = "Exposure by Pathway - by City",
             color = "Deployment",
             x = "E.coli Dose (log10)",
             y = "Percent") +
        scale_y_continuous(breaks = c(0,.5,1),
                           labels = scales::percent) +
        theme(strip.text.x = element_text(size = 8),
              strip.text.y = element_text(size = 7),
              strip.background = element_rect(fill = "white"),
              legend.position = "bottom") -> p


ggsave(filename = paste0(getwd(), "/images/new_exposure/", "exposure_neighborhoods_selected", ".png"), 
       plot = p, width = 8, height = 7, units="in", dpi = 300)

