# calculate exposure and dose data
# getting data ready for people plot



library(gridExtra)
library(grid)

# load ----
load("data/expo0911.rda")
dat.expo0 <- dat.expo %>% filter(neighb < 900 | neighb >906)

load("data/expo0217.rda")
dat.expo00 <- bind_rows(dat.expo0, dat.expo) 
dat.expo <- dat.expo00

# load("data/res_bw_0217.rda")



tail(colnames(dat.expo))

# dose ----
# arithmetic mean of nonzero
dat.expo$dose <- apply(dat.expo[, c(16:ncol(dat.expo))], 1, function(x) mean(x, na.rm = T))
# dat.expo$logdose <- log10(dat.expo$dose)


# exposure ----
dat.expo[dat.expo == 0] <- NA
# count NAs
dat.expo$na_count <- apply(dat.expo, 1, function(x) sum(is.na(x)))

# nonzero numbers / 1000
dat.expo$exposure <- (1000 - dat.expo$na_count) / 1000
# dat.expo$percent <- round((dat.expo$exposure) * 100, 0)

dat.expo$sample[dat.expo$s == "d"] <- "Open Drain Water"
dat.expo$sample[dat.expo$s == "dw"] <- "Municipal Drinking Water"

dat.expo <- dat.expo %>% left_join(., meta_sampleID, by = c("s" = "sample_type_var"))
        
dat.expo1 <- dat.expo %>% select(sample_type_name, "sample_id" = id, "neighb_id" = neighb, neighborhood, age, pop, p, r, 
                                 n.response, mu, sigma, n.sample, exposure, dose, popDose)

dat.expo1 <- dat.expo1 %>% left_join(., meta_neighb[c("neighb_UID", "deployment_id")], by = c("neighb_id" = "neighb_UID")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel", "date")], by = c("deployment_id" = "id"))

dat.expo1$pathway <- dat.expo1$sample_type_name

# steps
dat.expo1$neighb_pop <- paste(dat.expo1$city, dat.expo1$neighb_id, dat.expo1$age)
dat.expo1$neighb_pop_label <- paste(dat.expo1$city, dat.expo1$neighb_id)

# dat.expo1 <- read.csv(paste0(getwd(), "/data/", "multicity_exposure_2019-09-30.csv"))
#get total dose value for each of the city/neighborhood
#get percentage for each of the dose values
dat.expo11 <- dat.expo1 %>% group_by(neighb_pop) %>% mutate(perDose = log10(sum(popDose)),
                                                           perDose = popDose/sum(popDose)*log10(sum(popDose)) )



write.csv(dat.expo11, paste0(getwd(), "/data/", "multicity_exposure_", Sys.Date(), ".csv"), row.names = F, na="")

# graphs ----
# dat.expo1 %>% filter(pop == "a") %>%
#         ggplot(., aes(x=factor(neighb_id), perDose, fill=sample_type_name) ) +
#         geom_bar(stat="identity") +
#         facet_grid(~ city, scales = "free_x", space = "free_x") +
#         ylim(-0.5, 12) +
#         labs(subtitle = "Adults",
#              fill = "Sample Type",
#              x = "Neighborhood",
#              y = "Total Exposure (log10)") +
#         theme_bw() +
#         theme(legend.position="none") -> plot.adults
# 
# dat.expo1 %>% filter(pop == "c") %>%
#         ggplot(., aes(x=factor(neighb_id), perDose, fill=sample_type_name) ) +
#         geom_bar(stat="identity") +
#         facet_grid(~ city, scales = "free_x", space = "free_x") +
#         ylim(-0.5, 12) +
#         labs(subtitle = "Children",
#              fill = "Sample Type",
#              x = "Neighborhood",
#              y = "Total Exposure (log10)") +
#         theme_bw() +
#         theme(legend.position="bottom") -> plot.children
# 
# 
# grid.arrange(plot.adults, plot.children, nrow=2,
#              top = "Total Exposure")


# dat.expo1 %>%
#         ggplot(., aes(x=factor(neighb_id), perDose, fill=sample_type_name) ) +
#         geom_bar(stat="identity") +
#         facet_grid(age ~ city, scales = "free_x", space = "free_x") +
#         labs(subtitle = "Adults",
#              fill = "Sample Type",
#              x = "Neighborhood",
#              y = "Total Exposure (log10)") +
#         theme_bw() +
#         theme(legend.position="bottom")
# 
# 
# colors <- c("Open Drains" = '#e41a1c',
#             "Drinking Water" = '#377eb8',
#             "Raw Produce" = '#4daf4a',
#             "Floodwater" = '#984ea3',
#             "Bathing Water" = '#ff7f00',
#             "Oceans" = '#ffff33',
#             "Surface Water" = '#a65628',
#             "Public Latrine" = '#f781bf',
#             "Street Food" = '#999999')
# 
#         
# dat.expo1 %>%
#         ggplot(., aes(x=factor(neighb_id), y=perDose, fill=sample_type_name) ) +
#         geom_bar(stat="identity") +
#         facet_grid(age ~ city, scales = "free_x", space = "free_x") +
#         labs(fill = "Sample Type",
#              x = "Neighborhood",
#              y = "Total Exposure (log10)") +
#         theme_bw() +
#         theme(legend.position="bottom",
#               strip.text.x = element_text(size = 10),
#               strip.text.y = element_text(size = 12),
#               strip.background = element_rect(fill="white")) +
#         # scale_fill_brewer(palette="Dark2") 
#         scale_fill_manual(values = colors)
# 
# ggsave(filename=paste0(getwd(), "/images/new_exposure/", "exposure_colors1", ".png"), 
#        plot = last_plot(), width = 17.6, height = 9.5, units="in", dpi = 300)
# 
# 
