df2 <- df.ecdata %>%
  group_by(sample_type_name) %>%
  summarise(mean.cont = mean(log10(ec_conc), na.rm=TRUE))

pdf(paste("~/Desktop/SaniPath/Ghana/Accra_ec.pdf",sep=""), width = 13.3, height=7.5)

df.ecdata %>%
  filter(city=="Accra") %>%
  ggplot(., aes(y=factor(hood, levels=(unique(hood))), x=log10(ec_conc))) +
  geom_density_ridges(aes(fill=hood), quantile_lines=TRUE, quantiles=2, panel_scaling=FALSE
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
  # geom_vline(data=df2, aes(xintercept=mean.cont, color="red"), show.legend=FALSE) +
  # geom_point(aes(color=city) ) +
  # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
  facet_wrap( ~ sample_type_name, scales = "fixed", nrow=4, ncol=3) +
  labs(fill = "City",
       x = "E. coli (Log10)",
       y = "") +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=8),
        strip.background = element_rect(fill="white"),
        legend.position="bottom",
        legend.justification="center")
dev.off()



pdf(paste("~/Desktop/SaniPath/Ghana/Kumasi_ec.pdf",sep=""), width = 13.3, height=7.5)

df.ecdata %>%
  filter(city=="Kumasi") %>%
  ggplot(., aes(y=factor(hood, levels=(unique(hood))), x=log10(ec_conc))) +
  geom_density_ridges(aes(fill=hood), quantile_lines=TRUE, quantiles=2, panel_scaling=FALSE
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
  # geom_vline(data=df2, aes(xintercept=mean.cont, color="red"), show.legend=FALSE) +
  # geom_point(aes(color=city) ) +
  # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
  facet_wrap( ~ sample_type_name, scales = "fixed", nrow=4, ncol=3) +
  labs(fill = "City",
       x = "E. coli (Log10)",
       y = "") +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2),
        axis.text=element_text(size=8),
        strip.background = element_rect(fill="white"),
        legend.position="bottom",
        legend.justification="center")
dev.off()




df.perc$unit <- factor(df.perc$sample_type, levels=c("Open Drain Water", "Ocean", "Surface Water",
                                                     "Bathing Water", "Floodwater", "Public Latrine", "Raw Produce", "Street Food",
                                                     "Municipal Drinking Water"), labels=c("Open Drain Water"="Times/Month", "Ocean"="Month", "Surface Water"="Times/Month",
                                                                                           "Bathing Water"="Times/Week", "Floodwater"="Times/Week", "Public Latrine"="Times/Week", "Raw Produce"="Times/Week", "Street Food"="Times/Week",
                                                                                           "Municipal Drinking Water"="Days/Week"))

#------------------------------------------
pdf(paste("~/Desktop/SaniPath/Ghana/Accra_bx.pdf",sep=""), width = 13.3, height=7.5)
df.behav.city %>%
  filter(., city =="Accra") %>%
  melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood", "unit", "SES")) %>%
  na.omit(value) %>%
  # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
  ggplot(., aes(x = neighborhood, y = value, fill = variable)) + #add value labels, add linetype=SES if interested
  geom_bar(stat = "identity", colour="black") +
  # geom_point(aes(x= neighborhood, y=-.01, shape=SES)) +
  # geom_text(aes(label=SES), stat="identity", position=position_stack(0.5)) +
  coord_flip() +
  facet_nested(pop + city ~ sample_type + unit, scales = "free", space = "free") + #scales = "free_x"
  theme_bw() +
  labs(title = "Distribution of Behaviors",
       fill = "Frequency",
       # shape = "SES",
       x = "Neighborhood",
       y = "Percent") +
  # theme(strip.text.y = element_text(size = 7)) +
  scale_fill_brewer(palette="Set2") +
  # scale_linetype_manual(values=c("solid", "dashed", "dotted", "twodash")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
        # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        strip.background = element_rect(fill="grey"),
        strip.placement = "outside",
        legend.position="bottom",
        panel.spacing=unit(0.5,"lines")) + 
  scale_y_continuous(breaks=seq(0,1,by=.5), labels = scales::percent) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))


dev.off()



pdf(paste("~/Desktop/SaniPath/Ghana/Kumasi_bx.pdf",sep=""), width = 13.3, height=7.5)
df.behav.city %>%
  filter(., city =="Kumasi") %>%
  melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood", "unit", "SES")) %>%
  na.omit(value) %>%
  # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
  ggplot(., aes(x = neighborhood, y = value, fill = variable)) + #add value labels, add linetype=SES if interested
  geom_bar(stat = "identity", colour="black") +
  # geom_point(aes(x= neighborhood, y=-.01, shape=SES)) +
  # geom_text(aes(label=SES), stat="identity", position=position_stack(0.5)) +
  coord_flip() +
  facet_nested(pop + city ~ sample_type + unit, scales = "free", space = "free") + #scales = "free_x"
  theme_bw() +
  labs(title = "Distribution of Behaviors",
       fill = "Frequency",
       # shape = "SES",
       x = "Neighborhood",
       y = "Percent") +
  # theme(strip.text.y = element_text(size = 7)) +
  scale_fill_brewer(palette="Set2") +
  # scale_linetype_manual(values=c("solid", "dashed", "dotted", "twodash")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
        # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        strip.background = element_rect(fill="grey"),
        strip.placement = "outside",
        legend.position="bottom",
        panel.spacing=unit(0.5,"lines")) + 
  scale_y_continuous(breaks=seq(0,1,by=.5), labels = scales::percent) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))


dev.off()
