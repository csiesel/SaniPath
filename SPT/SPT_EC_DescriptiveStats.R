source("~/Desktop/SaniPath/SPT/4_SPT_Ecoli.R")

library(ggridges)

ec_data_spt %>% 
  filter(!is.na(count1)) %>% 
  group_by(sample_type) %>% 
  dplyr::summarise(count = dplyr::n(), 
            mean=mean(log10(ec_conc), na.rm=T), 
            min = min(log10(ec_conc), na.rm=T), 
            max = max(log10(ec_conc), na.rm=T))

ec_data_es %>% 
  filter(!is.na(count1)) %>% 
  group_by(sample_type, col_ward) %>% 
  dplyr::summarise(count = n(), 
            mean=mean(log10(ec_conc), na.rm=T), 
            min = min(log10(ec_conc), na.rm=T), 
            max = max(log10(ec_conc), na.rm=T))

ec_data_es %>% 
  filter(!is.na(count1)) %>% 
  group_by(sample_type) %>% 
  dplyr::summarise(count = n(), 
            mean=mean(log10(ec_conc), na.rm=T), 
            min = min(log10(ec_conc), na.rm=T), 
            max = max(log10(ec_conc), na.rm=T))

ec_data_es %>% 
  filter(!is.na(count1)) %>% 
  group_by(sample_type) %>% 
  ggplot(., aes(x=ec_conc, y=sample_type)) +
  geom_density_ridges()


pdf(paste("~/Desktop/SaniPath/SPT/output/","spt_ec_", Sys.Date(),".pdf",sep=""), width = 10, height=8)

ec_data_spt %>%
  filter(!is.na(count1)) %>% 
  mutate(sample_type_name = ifelse(sample_type==1, "Open Drain",
                            ifelse(sample_type==2, "Raw Produce",
                            ifelse(sample_type==3, "Drinking Water",
                            ifelse(sample_type==5, "Surface Water",
                            ifelse(sample_type==6, "Floodwater",
                            ifelse(sample_type==7, "Public Latrine",
                            ifelse(sample_type==8, "Soil",
                            ifelse(sample_type==9, "Bathing Water",
                            ifelse(sample_type==10, "Street Food",
                            ifelse(sample_type==99, "Field Blank",
                            "??"))))))))))) %>%
    ggplot(., aes(y=factor(sample_type_name, levels=(unique(sample_type_name)), labels=), x=log10(ec_conc))) +
  geom_density_ridges(quantile_lines=TRUE, quantiles=2
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
  labs(title="E. coli Concentration by Sample Type", x="log10 E. Coli Concentration", y="Sample Type")
  
dev.off()



