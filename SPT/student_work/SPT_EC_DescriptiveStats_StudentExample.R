library(ggridges)

#### ES Plots ####
es1_final %>% 
  filter(!is.na(ec_conc), sample_type != 99) %>% 
  mutate(sample_type_name = ifelse(sample_type==11, "Pooled Latrine",
                            ifelse(sample_type==12, "Moore Swab",
                            ifelse(sample_type==16, "Pumping Station - UF",
                            ifelse(sample_type==99, "Field Blank", "??"))))) %>%
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
  # scale_x_continuous(breaks = c(0,2,4,6,8,10,12), limits=c(-1,12)) +
  labs(title="Environmental Surveillance: E. coli Concentration by Sample Type", 
       x="log10 E. Coli Concentration", 
       y="Sample Type")

#### SPT Plots ####
spt1_final %>%
  filter(!is.na(ec_conc), sample_type !=99) %>% 
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
  # scale_x_continuous(breaks = c(0,2,4,6,8,10,12), limits=c(-1,12)) +
  labs(title="Exposure Assessment: E. coli Concentration by Sample Type", x="log10 E. Coli Concentration", y="Sample Type")
  


#### SO Plots ####
so_final %>% 
  filter(!is.na(ec_conc), sample_type !=99) %>% 
  mutate(sample_type_name = ifelse(sample_type_so==141, "Cutting /Grinding Surface (FPa)",
                            ifelse(sample_type_so==142, "Storage/Preparation Bowl (FPb)",
                            ifelse(sample_type_so==143, "Food Preparation Area (FPc)",
                            ifelse(sample_type_so==144, "Cooking/Preparation Water (FPd)",
                            ifelse(sample_type_so==145, "Cooking/Preparation Utensil (FPe)",
                            ifelse(sample_type_so==151, "Child Obs - Floor",
                            ifelse(sample_type_so==152, "Child Obs - Off Ground",
                                                    "??")))))))) %>%
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
  # scale_x_continuous(breaks = c(0,2,4,6,8,10,12), limits=c(-1,12)) +
  labs(title="Structured Observation: E. coli Concentration by Sample Type", x="log10 E. Coli Concentration", y="Sample Type")

