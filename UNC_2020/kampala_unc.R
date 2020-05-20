#Just run the caseyapp.R file to load the datasets that are needed - too confusing to change path...

kampala_sample_ec <- df.ecdata %>%
  filter(city=="Kampala") %>% 
  group_by(sample_type_name) %>% 
  summarise(n=n(), mean = mean(log10(ec_conc), na.rm=T), 
            sd = sd(log10(ec_conc), na.rm=T), min=min(log10(ec_conc), na.rm=T), 
            max=max(log10(ec_conc), na.rm=T))

kampala_food_sample <- df.ecdata %>% 
  filter(sample_type_name %in% c("Street Food", "Raw Produce"), city=="Kampala") %>% 
  group_by(sample_type_name) %>% 
  summarise(n=n(), mean = mean(log10(ec_conc), na.rm=T), 
            sd = sd(log10(ec_conc), na.rm=T), min=min(log10(ec_conc), na.rm=T), 
            max=max(log10(ec_conc), na.rm=T))

kampala_bx_city <- df.behav.all %>%
  filter(city=="Kampala") %>%
  group_by(sample_type, pop) %>%
  summarise(n=n(), any_contact = sum(`10+`, `6-10`, `<5`), 
            no_contact=`Never`, total=any_contact+no_contact, unit=unit,
            `10+`=`10+`, `6-10`=`6-10`, `<5`=`<5`)

kampala_exp_city <- df.exposure %>%
  filter(city=="Kampala") %>%
  group_by(sample_type_name, age) %>%
  summarise(n=n(), mean_dose = mean(log10(dose)), min_dose=min(log10(dose)), max_dose=max(log10(dose)), percExp=mean(exposure))
