#Just run the caseyapp.R file to load the datasets that are needed - too confusing to change path...

food <- df.ecdata %>% filter(sample_type_name %in% c("Street Food", "Raw Produce")) %>% group_by(sample_type_name, city) %>% summarise(n=n(), mean = mean(log10(ec_conc), na.rm=T), sd = sd(log10(ec_conc), na.rm=T))

food_sample <- df.ecdata %>% filter(sample_type_name %in% c("Street Food", "Raw Produce")) %>% group_by(sample_type_name) %>% summarise(n=n(), mean = mean(log10(ec_conc), na.rm=T), sd = sd(log10(ec_conc), na.rm=T), min=min(log10(ec_conc), na.rm=T), max=max(log10(ec_conc), na.rm=T))

food_ses <- df.ecdata %>% filter(sample_type_name %in% c("Street Food", "Raw Produce")) %>% group_by(sample_type_name, SES) %>% summarise(n=n(), mean = mean(log10(ec_conc), na.rm=T), sd = sd(log10(ec_conc), na.rm=T), min=min(log10(ec_conc), na.rm=T), max=max(log10(ec_conc), na.rm=T))
