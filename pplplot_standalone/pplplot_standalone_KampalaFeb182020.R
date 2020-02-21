##################################################################################################
# standalone people plot generator for sanipath
##################################################################################################

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(png)
library(dplyr)
library(magrittr)
##################################################################################################
setwd("pplplot_standalone")
source(paste0(getwd(),"/pplplot_functions.R"))
load("expo0217.rda")
load("res_bw_0217.rda")
load("expo_odw0219.rda")
kampala <- dat.expo
kampala_bw <- as.data.frame(res.bw)
kampala_odw <- as.data.frame(dat.expo.odw)
# define variables
# perc format: 87.8%
# dose in log format: 3.123
kampala$perExposed = kampala$perExposed*100
kampala$Dose = log10(kampala$Dose)

kampala_c <- filter(kampala, pop=="c")
kampala_a <- filter(kampala, pop=="a")
for(i in 1:nrow(kampala_c)){
        print(kampala_c$neighborhood[i])
        print(kampala_c$sample[i])
        pplplt <- make_pplplot(sample=kampala_c$sample[i], 
                       neighborhood=kampala_c$neighborhood[i], 
                       age="Children", 
                       perc=kampala_c$perExposed[i], 
                       dose=kampala_c$Dose[i], 
                       lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_c$neighborhood[i],kampala_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

for(i in 1:nrow(kampala_a)){
        print(kampala_a$neighborhood[i])
        print(kampala_a$sample[i])
        pplplt <- make_pplplot(sample=kampala_a$sample[i], 
                               neighborhood=kampala_a$neighborhood[i], 
                               age="Adults", 
                               perc=kampala_a$perExposed[i], 
                               dose=kampala_a$Dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_a$neighborhood[i],kampala_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

#DOING BATHING WATER BELOW

kampala_bw$neighborhood <- factor(kampala_bw$neighborhood, levels=c(2,3,4,5), labels=c("Central", "Kawempe", "Rubaga", "Nakawa"))
kampala_bw$sample <- "Bathing Water"

kampala_bw_c <- filter(kampala_bw, age==2)
kampala_bw_a <- filter(kampala_bw, age==1)
for(i in 1:nrow(kampala_bw_c)){
        print(kampala_bw_c$neighborhood[i])
        print(kampala_bw_c$sample[i])
        pplplt <- make_pplplot(sample=kampala_bw_c$sample[i], 
                               neighborhood=kampala_bw_c$neighborhood[i], 
                               age="Children", 
                               perc=kampala_bw_c$n[i], 
                               dose=kampala_bw_c$dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_bw_c$neighborhood[i],kampala_bw_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

for(i in 1:nrow(kampala_bw_a)){
        print(kampala_bw_a$neighborhood[i])
        print(kampala_bw_a$sample[i])
        pplplt <- make_pplplot(sample=kampala_bw_a$sample[i], 
                               neighborhood=kampala_bw_a$neighborhood[i], 
                               age="Adults", 
                               perc=kampala_bw_a$n[i], 
                               dose=kampala_bw_a$dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_bw_a$neighborhood[i],kampala_bw_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}


#DOING OTHER DRINKING WATER BELOW

kampala_odw$perExposed = kampala_odw$perExposed*100
kampala_odw$Dose = log10(kampala_odw$Dose)

kampala_odw$sample <- "Spring Drinking Water"

kampala_odw_c <- filter(kampala_odw, pop=="c")
kampala_odw_a <- filter(kampala_odw, pop=="a")
for(i in 1:nrow(kampala_odw_c)){
        print(kampala_odw_c$neighborhood[i])
        print(kampala_odw_c$sample[i])
        pplplt <- make_pplplot(sample=kampala_odw_c$sample[i], 
                               neighborhood=kampala_odw_c$neighborhood[i], 
                               age="Children", 
                               perc=kampala_odw_c$perExposed[i], 
                               dose=kampala_odw_c$Dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_odw_c$neighborhood[i],kampala_odw_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

for(i in 1:nrow(kampala_odw_a)){
        print(kampala_odw_a$neighborhood[i])
        print(kampala_odw_a$sample[i])
        pplplt <- make_pplplot(sample=kampala_odw_a$sample[i], 
                               neighborhood=kampala_odw_a$neighborhood[i], 
                               age="Adults", 
                               perc=kampala_odw_a$perExposed[i], 
                               dose=kampala_odw_a$Dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Kampala_plots/ppl_",kampala_odw_a$neighborhood[i],kampala_odw_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}


