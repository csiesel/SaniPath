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
source("pplplot_functions.R")
dakar <- read.csv(paste0(getwd(), "/data/expo_Dakar.csv"), stringsAsFactors = F)
# define variables
# perc format: 87.8%
# dose in log format: 3.123
dakar$perExposed = dakar$perExposed*100
dakar$Dose = log10(dakar$Dose)

dakar_c <- filter(dakar, pop=="c")
dakar_a <- filter(dakar, pop=="a")
for(i in 1:nrow(dakar_c)){
        print(dakar_c$neighborhood[i])
        print(dakar_c$sample[i])
        pplplt <- make_pplplot(sample=dakar_c$sample[i], 
                       neighborhood=dakar_c$neighborhood[i], 
                       age="Children", 
                       perc=dakar_c$perExposed[i], 
                       dose=dakar_c$Dose[i], 
                       lab_MF=T)
        ggsave(paste0(getwd(), "/Dakar_plots/ppl_",dakar_c$neighborhood[i],dakar_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

for(i in 1:nrow(dakar_a)){
        print(dakar_a$neighborhood[i])
        print(dakar_a$sample[i])
        pplplt <- make_pplplot(sample=dakar_a$sample[i], 
                               neighborhood=dakar_a$neighborhood[i], 
                               age="Adults", 
                               perc=dakar_a$perExposed[i], 
                               dose=dakar_a$Dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Dakar_plots/ppl_",dakar_a$neighborhood[i],dakar_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}
