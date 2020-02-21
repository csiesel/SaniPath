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
source("pplplot_functions.R")
kampala <- read.csv(paste0(getwd(), "/Kampala_Exposure_Data.csv"), stringsAsFactors = F)
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
        ggsave(paste0(getwd(), "/plots/ppl_",kampala_c$neighborhood[i],kampala_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
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
        ggsave(paste0(getwd(), "/plots/ppl_",kampala_a$neighborhood[i],kampala_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}
