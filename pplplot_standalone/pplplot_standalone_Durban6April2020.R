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
load("data/expo_durban.rda")

durban <- dat.expo

# define variables
# perc format: 87.8%
# dose in log format: 3.123
durban$perExposed = durban$perExposed*100
durban$Dose = log10(durban$Dose)

durban_c <- filter(durban, pop=="c")
durban_a <- filter(durban, pop=="a")
for(i in 1:nrow(durban_c)){
        print(durban_c$neighborhood[i])
        print(durban_c$sample[i])
        pplplt <- make_pplplot(sample=durban_c$sample[i], 
                       neighborhood=durban_c$neighborhood[i], 
                       age="Children", 
                       perc=durban_c$perExposed[i], 
                       dose=durban_c$Dose[i], 
                       lab_MF=T)
        ggsave(paste0(getwd(), "/Durban_plots/ppl_",durban_c$neighborhood[i],durban_c$sample[i],"_Children.png"), pplplt, width=7, height=7, dpi=200, units="in")
}

for(i in 1:nrow(durban_a)){
        print(durban_a$neighborhood[i])
        print(durban_a$sample[i])
        pplplt <- make_pplplot(sample=durban_a$sample[i], 
                               neighborhood=durban_a$neighborhood[i], 
                               age="Adults", 
                               perc=durban_a$perExposed[i], 
                               dose=durban_a$Dose[i], 
                               lab_MF=T)
        ggsave(paste0(getwd(), "/Durban_plots/ppl_",durban_a$neighborhood[i],durban_a$sample[i],"_Adults.png"), pplplt, width=7, height=7, dpi=200, units="in")
}


