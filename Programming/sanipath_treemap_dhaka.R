# Not sure if all these are necessary but just copy-pasted from my testing code

library(readxl)
library(ggplot2)
library(plotly)
library(stats)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(treemap)
library(igraph)



getwd()
multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")

# Creating the normal scale dose
multi$unlogdose <- 10^(multi$dose)
# creating the normal scale exposure (Big E)
multi$unlogexp <- multi$unlogdose * (multi$percent/100)
# restricting to dhaka and adults - can change to anything
dhaka <- filter(multi, site=='Dhaka, Bangladesh', age=='a')

# Iterative plotting by unique neighborhood
for(i in unique(dhaka$neighborhood)){
  dev.new()
  adultexp<-treemap(dhaka[dhaka$neighborhood==i,], index="pathway", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                    title=paste0("Total Exposure for Adults in ", i, sep=""), title.legend="Exposure", legend=FALSE)
}
