# Not sure if all these are necessary but just copy-pasted from my testing code

library(readxl)
library(ggplot2)
library(tidyverse)
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
hoods <- c("Mataheko", "Osu Alata", "Fante New Town", "Moshie Zongo", "Ahodwo", "Dakodwom")

ghana <- filter(multi, site=='Accra, Ghana' | site=='Kumasi, Ghana', neighborhood %in% hoods, age=='a')

ghana$unlogexp[which(ghana$site=="Kumasi, Ghana" & ghana$pathway=="surface")] <- 0
ghana$unlogexp[which(ghana$site=="Kumasi, Ghana" & ghana$pathway=="bathing")] <- 0

# Iterative plotting by unique neighborhood
adultexp2 <- treemap()
for(i in unique(ghana$neighborhood)){
  dev.new()
  adultexp<-treemap(ghana[ghana$neighborhood==i,], index="pathway", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                    title=paste0("Total Exposure for Adults in ", i, sep=""), title.legend="Exposure", legend=FALSE)
  }



# restricting to children
ghana2 <- filter(multi, site=='Accra, Ghana' | site=='Kumasi, Ghana', neighborhood %in% hoods, age=='c')
ghana2$unlogexp[which(ghana$site=="Kumasi, Ghana" & ghana$pathway=="surface")] <- 0
ghana2$unlogexp[which(ghana$site=="Kumasi, Ghana" & ghana$pathway=="bathing")] <- 0
# Iterative plotting by unique neighborhood
for(i in unique(ghana$neighborhood)){
  dev.new()
  childexp<-treemap(ghana2[ghana$neighborhood==i,], index="pathway", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                    title=paste0("Total Exposure for Children in ", i, sep=""), title.legend="Exposure", legend=FALSE)
}

