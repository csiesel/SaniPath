library(ggplot2)
library(readxl)
library(dplyr)
library(plotly)
library(shinyjs)
library(RColorBrewer)



# read data
setwd("/Users/caseysiesel/Desktop/SaniPath/Programming/Dakar")
ff.multi <- read.csv(paste0(getwd(), "/expo_Dakar.csv"), stringsAsFactors = F)

#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi <- plyr::arrange(ff.multi, date)
df.multi$citylabel <- "Dakar"
df.multi$citylabel <-  factor(df.multi$citylabel, levels = unique(df.multi$citylabel))

df.multi <- df.multi %>% 
  mutate(sample = factor(.$sample, levels = factor(unique(sample))))
df.multi$sample


# data used df.multi

# ***********************************************************************************************
# indiviual graphs ----
# ***********************************************************************************************

# df.multi <- subset(df.multi, neighborhood=="Central")

df.multi %>%
  group_by(neighborhood, age) %>%
  mutate(sum = sum(Dose)) %>%
  mutate(perc = (Dose / sum) * 100) -> df.kampala

# multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")




multi <- df.kampala
multi$dominant = "No"
multi$dominantcount = 0
multi$exposure=0
multi$logexp=0
sites <- unique(multi$site)
neighborhoods <- unique(multi$neighborhood)

# calculating exposure and log exposure
for(p in 1:nrow(multi)){
  multi$exposure[p] = (((multi$Dose[p]))*multi$perExposed[p])
  multi$logexp[p] = (log10(multi$exposure[p]))
  if(is.na(multi$logexp[p]) | multi$Dose[p]==0){
    multi$logexp[p]=-9999
  }
}


multiFinal = list()

for(q in 1:length(neighborhoods)){
  tempmulti1 <- filter(multi, neighborhood == neighborhoods[q], pop=='a')
  tempmulti2 <- filter(multi, neighborhood == neighborhoods[q], pop=='c')
  print(tempmulti1$neighborhood)
  for(a in 1:nrow(tempmulti1)){
    maxExpA = max(tempmulti1$logexp)
    botRangeA = maxExpA - 1
    if(tempmulti1$logexp[a]>= botRangeA){
      tempmulti1$dominant[a] = "Yes"
      tempmulti1$dominantcount[a] = 1
    }
    else{
      tempmulti1$dominant[a] = "No"
    }
  }
  
  for(c in 1:nrow(tempmulti2)){
    maxExpC = max(tempmulti2$logexp)
    botRangeC = maxExpC - 1
    if(tempmulti2$logexp[c]>= botRangeC){
      tempmulti2$dominant[c] = "Yes"
      tempmulti2$dominantcount[c] = 1
    }
    else{
      tempmulti2$dominant[c] = "No"
    }
  }
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti1
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti2
}

# combining the lists
multiFinalGood = do.call(rbind, multiFinal)

adult <- filter(multiFinalGood, pop=='a')
child <- filter(multiFinalGood, pop=='c')

# counting the number of dominant by pop and pathway
with(multiFinalGood, table(sample, dominant, pop, neighborhood))


# plotting count of dominant pathways
colors <- c('#1a3157', '#3e65ae', '#086fba', '#588fc7', '#6fb4dd', '#81d1ef', '#8d98ab', '#ffffff', '#cccccc')
asdf <- colorRampPalette(c('#1a3157', '#d4eaf7'))
asdf2 <- asdf(10)
plot_ly(data=adult, values=~dominantcount, labels=~sample, type='pie', hole=0.4,
        marker=list(colors=colors, line=list(color='#CCCCCC', width=1)),
        textinfo='label+value')
plot_ly(data=child, values=~dominantcount, labels=~sample, type='pie', hole=0.4,
        marker=list(colors=colors, line=list(color='#CCCCCC', width=1)),
        textinfo="label+value")
