library(ggplot2)
library(readxl)
library(dplyr)
library(plotly)
library(shinyjs)
library(RColorBrewer)

multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")
multi$dominant = "No"
multi$dominantcount = 0
multi$exposure=0
multi$logexp=0
sites <- unique(multi$site)
neighborhoods <- unique(multi$neighborhood)

# calculating exposure and log exposure
for(p in 1:nrow(multi)){
  multi$exposure[p] = ((10^(multi$dose[p]))*multi$percent[p])
  multi$logexp[p] = (log10(multi$exposure[p]))
  if(is.na(multi$logexp[p]) | multi$dose[p]==0){
    multi$logexp[p]=-9999
  }
}


multiFinal = list()

for(q in 1:length(neighborhoods)){
  tempmulti1 <- filter(multi, neighborhood == neighborhoods[q], age=='a')
  tempmulti2 <- filter(multi, neighborhood == neighborhoods[q], age=='c')
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

adult <- filter(multiFinalGood, age=='a')
child <- filter(multiFinalGood, age=='c')

# counting the number of dominant by age and pathway
with(multiFinalGood, table(pathway, dominant, age))


hoods <- c("Mataheko", "Osu Alata", "Fante New Town", "Moshie Zongo", "Ahodwo", "Dakodwom")

# Ghana
multiFinalGhana <- filter(multiFinalGood, site=="Accra, Ghana" | site=="Kumasi, Ghana", neighborhood %in% hoods)




with(multiFinalGhana, table(pathway, dominant, age, neighborhood))

with(multiFinalGhana, table(pathway, dominant, age))
taco <- filter(multiFinalGhana, age=="a", neighborhood=="Osu Alata")

# plotting count of dominant pathways
colors <- c('#1a3157', '#3e65ae', '#086fba', '#588fc7', '#6fb4dd', '#81d1ef', '#8d98ab', '#ffffff', '#cccccc')
asdf <- colorRampPalette(c('#1a3157', '#d4eaf7'))
asdf2 <- asdf(10)
plot_ly(data=adult, values=~dominantcount, labels=~pathway, type='pie', hole=0.4,
        marker=list(colors=colors, line=list(color='#CCCCCC', width=1)),
        textinfo='label+value')
plot_ly(data=taco, values=~exposure, labels=~pathway, type='pie', hole=0.4,
        marker=list(colors=colors, line=list(color='#CCCCCC', width=1)),
        textinfo="label+percent")
