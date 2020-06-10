library(ggplot2)
library(readxl)
library(dplyr)
library(plotly)
library(shinyjs)
library(RColorBrewer)



# read data
# ff.multi <- read_excel("~/Desktop/SaniPath/Durban/Durban_Exposure.xlsx")
load("~/Desktop/SaniPath/Durban/expo_durban.rda")
ff.multi <- dat.expo
# ff.multi$perExposed = ff.multi$Percent/100
# ff.multi$Dose = 10^ff.multi$Dose


#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi <- plyr::arrange(ff.multi, date)
df.multi$citylabel <- "Durban"
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
  mutate(perc = (Dose / sum) * 100) -> df.durban

# multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")


multi <- df.durban
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


###################################################################
library(treemap)
library(treemapify)
library(gtable)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(readxl)


# read data
load("~/Desktop/SaniPath/Durban/expo_durban.rda")
ff.multi <- dat.expo


#USE THIS TO GET RID OF ANY ESTIMATES WITH <=5 usable samples
#ff.multi <- ff.multi[ff.multi$n.sample >5,]


df.multi <- ff.multi
df.multi <- plyr::arrange(ff.multi, date)
df.multi$citylabel <- "Durban"
df.multi$citylabel <-  factor(df.multi$citylabel, levels = unique(df.multi$citylabel))

df.multi <- df.multi %>% 
  mutate(sample = factor(.$sample, levels = factor(unique(sample))))
df.multi$sample


df.multi %>%
  group_by(neighborhood, age) %>%
  mutate(sum = sum(Dose)) %>%
  mutate(perc = (Dose / sum) * 100) -> df.durban


{
  df.durban %>% filter(age == "Adults") %>% {
    ggplot(., aes(area = Dose,
                  fill = log10(Dose),
                  label = paste(sample, "\n", paste0(round(perc, 0), "%"), "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
      scale_fill_gradient(
        low = "yellow", high = "darkred",
        breaks = log10(range(.$Dose)),
        labels = round(range(log10(.$Dose)))
      ) + theme_bw() +
      facet_wrap(~neighborhood, nrow = 1) +
      theme(legend.position = "none") +
      labs(fill = "Exposure",
           title = "Total Exposure in Durban, South Africa",
           subtitle = "Adults") 
  } -> plot.adults
  
  df.durban %>% filter(age == "Children") %>% {
    ggplot(., aes(area = Dose,
                  fill = log10(Dose),
                  label = paste(sample, "\n", paste0(round(perc, 0), "%"), "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
      scale_fill_gradient(
        low = "yellow", high = "darkred",#,
        breaks = log10(range(.$Dose)),
        labels = round(range(log10(.$Dose)),0)
      ) + theme_bw() +
      facet_wrap(~neighborhood, nrow = 1) +
      theme(legend.position = "bottom") +
      labs(fill = "Exposure",
           title = "Total Exposure in Durban, South Africa",
           subtitle = "Children") 
  } -> plot.children
  
  legend <- gtable_filter(ggplot_gtable(ggplot_build(plot.children)), "guide-box")
  
  grid.arrange(plot.adults, plot.children + theme(legend.position = "none"), legend,
               nrow = 3,
               heights = c(1.1, 1.1, 0.5)
  )
}


# save
plot <- arrangeGrob(plot.adults, plot.children + theme(legend.position = "none"), legend,
                    nrow = 3,
                    heights = c(1.1, 1.1, 0.3)
)


ggsave(plot = plot, paste0("~/Desktop/SaniPath/Durban/exposure_treemap_Durban_noremoval", Sys.Date(), ".png"), dpi = 300, width = 7, height = 6, units = "in")

######### STACKED BOX POT ########

load("~/Desktop/SaniPath/Durban/expo_durban.rda")

durban <- dat.expo

durban$neighb_pop <- paste0(durban$neighborhood, " ", durban$age)
durban <- durban %>% group_by(neighb_pop) %>% mutate(perDose = log10(sum(popDose)),
                                                     perDose = popDose/sum(popDose)*log10(sum(popDose)) )

durban$pathway <- durban$sample
durban$city <- "Durban"
durban$pathway[which(durban$sample=="Municipal and Piped Water")] <- "Municipal Drinking Water"
durban$pathway[which(durban$sample=="Flood Water")] <- "Floodwater"
durban$pathway[which(durban$sample=="Drain Water")] <- "Open Drain Water"
durban$pathway[which(durban$sample=="Produce")] <- "Raw Produce"

durban <- durban %>% select(c("neighborhood", "perDose", "pathway", "city", "age"))
durban <- durban %>% ungroup() %>% select(-c("neighb_pop"))

load("~/Desktop/SaniPath/Durban/dhaka.exp.RDA")

dhaka <- df.exposure.dhaka


dhaka <- dhaka %>% select(c("neighborhood", "perDose", "pathway","city", "age"))


df.exposure <- bind_rows(dhaka, durban)



  colors <- c("Open Drain Water" = '#0F8554',
              "Municipal Drinking Water" = '#1D6996',
              "Raw Produce" = '#38A6A5',
              "Floodwater" = '#5F4690',
              "Bathing Water" = '#EDD808',
              "Ocean Water" = '#994E95',
              "Surface Water" = '#E17C05',
              "Public Latrine" = '#74202D',
              "Street Food" = '#CC503E')
  
  
 durban_dhaka <- df.exposure %>% 
    ggplot(., aes(x=factor(neighborhood), y=perDose, fill=pathway) ) +
    geom_bar(stat="identity") +
    facet_grid(age~ city, scales = "free_x", space = "free_x") +
    labs(fill = "Pathway",
         x = "Neighborhood",
         y = "Total Exposure (log10)") +
    theme_bw() +
    theme(#legend.position="bottom",
      strip.text.x = element_text(size = 12),
      axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
      # strip.text.y = element_text(size = 12),
      strip.background = element_rect(fill="white")) +
    scale_fill_manual(values = colors)


  ggsave(plot = durban_dhaka, paste0("~/Desktop/SaniPath/Durban/durban_dhaka_exp", Sys.Date(), ".png"), dpi = 300, width = 14, height = 8.5, units = "in")
  
