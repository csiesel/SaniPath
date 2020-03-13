library(ggplot2)
library(readxl)
library(dplyr)
library(RColorBrewer)
library(plotly)

getwd()

multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")
multi$rank=0
multi$size=0
multi$shape=0
multi$fillcolor=""
multi$linecolor=""


#This is providing a rank to order the grid of output plots
for(i in 1:nrow(multi)){
  if(multi$pathway[i]=='bathing'){
    multi$rank[i]=5
  }
  else if(multi$pathway[i]=='municipal'){
    multi$rank[i]=4
}
  else if(multi$pathway[i]=="latrine"){
    multi$rank[i]=6
}
  else if(multi$pathway[i]=="drain"){
    multi$rank[i]=2
}
  else if(multi$pathway[i]=="flood"){
    multi$rank[i]=3
  }
  else if(multi$pathway[i]=='produce'){
    multi$rank[i]=1
  }
  else{
    multi$rank[i]=999
  }
  
}
markcolor <- c("Dhaka, Bangladesh"="#a6cee3", "Siem Reap, Cambodia"="#b2df8a", "Accra, Ghana"="#fb9a99",
               "Vellore, India"="#fdbf6f","Maputo, Mozambique"="#cab2d6","Atlanta, USA"="#f4f442", "Lusaka, Zambia"='#00b79c')
linecolor <-c("Dhaka, Bangladesh"="#1f78b4", "Siem Reap, Cambodia"="#33a02c", "Accra, Ghana"="#e31a1c",
              "Vellore, India"="#ff7f00","Maputo, Mozambique"="#6a3d9a", "Atlanta, USA"="#fed976", "Lusaka, Zambia"='#008470')

#This is providing a rank to order the grid of output plots
for(i in 1:nrow(multi)){
  if(multi$site[i]=='Dhaka, Bangladesh'){
    multi$size[i]=4
    multi$fillcolor[i]="#a6cee3"
    multi$linecolor[i]="#1f78b4"
  }
  else if(multi$site[i]=='Siem Reap, Cambodia'){
    multi$size[i]=4
    multi$fillcolor[i]="#b2df8a"
    multi$linecolor[i]="#33a02c"
  }
  else if(multi$site[i]=="Accra, Ghana"){
    multi$size[i]=4
    multi$fillcolor[i]="#fb9a99"
    multi$linecolor[i]="#e31a1c"
  }
  else if(multi$site[i]=="Vellore, India"){
    multi$size[i]=4
    multi$fillcolor[i]="#fdbf6f"
    multi$linecolor[i]="#ff7f00"
  }
  else if(multi$site[i]=="Maputo, Mozambique"){
    multi$size[i]=4
    multi$fillcolor[i]="#cab2d6"
    multi$linecolor[i]="#6a3d9a"
  }
  else if(multi$site[i]=='Atlanta, USA'){
    multi$size[i]=4
    multi$fillcolor[i]="#f4f442"
    multi$linecolor[i]="#fed976"
  }
  else{
    multi$size[i]=4
    multi$fillcolor[i]='#00b79c'
    multi$linecolor[i]='#008470'
  }
  
}

# Changing Shapes
for(i in 1:nrow(multi)){
  if(multi$site[i]=='Dhaka, Bangladesh'){
    multi$shape[i]=21
  }
  else if(multi$site[i]=='Siem Reap, Cambodia'){
    multi$shape[i]=21
  }
  else if(multi$site[i]=="Accra, Ghana"){
    multi$shape[i]=21
  }
  else if(multi$site[i]=="Vellore, India"){
    multi$shape[i]=21
  }
  else if(multi$site[i]=="Maputo, Mozambique"){
    multi$shape[i]=21
  }
  else if(multi$site[i]=='Atlanta, USA'){
    multi$shape[i]=21
  }
  else{
    multi$shape[i]=21
  }
  
}

###################### ONLY FOR DHAKA PRODUCE AND SF
pchoiceDhaka <- c("produce", "streetfood")
multiDhaka <- filter(multi, age=="a", pathway %in% pchoiceDhaka)
multiDhaka$pathway2<- factor(multiDhaka$pathway, levels=c("produce","streetfood"),
                         labels=c("Produce","Street Food"))
#######################




#Only using these pathways
pchoice <- c("municipal","latrine","flood","drain","produce","bathing")
#Filtering the data for only adults and only the pathways above
multi2 <- filter(multi, age=="a", pathway %in% pchoice)

#Changing the order of variables for horizontal plot with factor
multi2$pathway2<- factor(multi2$pathway, levels=c("produce","drain","flood","municipal","bathing","latrine"),
                         labels=c("Produce","Open Drains","Flood Water","Municipal Drinking Water","Bathing Water","Public Latrines"))
#Changing the order of variables for vertical plot with factor
multi2$pathway3<- factor(multi2$pathway, levels=c("latrine","bathing","municipal","flood","drain","produce"),
                         labels=c("Public Latrines","Bathing Water","Municipal Drinking Water","Flood Water","Open Drains","Produce"))





#Colors of dots and lines
markcolor <- c("Dhaka, Bangladesh"="#a6cee3", "Siem Reap, Cambodia"="#b2df8a", "Accra, Ghana"="#fb9a99",
               "Vellore, India"="#fdbf6f","Maputo, Mozambique"="#cab2d6","Atlanta, USA"="#f4f442", "Lusaka, Zambia"='#00b79c')
linecolor <-c("Dhaka, Bangladesh"="#1f78b4", "Siem Reap, Cambodia"="#33a02c", "Accra, Ghana"="#e31a1c",
              "Vellore, India"="#ff7f00","Maputo, Mozambique"="#6a3d9a", "Atlanta, USA"="#fed976", "Lusaka, Zambia"='#008470')
markshape <- c("Dhaka, Bangladesh"=21, "Siem Reap, Cambodia"=21, "Accra, Ghana"=21,
                "Vellore, India"=21,"Maputo, Mozambique"=21,"Atlanta, USA"=21, "Lusaka, Zambia"=21)


multi2mean <- multi2 %>% group_by(pathway3) %>% summarize(meanDose = mean(dose, na.rm=TRUE))



#Horizontal version
ggplot(multi2, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  geom_point(aes(size=size, shape=shape))+
  scale_shape_identity()+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,50))+
  scale_x_continuous(limits=c(0,14),breaks=seq(0,14,2))+
  labs(title="Multi-City Comparison")+
  labs(x="Log Dose E. coli",y="Percent Exposed")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(strip.background=element_rect(fill="#3673b8",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=10,face='bold'))+
  facet_grid(pathway2~.,labeller = label_wrap_gen(width=3, multi_line=TRUE))

#GOOD GOOD GOOD Horizontal
ggplot(multi2, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  geom_point(aes(size=size), shape=21)+
  scale_shape_identity()+
  scale_size_continuous(range=c(3,7), guide='none')+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,50))+
  scale_x_continuous(limits=c(0,14),breaks=seq(0,14,2))+
  labs(title="Multi-City Comparison")+
  labs(x="Log Dose E. coli",y="Percent Exposed")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(legend.text=element_text(size=10))+
  theme(axis.title = element_text(size=15, face='bold'))+
  theme(axis.text = element_text(size=15))+
  theme(strip.background=element_rect(fill="#3673b8",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=15,face='bold'))+
  guides(colour = guide_legend(override.aes=list(size=6)))+
  facet_grid(pathway2~., labeller = label_wrap_gen(width=3, multi_line=TRUE))


#GOOD GOOD GOOD Vertical version
ggplot(multi2, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  geom_vline(data=multi2mean, aes(xintercept=meanDose), color="darkred")+
  geom_point(size=3, shape=21)+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,25))+
  scale_x_continuous(limits=c(0,15),breaks=seq(0,15,5))+
  labs(title="Multi-City Comparison")+
  labs(x="Log Dose E. coli",y="Percent Exposed")+
  labs(caption="Note: Red line indicates the mean dose across sites")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(legend.text=element_text(size=10))+
  theme(axis.title = element_text(size=15, face='bold'))+
  theme(axis.text = element_text(size=15))+
  theme(strip.background=element_rect(fill="#1e3154",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=15,face='bold'))+
  guides(colour = guide_legend(override.aes=list(size=6)))+
  facet_grid(.~pathway3, labeller = label_wrap_gen(width=3, multi_line=TRUE))




#GOOD GOOD GOOD Vertical version - ONLY OPEN DRAINS, DW, and PRODUCE
multi3 <- filter(multi2, pathway3=="Open Drains" | pathway3=="Municipal Drinking Water" | pathway3=="Produce")
ggplot(multi3, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  geom_point(aes(size=size, shape=shape))+
  scale_shape_identity()+
  scale_size_continuous(range=c(3,7), guide='none')+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,50))+
  scale_x_continuous(limits=c(0,14),breaks=seq(0,14,2))+
  labs(title="Multi-City Comparison")+
  labs(x="Log Dose E. coli",y="Percent Exposed")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(legend.text=element_text(size=10))+
  theme(axis.title = element_text(size=15, face='bold'))+
  theme(axis.text = element_text(size=15))+
  theme(strip.background=element_rect(fill="#1e3154",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=15,face='bold'))+
  guides(colour = guide_legend(override.aes=list(size=6)))+
  facet_grid(pathway3~., labeller = label_wrap_gen(width=3, multi_line=TRUE))



# PRODUCE PLOT FOR PROPOSAL - Horizontal version
multi2produce <- subset(multi2, pathway=="produce")
produce<- ggplot(multi2produce, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  geom_point(size=6, shape=21)+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,25))+
  scale_x_continuous(limits=c(0,14),breaks=seq(0,14,2))+
  labs(title="SaniPath - Exposure to Fecal Contamination via Produce in 7 Countries")+
  labs(x="Log Dose E. coli (CFU/month or MPN/month)",y="Percent Exposed")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(strip.background=element_rect(fill="#3673b8",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=10,face='bold'))
produce
#produce <- ggplotly(produce)
#produce
#export(produce, file="produce.pdf")



ggplot(multiDhaka, aes(x=dose, y=percent, colour=site, fill=site, label=neighborhood,
                   text=paste("Log Dose E. coli: ",dose, '\nPercent Exposed: ', percent ,'%',
                              '\nSite: ', site, '\nNeighborhood: ', neighborhood)))+
  scale_color_manual(name="Study Site",
                     values= linecolor)+
  scale_fill_manual(name="Study Site",
                    values=markcolor)+
  scale_shape_manual(name="Study Site", values=markshape)+
  geom_point(aes(size=size))+
  scale_size_continuous(range=c(3,7), guide='none')+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,50))+
  scale_x_continuous(limits=c(0,14),breaks=seq(0,14,2))+
  labs(title="Multi-City Comparison")+
  labs(x="Log Dose E. coli",y="Percent Exposed")+
  theme_bw()+
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5,face='bold'))+
  theme(legend.text=element_text(size=10))+
  theme(axis.title = element_text(size=15, face='bold'))+
  theme(axis.text = element_text(size=15))+
  theme(strip.background=element_rect(fill="#1e3154",colour="grey"))+
  theme(strip.text=element_text(colour="white",size=15,face='bold'))+
  guides(colour = guide_legend(override.aes=list(size=6)))+
  guides(line = guide_legend(override.aes=list(colour=markcolor, shape=markshape)))+
  facet_grid(pathway2~., labeller = label_wrap_gen(width=3, multi_line=TRUE))







###################################
######## Treemap Things ###########
###################################
multi2$unlogdose <- 10^(multi2$dose)
multi2$unlogexp <- multi2$unlogdose * (multi2$percent/100)

multi3 <- filter(multi2, site=='Dhaka, Bangladesh')

adultexp<-treemap(multi3, index="pathway2", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                  title="Total Exposure for Adults", title.legend="Exposure", legend=FALSE)
#Treemap for adults w/o shallow well
adultresult2<- adultresult
adultresult2[2,]$unlogexp=0
adultexpnowell<-treemap(adultresult2, index="pathway", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                        title="Total Exposure for Adults (No Shallow Well)", title.legend="Exposure", legend=FALSE)

#Treemap for children
childexp<-treemap(childresult, index="pathway", vSize="unlogexp", vColor='unlogexp', type="value", palette="Reds",
                  title="Total Exposure for Children", title.legend="Exposure", legend=FALSE)
childexp



