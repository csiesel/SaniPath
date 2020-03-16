library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)
library(plotly)
library(shinyjs)
library(RColorBrewer)
library(lubridate)

# data<- read_excel("H:/SaniPath/Deployments/Durban/diarrhea.xlsx")
# data_under5 <- filter(data, data$Data=="Acute diarrhoea under 5 years")
# data_5to14 <- filter(data, data$Data=="Acute diarrhoea 5-14 years")
# data_15plus <- filter(data, data$Data=="Acute diarrhoea 15 years and older")
# data_helminth <- filter(data, data$Data=="Worms (Helminthic) case")
# 
# write.csv(data_under5, "under5.csv")
# write.csv(data_5to14, "5to14.csv")
# write.csv(data_15plus, "15plus.csv")
# write.csv(data_helminth, 'helminth.csv')

# goodData <- data[FALSE,]
# 
# variable.names(data[4:111])
# names <- variable.names(data[4:111])
# i=0
# for(i in  4:111){
#   for(p in 0:121){
#     data_under5[i]
#   }
#   
# }

poop <- read_excel("DurbanDiarrhea.xlsx")
rain <- read_excel("mydata.xlsx")
rain <- as.data.frame(rain)
rain$date <- as.Date(rain$`date/time`)
poop$Date <- as.Date(poop$Date)
poop$total <- 0
for(i in 1:nrow(poop)){
  poop$total[i] = poop$under5[i]+poop$`5to14`[i]+poop$`15plus`[i]
}

poop <- poop[-c(1,2,3,4,5,6,7,8,9,76:108),]


asdf <- ggplot()+
  geom_line(data=poop, aes(x=poop$Date, y=poop$under5), color="blue")+
  geom_line(data=poop, aes(x=poop$Date, y=poop$`5to14`), color="red")+
  geom_line(data=poop, aes(x=poop$Date, y=poop$`15plus`),color="green")+
  geom_line(data=poop, aes(x=poop$Date, y=poop$helminth),color="purple")+
  geom_line(data=poop, aes(x=poop$Date, y=poop$total), color="brown")+
  geom_line(data=poop, aes(x=poop$Date, y=poop$rain), color="black")
asdf<- asdf + scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits=as.Date(c("2010-10-01","2016-03-01")))
poop$under5
ggplotly(asdf)


ay <- list(
  overlaying = "y",
  side = "right",
  title = "Rainfall (mm)"
)
ax <- list(
  tickformat="%b",
  dtick="M1"
)
#lineplots - above is formatting axes
p <- plot_ly(poop) %>%
  add_lines(x=~Date, y= ~total, name="Diarrhea", xaxis="M1") %>%
  add_lines(x=~Date, y= ~rain, yaxis="y2", name="Rain", xaxis="M1") %>%
  layout(yaxis2=ay, xaxis=ax) %>%
  rangeslider()
p

#this is combining everything by month
jkl <- poop
jkl$month=0
test <- jkl %>% mutate(month = format(Date, "%b")) %>%
  group_by(month) %>%
  summarise(total2 = sum(total), rain2=sum(rain))

test$month <- factor(test$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

plot_ly(test, labels=~month, values=~total2, type='pie')

plot_ly(test, x=~month, y=~total2, type='bar', name="Total Diarrhea Counts/ # Clinics Reporting", marker=list(color="rgb(153, 102, 0)")) %>%
  add_bars(y=~rain2, yaxis="y2", name="Rain", width=.2, marker=list(color="rgb(51, 153, 255)")) %>%
  layout(barmode='group', yaxis2 = ay, yaxis=list(title="Diarrhea Cases/Reporting Clinics"))

