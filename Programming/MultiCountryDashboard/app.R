

library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(Hmisc)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyUI(navbarPage("Multi-Country Dashboard",
                     #Panel to display plots by pathway                 
                     tabPanel("MULTICOUNTRY",
                              plotOutput("pplot"),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              hr(),
                              #h6("11: Kalshi_Mirpure • 22: Badda  • 33: Gabtoli •  44: UttarKhan  • 55: Gulshan •  66: Kamalapur • 77: Ganderia •  88: Hazaribagh •  99: Motijhil •  0: Dhanmondi",align="center"),
                             # h6("*NOTE: x scale changes depending on maximum log dose observed for the chosen pathway",align="center"),
                              fluidRow(
                                column(3,
                                       checkboxGroupInput("site", "Select a Study Site", c("Dhaka, Bangladesh"="Dhaka, Bangladesh", "Siem Reap, Cambodia"="Siem Reap, Cambodia", 
                                                                                     "Accra, Ghana"="Accra, Ghana", "Vellore, India"="Vellore, India","Maputo, Mozambique"="Maputo, Mozambique")),
                                       offset = 2,
                                       div(style="height:10px, padding-bottom:0px, Left")
                                ),
                                column(3,
                                       radioButtons("age", "Select an Age Group", c("Adults"="a", "Children"="c", "Both"="b")),
                                       offset = 2,
                                       div(style="height:5px, padding-bottom:0px Topright")
                                       
                                ),
                                column(3,
                                       radioButtons("pathway", "Select a Pathway", c("Bathing Water"="bathing", "Open Drains"="drain",
                                                                                     "Public Latrines"="latrine", "Produce"="produce","Surface Water"="surface",
                                                                                     "Street Food"="streetfood", "Municipal Water"="municipal",
                                                                                     "Other Drinking Water"="otherdrinking", "Flood Water"="flood")),
                                       offset = 2,
                                       div(style="height:10px, padding-bottom:0px, Left")
                                )
                                
                              )
                     )
                     
                     #Panel to display plots by neighborhood
                     # tabPanel("Within Neighborhoods",
                     #          plotOutput("hplot"),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          hr(),
                     #          h6("*NOTE: x scale changes depending on maximum log dose observed for the chosen neighborhood",align="center"),
                     #          fluidRow(
                     #            column(3,
                     #                   radioButtons("hood", "Select a Neighborhood", c("Dhanmondi (0)"="0", "Kalshi_Mirpure (11)"="11",
                     #                                                                   "Badda (22)"="22", "Gabtoli (33)"="33","UttarKhan (44)"="44",
                     #                                                                   "Gulshan (55)"="55", "Kamalapur (66)"="66",
                     #                                                                   "Ganderia (77)"="77", "Hazaribagh (88)"="88",
                     #                                                                   "Motijhil (99)"="99")),
                     #                   offset = 2,
                     #                   div(style="height:10px, padding-bottom:0px, Left")
                     #            ),
                     #            column(3,
                     #                   radioButtons("age2", "Select an Age Group", c("Adults"="a", "Children"="c", "Both"="both")),
                     #                   offset = 2,
                     #                   div(style="height:5px, padding-bottom:0px Topright")
                     #            )
                     #          )       
                     # )
  )
  )
)

###### Define server logic required to draw a histogram
server <- function(input, output) {
  
  multi <- read_excel(paste0(getwd(),"/cross_country_data.xlsx"), col_names=TRUE, na="NA")
  multi$age2 <- factor(multi$age, levels=c("a","c"),labels=c("Adults","Children"))

  
  
  ####### REACTIVE CHOICES  ##########
  agechoice <- reactive({input$age})
  #agechoice2 <- reactive({input$age2})
  pchoice <- reactive({input$pathway})
  schoice <- reactive({input$site})
  

  pplots <- reactive({
    if (agechoice() == 'a' & pchoice()!='otherdrinking'){
      multi2 <- filter(multi, age=="a", pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site))+geom_point(size=10)+ylim(0,100)+xlim(0,(max(multi2$dose)))+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Adults") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
    }
    else if (agechoice() == 'a' & pchoice()=='otherdrinking'){
      multi2 <- filter(multi, age=="a", pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site,shape=otherdrink))+geom_point(size=10)+ylim(0,100)+xlim(0,(max(multi2$dose)))+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Adults") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
    }
    else if (agechoice() == 'c' & pchoice()!='otherdrinking'){
      multi2 <- filter(multi, age=="c", pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site))+geom_point(shape=17,size=10)+
        xlim(0,(max(multi2$dose)))+ylim(0,100)+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Children") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
    }
    else if (agechoice() == 'c' & pchoice()=='otherdrinking'){
      multi2 <- filter(multi, age=="c", pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site,shape=otherdrink))+geom_point(size=10)+ylim(0,100)+xlim(0,(max(multi2$dose)))+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Adults") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
    }
    else if (agechoice() == 'b' & pchoice()!='otherdrinking'){
      multi2 <- filter(multi, pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site,shape=age2), fill=age)+scale_shape_manual("Age Group",values=c(16,17))+guides(shape=guide_legend(override.aes=list(colour="grey",shape=c(1,2))))+
        geom_point(size=10)+xlim(0,(max(multi2$dose)))+ylim(0,100)+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Adults and Children") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
      
    }
    else{
      multi2 <- filter(multi, pathway==pchoice(), site %in% schoice())
      ggplot(multi2, aes(x=dose,y=percent,colour=site,shape=age2), fill=age)+scale_shape_manual("Age Group",values=c(16,17))+guides(shape=guide_legend(override.aes=list(colour="grey",shape=c(1,2))))+
        geom_point(size=10)+xlim(0,(max(multi2$dose)))+ylim(0,100)+
        geom_text(aes(x=dose,y=percent,label=neighborhood),size=5, colour="Black",check_overlap=FALSE)+labs(title=paste0(toupper(pchoice()),", Adults and Children") ,x="Log Dose",y="Percent Exposed")+
        geom_vline(xintercept=c(2,4,6,8,10),colour="#1f78b4")
    }   
    
  })
  
  output$pplot <- renderPlot(pplots(),width="auto",height=600)
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

