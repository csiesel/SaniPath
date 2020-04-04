library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(shinythemes)
library(dashboardthemes)
library(shinyWidgets)
# http://shinyapps.dreamrs.fr/shinyWidgets/
library(ggh4x)
#devtools::install_github("teunbrand/ggh4x")
library(DT)
library(expss)
library(tidyverse)
library(reshape2)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(forcats)
library(ggridges)


# load data
source("helper_dataload.R")
source("themes.R")

# Define UI for application that draws a histogram
#### UI ####
ui <-
  fluidPage(dashboardPage(
    dashboardHeader(title = logo_sanipath),
    dashboardSidebar(
      sidebarMenu(
        HTML(
          "<div style=background-color:rgb(84,84,84);height:20px;> <h5 align=center> <b>
          <font color=white> SaniPath Multi-City Comparison </font>
          </b></h5></div>"
        ),
        HTML(
          "<h6 align=center> <b>
          <font color=rgb(26, 49, 87)> The following tab presents results for <br> all study sites </font>
          </b></h5>"
        ),
        menuItem(
          "Multi-City Comparison",
          tabName = "tabmulti",
          icon = icon("globe-africa")
        ),
        # tags$hr(style="background-color: rgb(26, 49, 87); height: 3px;"),
        HTML(
          "<div style=background-color:rgb(84,84,84);height:20px;> <h5 align=center> <b>
          <font color=white> Deployment-Specific Results </font>
          </b></h5></div>"
        ),
        HTML(
          "<h6 align=center> <b>
          <font color=rgb(26, 49, 87)> The following tabs present results for <br> the study site(s) selected below </font>
          </b></h5>"
        ),
        menuItem(
          "Deployment Overview",
          tabName = "taboverview",
          icon = icon("home")
        ),
        menuItem(
          "Environmental Contamination",
          tabName = "tabenviron",
          icon = icon("leaf")
        ),
        menuItem(
          "Behavior Frequency",
          tabName = "tabbehav",
          icon = icon("pie-chart")
        ),
        menuItem("Exposure", tabName = "tabexposure", icon = icon("asterisk")),
        tags$h6(
          "Select City/Cities for",
          tags$br(),
          "Deployment Results",
          style = "text-align: center;  text-decoration: underline; font-weight: bold; color: rgb(26, 49, 87);"
        ),
        checkboxGroupButtons(
          "city",
          NULL,
          c(sort(cities)),
          selected = "Accra",
          direction = "vertical",
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        tags$h6("* Selections above will change the", tags$br(), "deployment-specific results ",
                style="text-align: center; font-style: italic; color: rgb(26, 49, 87);")
        )
      ),
    
    dashboardBody(
      #THis is loading the custom theme
      sanipath,
      tabItems(
        # **************************************************************************************************
        ##### Tab 1: MultiCity Comparison ####
        tabItem(
          tabName = "tabmulti",
          wellPanel(style = "padding: 10px;",
            h2("SaniPath Study by the Numbers")
          ),
            fluidRow(
              valueBoxOutput("multiboxcountries"),
              valueBoxOutput("multiboxcity"),
              valueBoxOutput("multiboxneighb")
            ),
            fluidRow(
              valueBoxOutput("multiboxsamples")
            ),
            fluidRow(
              valueBoxOutput("multiboxhhsurveys"),
              valueBoxOutput("multiboxccsurveys"),
              valueBoxOutput("multiboxsssurveys")
            ),
          wellPanel(
            h2("SaniPath Study Sites"),
            leafletOutput("mapcountries")),
          wellPanel(
            h2("Exposure to Fecal Contamination for Adults and Children by Study Site"),
            plotOutput("plot_exposure_multi")
          ),
            fluidRow(
              column(6,
                     wellPanel(h4(uiOutput("commondomadult"), align = "center"))
              ),
              column(6,
                     wellPanel(h4(uiOutput("commondomchild"), align = "center"))
              )
            ),
            wellPanel(
            h2("Count of Dominant Pathways across Cities", align = "center"),
            plotOutput("multidom")
            )
        ),
        
        # **************************************************************************************************
        #### Tab 2: Deployment Overview ####
        tabItem(
          tabName = "taboverview",
          wellPanel(style = "padding: 10px",
                    h2("Deployment by the Numbers")),
          fluidRow(
            valueBoxOutput("boxneighb"),
            valueBoxOutput("boxsamples")
          ),
          fluidRow(
            valueBoxOutput("boxhhsurveys"),
            valueBoxOutput("boxccsurveys"),
            valueBoxOutput("boxsssurveys")
          ),
          wellPanel(style = "padding: 10px;",
            h2("Study Neighborhoods"),
            leafletOutput("mapneighborhoods")
          ),
          fluidRow(
            wellPanel(style = "padding: 10px;",
              h2("Dominant Pathways of Exposure to Fecal Contamination"),
              dataTableOutput("domtable")
            )
          )
        ),
        
        # **************************************************************************************************
        #### Tab 3: Environmental Samples ####
        tabItem(
          tabName = "tabenviron",
          wellPanel(style = "padding: 10px;",
            h2("  Environmental Contamination"),
            h4("  Select environmental pathways below to update the map and graphs"),
            checkboxGroupButtons(
              "sample",
              NULL,
              c(samples),
              individual = TRUE,
              width = '100%',
              status = "primary",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              )
            ),
            tags$h6("* We recommend only selecting a single city for the map comparison", style="font-style: italic")
          ),
          fluidRow(
            column(6,
              wellPanel(style = "padding: 10px;",
                h3("Hotspots of Fecal Contamination", align="center"),
                leafletOutput("mapecoli", height = "600px"),
                HTML(
                  "<p align=center> <i> <font size=2 color=darkred>
                  NOTE: Units are as normalized Log10 E. coli. Normalized Log10 E. coli is calculated on the city level and provides
                  a scale of 0-1 using the following formula: x-min/max-min, where x is the Log10 E. coli concentration, min is the
                  lowest concentration of E. coli for that sample type in the city and max is the highest.
                  </font> </i> </p>"
                )
              )
            ),
            column(6,
              wellPanel(style = "padding: 10px;",
                h3("E. coli contamination by sample type", align ="center"),
                plotOutput("plot_ecoli", height = "600px"),
                HTML(
                  "<p align=center> <i> <font size=2 color=darkred>
                  NOTE: Units are as Log10 E. coli/100mL except for the following: Street Food and Raw Produce (Log10 E. coli/serving),
                  Latrine Swabs (Log10 E. coli/swab), Soil (Log10 E. coli/gram). <br> <br>
                  These plots show the data density estimations based on the distribution of contamination found on environmental samples
                  from the SaniPath Study. Dots represent individual samples and the level of E. coli detected.

                  </font> </i> </p>"
                )
              )
            )
          )
        ),
        # **************************************************************************************************
        #### Tab 4: Behavioral frequency ####
        tabItem(
          tabName = "tabbehav",
          wellPanel(style = "padding: 10px;",
            h2("Behavior Frequency"),
            h4("Select environmental pathways below to update the graphs"),
            checkboxGroupButtons(
              "bx",
              NULL,
              c(samples),
              individual = TRUE,
              width = '100%',
              status ="primary",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              )
            )
          ),
          wellPanel(style = "padding: 10px;",
            h3("Distribution of Behavior by Neighborhood and City for Selected Pathways"),
            plotOutput("plot_behavior", height = "700px")
          ),
          wellPanel(style = "padding: 10px;",
            h3("Distribution of Behavior by City (combined neighborhoods) for Selected Pathways"),
            plotOutput("plot_behavior_city", height ="700px")
          )
        ),
        # **************************************************************************************************
        #### Tab 5: Exposure ####
        tabItem(
          tabName = "tabexposure",
          h1("Exposure and Dominant Pathways"),
          wellPanel(
            h2("Distribution of Exposure by Neighborhood and City Across Pathways"),
            plotOutput("plot_exposure_all", height = "700px")
          ),
          wellPanel(
            fluidRow(
              tags$h1("The Most Common Dominant Pathways for ", style = "display:inline;vertical-align:top;"),
              div(style = "display:inline-block;vertical-align:top;",
                selectInput(
                  "age",
                  label = NULL,
                  choices = c("Adults", "Children"),
                  selected = "Adults",
                  width = "100px"
                )
              ),
              tags$h1(" in:", style = "display:inline;vertical-align:top;"),
              tags$h1(textOutput("citychoice"), style = "display:inline;vertical-align:top;color:rgb(62, 0, 0);")
            )
          ),
          fluidRow(
            column(4,
                   uiOutput("dom1")),
            column(4,
                   uiOutput("dom2")),
            column(4,
                   uiOutput("dom3"))
          )
          
          #delete below
          ,
          h3("BELOW ARE JUST EXAMPLES")
          ,
          fluidRow(
            column(4,
                   uiOutput("test1")),
            column(4,
                   uiOutput("test2")),
            column(4,
                   uiOutput("test3"))
          )
          ,
          fluidRow(
            column(4,
                   uiOutput("test4")),
            column(4,
                   uiOutput("test5")),
            column(4,
                   uiOutput("test6"))
          )
          ,
          fluidRow(
            column(4,
                   uiOutput("test7")),
            column(4,
                   uiOutput("test8")),
            column(4,
                   uiOutput("test9"))
          )
          ,
          fluidRow(column(4,
                          uiOutput("test10")))

        )
      )
    )
  )
)


# Define server logic required to draw a histogram
#### Server ####
server <- function(input, output, session) {
        # **************************************************************************************************
        #### INPUT: City Choice ####
          citychoice <- reactive({input$city})
        
        # **************************************************************************************************
        #### INPUT: Sample Choice Important Input for Reactive CheckboxGroupInput on Sample Slide####
          observeEvent(input$city,{
            df.ecdata <- filter(df.ecdata, city %in% citychoice())
            samples <- as.character(unique(df.ecdata$sample_type_name))
            updateCheckboxGroupButtons(session, "sample", choices=c(samples), selected=samples[[1]], status="primary",
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")))
          })
          samplechoice <- reactive({input$sample})
        
        # **************************************************************************************************
        #### INPUT: Bx Choice Important Input for Reactive CheckboxGroupInput on Bx Slide####
          observeEvent(input$city,{
            df.ecdata <- filter(df.ecdata, city %in% citychoice())
            samples <- as.character(unique(df.behav.all$sample_type))
            updateCheckboxGroupButtons(session, "bx", choices=c(samples), selected=samples[[1]], status="primary",
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")))
          })
          bxchoice <- reactive({input$bx})
        
        # **************************************************************************************************
        #### INPUT: age selector on Exposure tab ####
          agechoice <- reactive({input$age})
        
        # **************************************************************************************************
        #### Multi-City Comparison Tab####
        
          #multicity most common dominant pathways: Adult
          output$commondomadult <- renderUI({
            domcount <- df.dominant %>%
              group_by(pathway, age) %>%
              summarise(n=n())
            nhoods <- length(unique(meta_neighb$neighborhood))
            domcount <- domcount %>%
              mutate(., percent=ceiling((n/nhoods)*100))
            domcount %>%
              filter(., age=="Adults") %>%
              arrange(., desc(n)) -> adult
  
            tags$h2("The 3 most common dominant pathways for Adults across cities are: ", style="text-align: center;",
                    tags$h3("1) ",adult[1,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
                    tags$h4(" -", adult[1,]$percent, "% of all neighborhoods", style="text-align: center;"),
                    tags$h3("2) ",adult[2,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
                    tags$h4(" -", adult[2,]$percent, "% of all neighborhoods", style="text-align: center;"),
                    tags$h3("3) ",adult[3,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
                    tags$h4(" -", adult[3,]$percent, "% of all neighborhoods", style="text-align: center;")
            )
          })  
          
          #multicity most common dominant pathways: Child
          output$commondomchild <- renderUI({
            domcount <- df.dominant %>% group_by(pathway, age) %>% summarise(n=n())
            nhoods <- length(unique(meta_neighb$neighborhood))
            domcount <- domcount %>%
              mutate(., percent=ceiling((n/nhoods)*100))
            domcount %>%
              filter(., age=="Children") %>%
              arrange(., desc(n)) -> child
            

            tags$h2("The 3 most common dominant pathways for Children across cities are: ", style="text-align: center;",
            tags$h3("1) ",child[1,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
            tags$h4(" -", child[1,]$percent, "% of all neighborhoods", style="text-align: center;"),
            tags$h3("2) ",child[2,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
            tags$h4(" -", child[2,]$percent, "% of all neighborhoods", style="text-align: center;"),
            tags$h3("3) ",child[3,]$pathway, style="color: rgb(62, 0, 0); text-align: center;"),
            tags$h4(" -", child[3,]$percent, "% of all neighborhoods", style="text-align: center;")
            )
          })
          
          #multicity box
          output$multiboxcity <- renderValueBox({
                  valueBox(
                          length(unique(meta_dply$city)), "Cities", icon = icon("info-sign", lib = "glyphicon"),
                          color = "teal")
          })
          
          #multineighborhood box
          output$multiboxneighb <- renderValueBox({
            valueBox(
              length(unique(meta_neighb$neighborhood)), "Neighborhoods", icon = icon("city"),
              color = "teal")
          })
          
          #country box
          output$multiboxcountries <- renderValueBox({
            valueBox(length(unique(meta_dply$country)), "Countries", icon = icon("flag", lib = "glyphicon"), color = "teal")
          })
          
          #multisamples box
          output$multiboxsamples <- renderValueBox({
            valueBox(
              nrow(df.ecdata), "Environmental Samples", icon = icon("flask"),
              color = "light-blue")
          })
          
          #multi hh survey box
          output$multiboxhhsurveys <- renderValueBox({
            valueBox(nrow(df.hh), "Household Surveys", icon=icon("clipboard"), color="aqua")
          })
          
          #multi cc survey box
          output$multiboxccsurveys <- renderValueBox({
            valueBox(nrow(df.cc), "Community Surveys", icon=icon("clipboard"), color="aqua")
          })
          
          #multi sc survey box
          output$multiboxsssurveys <- renderValueBox({
            valueBox(nrow(df.sc), "School Surveys", icon=icon("clipboard"), color="aqua")
          })
          
          #multi dom Lollipop chart
          multidom <- reactive({
            if(is.null(input$city)){
              return(NULL)
            }
            
            colors <- c("Adults" = '#961f16',
                        "Children" = '#1a3157')
            
            domcount <- df.dominant %>% group_by(pathway, age) %>% summarise(n=n())
            ggplot(domcount, aes(x=pathway, y=n, fill=age)) +
              geom_linerange(position=position_dodge(.5),
                             aes(xmin=pathway, xmax=pathway, ymin=0, ymax=n
                                  , colour=age
                                 , size=10, alpha=0.6),
                             show.legend=F) +
              geom_point(position=position_dodge(.5)
                          , aes(colour=age)
                         , size=10, show.legend=T) +
              theme_light() +
              coord_flip() +
              scale_color_manual(values = colors) +
              guides(fill=FALSE, size=FALSE, alpha=FALSE) +
              labs(y="Dominant Pathway Count Across Neighborhoods", x="", color="Age") +
              theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_text(size=12)
              )
            
            
          })
          output$multidom <- renderPlot(multidom())
          
          #multi exposure plot
          output$plot_exposure_multi <-renderPlot({
            colors <- c("Open Drain Water" = '#0F8554',
                         "Municipal Drinking Water" = '#1D6996',
                         "Raw Produce" = '#38A6A5',
                         "Floodwater" = '#5F4690',
                         "Bathing Water" = '#EDD808',
                         "Ocean" = '#994E95',
                         "Surface Water" = '#E17C05',
                         "Public Latrine" = '#74202D',
                         "Street Food" = '#CC503E')
            
            
            df.exposure %>% 
              mutate(city = replace(city, city == "Atlanta", "Atl")) %>%
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
          })
          
          #country-level map
          output$mapcountries <- renderLeaflet({
            leaflet(meta_dply) %>% 
              addTiles() %>% 
              addMarkers(lng = ~long,
                         lat = ~lat, 
                         label = paste0(meta_dply$city, ", ", meta_dply$country),
                         popup = ~country,
                         options = markerOptions(draggable = F, riseOnHover = TRUE))
          })
        
        # **************************************************************************************************
        #### Deployment Overview Tab####
          
          #neighborhood box
          boxneighb <- reactive({
            meta_full <- filter(meta_full, city %in% citychoice())
            valueBox(length(unique(meta_full$neighborhood)), "Neighborhoods", icon = icon("city"), color = "teal")
          })
          output$boxneighb <-renderValueBox(boxneighb())
          
          #sample box
          boxsamples <- reactive({
            df.ecdata <- filter(df.ecdata, city %in% citychoice())
            valueBox(nrow(df.ecdata), "Environmental Samples", icon = icon("flask"), color = "light-blue")
          })
          output$boxsamples <- renderValueBox(boxsamples())
          
          #hhsurvey box
          boxhhsurveys <- reactive({
            df.hh <- filter(df.hh, city %in% citychoice())
            valueBox(nrow(df.hh), "Household Surveys", icon=icon("clipboard"), color="aqua")
          })
          output$boxhhsurveys <- renderValueBox(boxhhsurveys())
  
          #ccsurvey box
          boxccsurveys <- reactive({
            df.cc <- filter(df.cc, city %in% citychoice())
            valueBox(nrow(df.cc), "Community Surveys", icon=icon("clipboard"), color="aqua")
          })
          output$boxccsurveys <- renderValueBox(boxccsurveys())
          
          #ssurvey box
          boxsssurveys <- reactive({
            df.sc <- filter(df.sc, city %in% citychoice())
            valueBox(nrow(df.sc), "School Surveys", icon=icon("clipboard"), color="aqua")
          })
          output$boxsssurveys <- renderValueBox(boxsssurveys())
          
          #neighborhood map
          mapneighborhoods <- reactive({
            factpal <- colorFactor("plasma", meta_neighb$city)
            df11 <- left_join(meta_neighb, meta_dply[, c(1,2,4)], by=c("deployment_id" = "id"))
            df11 <- filter(df11, city %in% citychoice())
            leaflet(df11) %>% 
              addTiles() %>%
              addCircleMarkers(lng = ~long,
                               lat = ~lat, 
                               label = paste0(df11$neighborhood, ", ", df11$city, " (", df11$country, ")"),
                               popup = ~deployment,
                               options = markerOptions(draggable = F, riseOnHover = TRUE),
                               color = ~factpal(city))
            
          })
          output$mapneighborhoods <- renderLeaflet(mapneighborhoods())
          
          
          
          #domtable
          domtable <- reactive({
            if(is.null(input$city)){
              return(NULL)
            }
              df.dominant <- filter(df.dominant, city %in% citychoice())
              df.dominant <- aggregate(pathway ~ neighborhood + age + city, data=df.dominant, paste, collapse=", ")
              
              tableColor <- getPalette2(n=length(unique(df.dominant$city)))
              
              df.dominant <- df.dominant %>%
                select(., c("city", "neighborhood", "age", "pathway")) %>%
                apply_labels(., neighborhood="Neighborhood", age="Age", pathway="Dominant Pathway(s)")
              dom_table <- datatable(df.dominant, colnames=c("City", "Neighborhood","Age", "Dominant Pathway(s)"),
                                    options = list(pageLength=25,
                                                   columnDefs = list(list(className = 'dt-center',targets = 0:4))))
              dom_table %>% formatStyle(columns= "city", target="row",
                                        background=styleEqual(unique(df.dominant$city), c((tableColor))))
            })
          output$domtable <- renderDataTable(domtable())

        # **************************************************************************************************
        #### Behavior Tab####
          output$plot_behavior <- renderPlot({
            if(is.null(citychoice()) | is.null(bxchoice())){
              return(NULL)
            }
            df.behav.city %>%
              filter(., city %in% citychoice() & sample_type %in% bxchoice()) %>%
              melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood")) %>%
              na.omit(value) %>%
              # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
              ggplot(aes(x = neighborhood, y = value, fill = variable)) + #add value labels
              geom_bar(stat = "identity") +
              facet_nested(pop ~ sample_type + city, scales = "free", space = "free") + #scales = "free_x"
              theme_bw() +
              labs(title = "Distribution of Behaviors",
                   fill = "Frequency",
                   x = "City",
                   y = "Percent") +
              # theme(strip.text.y = element_text(size = 7)) +
              scale_fill_brewer(palette="Set2") +
              theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
                    # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                    strip.text.x = element_text(size = 10),
                    strip.text.y = element_text(size = 10),
                    strip.background = element_rect(fill="grey"),
                    legend.position="bottom",
                    panel.spacing=unit(0.5,"lines")) + 
              scale_y_continuous(labels = scales::percent)
                  
                  
                  
          })
          
          output$plot_behavior_city <- renderPlot({
            if(is.null(citychoice()) | is.null(bxchoice())){
              return(NULL)
            }
            df.behav.all %>%
              filter(., city %in% citychoice() & sample_type %in% bxchoice()) %>%
              melt(., id.vars = c("city", "sample_type", "pop")) %>%
              na.omit(value) %>%
              # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
              ggplot(aes(x = city, y = value, fill = variable)) + #add value labels
              geom_bar(stat = "identity") +
              facet_nested(pop ~ sample_type, scales = "free", space = "free") + #scales = "free_x"
              theme_bw() +
              labs(title = "Distribution of Behaviors",
                   fill = "Frequency",
                   x = "City",
                   y = "Percent") +
              # theme(strip.text.y = element_text(size = 7)) +
              scale_fill_brewer(palette="Set2") +
              theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
                    # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                    strip.text.x = element_text(size = 10),
                    strip.text.y = element_text(size = 10),
                    strip.background = element_rect(fill="grey"),
                    legend.position="bottom",
                    panel.spacing=unit(0.5,"lines")) + 
              scale_y_continuous(labels = scales::percent)
            
            
            
          })

        # **************************************************************************************************
        #### Contamination Tab####
          
          #ecoli map
          ecoli_map <- reactive({
            df.ecdata<- filter(df.ecdata, city %in% citychoice() & sample_type_name %in% samplechoice())
            
            df <- left_join(df.ecdata, df.col[, c("col_UID", "lat" = "X_col_location_latitude", "X_col_location_longitude")],
                            by = c("UID" = "col_UID"))
            
            colnames(df)[colnames(df)=="X_col_location_latitude"] <- "lat"
            colnames(df)[colnames(df)=="X_col_location_longitude"] <- "lon"
            
            df <- df %>% filter_all(all_vars(!is.na(std_ec_conc)))
            
            #Setting standard cutoffs
            cutoff <- c(0, .25, .50, 0.75, 1)
            df <- df %>% mutate(dot_label = case_when(std_ec_conc <= cutoff[[2]] ~ 1,
                                                      std_ec_conc > cutoff[[2]] & std_ec_conc <= cutoff[[3]] ~ 2,
                                                      std_ec_conc > cutoff[[3]] & std_ec_conc <= cutoff[[4]] ~ 3,
                                                      std_ec_conc > cutoff[[4]] & std_ec_conc <= cutoff[[5]] ~ 4))
            df <- df %>% mutate(dot_color = case_when(dot_label == 1 ~ "green",
                                                      dot_label == 2 ~ "yellow",
                                                      dot_label == 3 ~ "orange",
                                                      dot_label == 4 ~ "red"))
            cutoff1 <- c("Minimal", "Low", "Medium", "High")
            # cutoff1 <- sprintf("%.2f", round(cutoff1, 2))
            color1 <- c("green", "yellow", "orange", "red")
            
            # outlier 
            df$lat[df$UID == "11_02_1018"] <- 12.915
            df$lon[df$UID == "11_02_1018"] <- 79.141
            
            factpal <- colorFactor("plasma", meta_neighb$deployment)
            df11 <- left_join(meta_neighb, meta_dply[, c(1,2,4)], by=c("deployment_id" = "id"))
            df11 <- filter(df11, city %in% citychoice())
            
            leaflet() %>% 
              addTiles() %>%
              # addProviderTiles(providers$MtbMap) %>%
              # addProviderTiles(providers$Stamen.TonerLines,
              #                  options = providerTileOptions(opacity = 0.35)) %>%
              # addProviderTiles(providers$Stamen.TonerLabels) %>%
              addCircleMarkers(data=df11, lng = ~long,
                               lat = ~lat, 
                               label = paste0(df11$neighborhood, ", ", df11$city, " (", df11$country, ")"),
                               popup = ~deployment,
                               options = markerOptions(draggable = F, riseOnHover = TRUE),
                               color = ~factpal(deployment)) %>%
              addCircles(data=df, lng=~lon, lat=~lat,
                         popup = paste0(df$sample_type_name, ", ", sprintf("%.2f", round(df$std_ec_conc, 2)), " Normalized E.coli", " (log10)"),
                         weight = 3, radius=50, color=~dot_color, stroke = TRUE, fillOpacity = 1) %>%
              addLegend("bottomright", colors = color1, labels = cutoff1,
                        title = "Fecal Contamination Risk Scale") %>%
              addScaleBar("bottomleft")
          })
          output$mapecoli <- renderLeaflet(ecoli_map())
          
          #ecoli ridges plot
          ecoli_plot <- reactive({
            if(is.null(input$city) | is.null(input$sample)){
              return(NULL)
            }
            
            df.ecdata <- df.ecdata %>% filter(., city %in% citychoice() & sample_type_name %in% samplechoice())
            df2 <- df.ecdata %>%
              group_by(sample_type_name) %>%
              summarise(mean.cont = mean(log10(ec_conc), na.rm=TRUE))
            
            df.ecdata %>%
              ggplot(., aes(y=factor(hood, levels=(unique(hood))), x=log10(ec_conc))) +
              geom_density_ridges(aes(fill=city), quantile_lines=TRUE, quantiles=2, panel_scaling=FALSE
                                  #comment this chunk out to get rid of lines: from here
                                  ,
                                  jittered_points = TRUE,
                                  # position = position_points_jitter(width = 0.05, height = 0),
                                  # point_shape = '|',
                                  point_size = 0.75,
                                  point_alpha = 0.5,
                                  alpha = 0.7
                                  #to here
                                  ) +
              geom_vline(data=df2, aes(xintercept=mean.cont, color="red"), show.legend=FALSE) +
              # geom_point(aes(color=city) ) +
              # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
              facet_wrap( ~ sample_type_name, scales = "fixed", nrow=4, ncol=3) +
              labs(fill = "City",
                   x = "E. coli (Log10)",
                   y = "") +
              scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
              theme_bw() +
              theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2),
                    axis.text=element_text(size=8),
                    strip.background = element_rect(fill="white"),
                    legend.position="bottom",
                    legend.justification="center") +
              colScale
            
          })
          output$plot_ecoli <- renderPlot(ecoli_plot())
          
        # **************************************************************************************************  
        #### Exposure  Tab####
          
          # city choice text
          output$citychoice <- renderText(paste0(citychoice(), collapse=", "))
          
          plot_exposure_all <- reactive({
                  colors <- c("Open Drain Water" = '#0F8554',
                              "Municipal Drinking Water" = '#1D6996',
                              "Raw Produce" = '#38A6A5',
                              "Floodwater" = '#5F4690',
                              "Bathing Water" = '#EDD808',
                              "Ocean" = '#994E95',
                              "Surface Water" = '#E17C05',
                              "Public Latrine" = '#74202D',
                              "Street Food" = '#CC503E')
                  
                  
                  # "FW=#5F4690", "DW=#1D6996", "SW=#38A6A5","OD=#0F8554","#73AF48","BW=#EDAD08",PR="#E17C05",
                  # SF="#CC503E","#94346E","PL=#6F4070","OW=#994E95","#666666"
                  
                  if(is.null(citychoice())){
                    return(NULL)
                  }
                  
                  df.exposure %>% 
                    filter(., city %in% citychoice()) %>%
                          ggplot(., aes(x=factor(neighborhood), y=perDose, fill=pathway) ) +
                          geom_bar(stat="identity") +
                          facet_grid(age~ city, scales = "free_x", space = "free_x") +
                          labs(fill = "Pathway",
                               x = "Neighborhood",
                               y = "Total Exposure (log10)") +
                          theme_bw() +
                          theme(#legend.position="bottom",
                                  strip.text.x = element_text(size = 12),
                                  strip.text.y = element_text(size = 12),
                                  strip.background = element_rect(fill="white"),
                                  axis.text.x = element_text(angle=90)) +
                          scale_fill_manual(values = colors)
          })
          output$plot_exposure_all <- renderPlot({plot_exposure_all()})
          
  
          #Top 3 Common Dominant pathways for Adults / Children based on input
          commondom1 <- reactive({
            meta_full <- filter(meta_full, city %in% citychoice())
            city.domcount <- df.dominant %>%
              filter(., city %in% citychoice()) %>%
              group_by(pathway, age) %>%
              summarise(n=n())
            nhoods <- length(unique(meta_full$neighborhood))
            city.domcount <- city.domcount %>%
              filter(., age %in% agechoice()) %>%
              mutate(., percent=ceiling((n/nhoods)*100)) %>%
              arrange(., desc(n))
  
            if(is.na(city.domcount$pathway[1])){
              return(NULL)
            }
            
            widgetUserBoxCasey(
              title = tags$h3(city.domcount$pathway[1], tags$br(),
                              "(", city.domcount$n[1], ")",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway==city.domcount$pathway[1])],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway==city.domcount$pathway[1])],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway==city.domcount$pathway[1])],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway==city.domcount$pathway[1])],
              boxToolSize="xs"
            )
  
          })
          output$dom1 <- renderUI(commondom1())
  
          commondom2 <- reactive({
            meta_full <- filter(meta_full, city %in% citychoice())
            city.domcount <- df.dominant %>%
              filter(., city %in% citychoice()) %>%
              group_by(pathway, age) %>%
              summarise(n=n())
            nhoods <- length(unique(meta_full$neighborhood))
            city.domcount <- city.domcount %>%
              filter(., age %in% agechoice()) %>%
              mutate(., percent=ceiling((n/nhoods)*100)) %>%
              arrange(., desc(n))
            
            if(is.na(city.domcount$pathway[2])){
              return(NULL)
            }
            
            widgetUserBoxCasey(
              title = tags$h3(city.domcount$pathway[2], tags$br(),
                              "(", city.domcount$n[2], ")",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway==city.domcount$pathway[2])],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway==city.domcount$pathway[2])],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway==city.domcount$pathway[2])],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway==city.domcount$pathway[2])],
              boxToolSize="xs"
            )
          })
          output$dom2 <- renderUI(commondom2())
          
          commondom3 <- reactive({
            meta_full <- filter(meta_full, city %in% citychoice())
            city.domcount <- df.dominant %>%
              filter(., city %in% citychoice()) %>%
              group_by(pathway, age) %>%
              summarise(n=n())
            nhoods <- length(unique(meta_full$neighborhood))
            city.domcount <- city.domcount %>%
              filter(., age %in% agechoice()) %>%
              mutate(., percent=ceiling((n/nhoods)*100)) %>%
              arrange(., desc(n))
            
            if(is.na(city.domcount$pathway[3])){
              return(NULL)
            }
            
            widgetUserBoxCasey(
              title = tags$h3(city.domcount$pathway[3], tags$br(),
                              "(", city.domcount$n[3], ")",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway==city.domcount$pathway[3])],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway==city.domcount$pathway[3])],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway==city.domcount$pathway[3])],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway==city.domcount$pathway[3])],
              boxToolSize="xs"
            )
            
            
          })
          output$dom3 <- renderUI(commondom3())
          
          #Below are tests to check the boxes and figures
          output$test1 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("DW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Municipal Drinking Water")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Municipal Drinking Water")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Municipal Drinking Water")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Municipal Drinking Water")],
              boxToolSize="xs"
            )
          })
          output$test2 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("BW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Bathing Water")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Bathing Water")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Bathing Water")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Bathing Water")],
              boxToolSize="xs"
            )
          })
          output$test3 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("OD",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Open Drain Water")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Open Drain Water")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Open Drain Water")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Open Drain Water")],
              boxToolSize="xs"
            )
          })
          output$test4 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("0DW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Other Drinking Water")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Other Drinking Water")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Other Drinking Water")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Other Drinking Water")],
              boxToolSize="xs"
            )
          })
          output$test5 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("OW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Ocean")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Ocean")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Ocean")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Ocean")],
              boxToolSize="xs"
            )
          })
          output$test6 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("FW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Floodwater")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Floodwater")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Floodwater")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Floodwater")],
              boxToolSize="xs"
            )
          })
          output$test7 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("SW",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Surface Water")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Surface Water")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Surface Water")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Surface Water")],
              boxToolSize="xs"
            )
          })
          output$test8 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("SF",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Street Food")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Street Food")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Street Food")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Street Food")],
              boxToolSize="xs"
            )
          })
          output$test9 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("PR",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Raw Produce")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Raw Produce")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Raw Produce")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Raw Produce")],
              boxToolSize="xs"
            )
          })
          output$test10 <- renderUI({
            widgetUserBoxCasey(
              title = tags$h3("LS",
                              style="text-align: center;color: rgb(255, 255, 255);"),
              subtitle = NULL,
              type = 2,
              width = 12,
              src = pathway.info$icon[which(pathway.info$pathway=="Public Latrine")],
              background = TRUE,
              backgroundUrl = pathway.info$bg[which(pathway.info$pathway=="Public Latrine")],
              closable = FALSE,
              tags$h5(pathway.info$front_text[which(pathway.info$pathway=="Public Latrine")],
                      style="text-align: center; color: rgb(62, 0, 0);"),
              footer = pathway.info$back_text[which(pathway.info$pathway=="Public Latrine")],
              boxToolSize="xs"
            )
          })
          
}

# Run the application 
shinyApp(ui = ui, server = server)

