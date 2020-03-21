library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
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


# Define UI for application that draws a histogram
#### UI ####
ui <- fluidPage(
        dashboardPage(
                dashboardHeader(title = "SaniPath Dashboard"),
                dashboardSidebar(
                        sidebarMenu(
                          h5("SaniPath Multi-City Comparison"),
                          menuItem("Multi-City Comparison", tabName = "tabmulti", icon = icon("globe-africa")),
                          h5(textOutput("citychoice")),
                          menuItem("Deployment Overview", tabName = "taboverview", icon = icon("home")),
                          menuItem("Environmental Contamination", tabName = "tabenviron", icon = icon("leaf")),
                          menuItem("Behavior Frequency", tabName = "tabbehav", icon = icon("pie-chart")),
                          menuItem("Exposure", tabName = "tabexposure", icon = icon("asterisk")),
                          h4("Select City/Cities"),
                        checkboxGroupInput("city", NULL, c(cities), selected="Accra")
                )
                ),
                dashboardBody(
                        tabItems(
                                # **************************************************************************************************
                                ##### Tab 1: MultiCity Comparison ####
                                tabItem(tabName = "tabmulti",
                                        p("Countries and Cities"),
                                        leafletOutput("mapcountries"),
                                        hr(),
                                        h2("Overview"),
                                        p("Here is an overview of the Current SaniPath deployment statistics."),
                                        
                                        fluidRow(
                                          # valueBoxOutput("boxdply"),
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
                                        fluidRow(
                                          plotOutput("plot_exposure_multi")
                                        ),
                                        fluidRow(
                                          h2("dominant pathway count"),
                                          plotOutput("multidom")
                                        )
                                ),
                                
                                # **************************************************************************************************
                                #### Tab 2: Deployment Overview ####
                                tabItem(tabName = "taboverview",
                                        h3("Neighborhoods"),
                                        leafletOutput("mapneighborhoods"),
                                        fluidRow(column(7,
                                                h3("Deployment by the Numbers"),
                                                fluidRow(valueBoxOutput("boxneighb"),
                                                valueBoxOutput("boxsamples")),
                                                fluidRow(valueBoxOutput("boxhhsurveys"),
                                                valueBoxOutput("boxccsurveys")),
                                                fluidRow(valueBoxOutput("boxsssurveys"))
                                                ),
                                                column(5,
                                                h3("Dominant Pathways"),
                                                dataTableOutput("domtable")
                                                )
                                        )

                                ),

                                # **************************************************************************************************
                                #### Tab 3: Environmental Samples ####
                                tabItem(tabName = "tabenviron",
                                        fluidRow(
                                          wellPanel(style="padding: 10px;",
                                          h2("Environmental Contamination"),
                                          checkboxGroupInput("sample", NULL, c(samples), inline=TRUE)
                                        )),
                                        fluidRow(
                                          column(6,
                                          wellPanel(style="padding: 10px;",
                                            h4("Map of samples and sample contamination score"),
                                            leafletOutput("mapecoli", height="600px")
                                          )),
                                          column(6,
                                          wellPanel(style="padding: 10px;",
                                            h4("E. coli contamination by sample type", align="center"),
                                            plotOutput("plot_ecoli", height="600px"),
                                            HTML("<p align=center> <i> <font size=2 color=darkred>
                                                 NOTE: Units are as Log10 E. coli/100mL except for the following: Street Food and Raw Produce (Log10 E. coli/serving),
                                                 Latrine Swabs (Log10 E. coli/swab), Soil (Log10 E. coli/gram)
                                                 </font> </i> </p>")
                                            ))
                                        )

                                        
                                        
                                ),
                                # **************************************************************************************************
                                #### Tab 4: Behavioral frequency ####
                                tabItem(tabName = "tabbehav",
                                        h2("Behavior Frequency"),
                                        
                                        p("Static Graph:"),
                                        plotOutput("plot_behavior", height = "700px"), 
                                        hr(),
                                        
                                        p("Interactive Graph:"),
                                        plotlyOutput("plot_behavior1"), #, width = "800px", height = "417px" )
                                        hr(),
                                        
                                        plotlyOutput("plot_behavior2") 
                                        
                                        
                                ),
                                # **************************************************************************************************
                                #### Tab 5: Exposure ####
                                tabItem(tabName = "tabexposure",
                                        h2("Exposure and Dominant Pathways"),
                                        plotOutput("plot_exposure_all", height = "700px")
                                        
                                )

                        ))
        )
        
   
        
        
)


# Define server logic required to draw a histogram
#### Server ####
server <- function(input, output, session) {
        # **************************************************************************************************
        #### City Choice Important Input ####
        citychoice <- reactive({input$city})
        
        # **************************************************************************************************
        #### Sample Choice Important Input for Reactive CheckboxGroupInput on Sample Slide####
        observeEvent(input$city,{
          df.ecdata <- filter(df.ecdata, city %in% citychoice())
          samples <- as.character(unique(df.ecdata$sample_type_name))
          updateCheckboxGroupInput(session, "sample", choices=c(samples), selected=samples[[1]], inline=TRUE)
        })
        samplechoice <- reactive({input$sample})
        
        # **************************************************************************************************
        #### City Choice Important Input ####
        output$citychoice <- renderText(paste0("Deployment Overview in ", citychoice(),"."))
        
        #multicity box
        output$multiboxcity <- renderValueBox({
                valueBox(
                        length(unique(meta_dply$city)), "Cities", icon = icon("info-sign", lib = "glyphicon"),
                        color = "teal")
        })
        
        #neighborhood box
        boxneighb <- reactive({
          meta_full <- filter(meta_full, city %in% citychoice())
          valueBox(length(unique(meta_full$neighborhood)), "Neighborhoods", icon = icon("city"), color = "teal")
        })
        output$boxneighb <-renderValueBox(boxneighb())
        
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
        
        #multisamples box
        output$multiboxsamples <- renderValueBox({
          valueBox(
            nrow(df.ecdata), "Environmental Samples", icon = icon("flask"),
            color = "light-blue")
        })
        
        output$multiboxhhsurveys <- renderValueBox({
          valueBox(nrow(df.hh), "Household Surveys", icon=icon("clipboard"), color="aqua")
        })
        
        output$multiboxccsurveys <- renderValueBox({
          valueBox(nrow(df.cc), "Community Surveys", icon=icon("clipboard"), color="aqua")
        })
        
        output$multiboxsssurveys <- renderValueBox({
          valueBox(nrow(df.sc), "School Surveys", icon=icon("clipboard"), color="aqua")
        })
        
        
        #multi dom pie chart
        multidom <- reactive({
          if(is.null(input$city)){
            return(NULL)
          }
          domcount <- df.dominant %>% group_by(pathway, age) %>% summarise(n=n())
          ggplot(domcount, aes(x=pathway, y=n, fill=age)) +
            geom_segment(aes(x=pathway, xend=pathway, y=0, yend=n, colour=age), show.legend=F) +
            geom_point(aes(colour=age), size=4, alpha=0.6) +
            theme_light() +
            coord_flip() +
            labs(y="Count of neighborhoods with each pathway as dominant", x="") +
            theme(
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              axis.ticks.y = element_blank()
            )
          
          
        })
        output$multidom <- renderPlot(multidom())
        
        
        
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
                                  options = list(dom = 't', pageLength=30,columnDefs = list(list(className = 'dt-center',
                                                                                                targets = 0:4))))
            dom_table %>% formatStyle(columns= "city", target="row",
                                      background=styleEqual(unique(df.dominant$city), c((tableColor))))
          })

        output$domtable <- renderDataTable(domtable())
        
        # **************************************************************************************************
        
        colourCount = length(unique(meta_dply$city))
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))
        colScale <- scale_fill_manual(values=getPalette(colourCount))

        
        output$plot_ecoli_plotly1 <- renderPlotly({
                df.ecdata %>% 
                        filter(sample_type_name %in% c("Drinking Water", "Other Drinking Water", "Bathing Water", "Floodwater", "Open Drains" )) %>%
                        ggplot(., aes(x=factor(city), y=log10(ec_conc), fill=city)) +
                        geom_boxplot() +
                        # geom_point(aes(color=city) ) +
                        facet_grid( ~ sample_type_name, scales = "free", space = "free") + 
                        ylim(c(-1,10)) +
                        labs(fill = "City",
                             x = "",
                             y = "E.coli (Log10)") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              axis.text=element_text(size=8),
                              strip.background = element_rect(fill="white")) +
                        colScale
                })
        
        output$plot_ecoli_plotly2 <- renderPlotly({
                df.ecdata %>% 
                        filter(sample_type_name %in% c("Surface Water", "Oceans", "Raw Produce","Street Food", "Public Latrine", "Particulate")) %>%
                        mutate(sample_type_name = factor(sample_type_name, 
                                                    levels=c("Oceans", "Surface Water", "Public Latrine", "Particulate", "Raw Produce","Street Food"))) %>%
                        ggplot(., aes(x=factor(city), y=log10(ec_conc), fill=city)) +
                        geom_boxplot() +
                        # geom_point(aes(color=city) ) +
                        facet_grid( ~ sample_type_name, scales = "free", space = "free") + 
                        ylim(c(-1,10)) +
                        labs(fill = "City",
                             x = "",
                             y = "E.coli (Log10)") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              axis.text=element_text(size=8),
                              strip.background = element_rect(fill="white")) +
                        colScale
        })
        
        
        # **************************************************************************************************
        output$plot_behavior <- renderPlot({
                df.behav %>% 
                        melt(., id.vars = c("city", "sample_type", "pop")) %>%
                        na.omit(value) %>%
                        ggplot(., aes(x = city, y = value, fill = variable)) + 
                        geom_bar(stat = "identity") +
                        facet_grid(pop ~ sample_type, scales = "free", space = "free") + 
                        theme_bw() +
                        labs(fill = "Frequency",
                             x = "",
                             y = "Percent") +
                        # theme(strip.text.y = element_text(size = 7)) +
                        scale_fill_brewer(palette="Set2") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              strip.text.x = element_text(size = 8),
                              strip.text.y = element_text(size = 8),
                              strip.background = element_rect(fill="white"),
                              legend.position="bottom") + 
                        scale_y_continuous(labels = scales::percent) 
                
                
                
        })
        
        # **************************************************************************************************
        output$plot_behavior1 <- renderPlotly({
                df.behav %>% filter(sample_type %in% c("Drinking Water", "DW, other", "Bathing Water", "Floodwater", "Open Drains" )) %>%
                        melt(., id.vars = c("city", "sample_type", "pop")) %>%
                        na.omit(value) %>%
                        ggplot(., aes(x = city, y = value, fill = variable, group = 1,
                                      text = paste('City: ', city,
                                                   '<br>Percent:', round((value*100),2), 
                                                   '<br>Frequency:', variable)
                                      )) + 
                        geom_bar(stat = "identity") +
                        facet_grid(pop ~ sample_type, scales = "free", space = "free") + 
                        theme_bw() +
                        labs(fill = "Frequency",
                             x = "",
                             y = "Percent") +
                        # theme(strip.text.y = element_text(size = 7)) +
                        scale_fill_brewer(palette="Set2") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              strip.text.x = element_text(size = 8),
                              strip.text.y = element_text(size = 8),
                              strip.background = element_rect(fill="white"),
                              legend.position="bottom") + 
                        scale_y_continuous(labels = scales::percent) -> p
                
                
                ggplotly(p, tooltip = "text")

                
                
        })
        # **************************************************************************************************
        output$plot_behavior2 <- renderPlotly({
                df.behav %>% mutate(sample_type = replace(sample_type, sample_type=="Ocn", "Ocean")) %>%
                        filter(sample_type %in% c("Surface Water", "Ocean", "Raw Produce","Street Food", "Public Latrine")) %>%
                        mutate(sample_type = factor(sample_type,
                                                    levels=c("Ocean","Surface Water",  "Public Latrine", "Raw Produce","Street Food"))) %>%
                        melt(., id.vars = c("city", "sample_type", "pop")) %>%
                        na.omit(value) %>%
                        ggplot(., aes(x = city, y = value, fill = variable, group = 1,
                                      text = paste('City: ', city,
                                                   '<br>Percent:', round((value*100),2), 
                                                   '<br>Frequency:', variable)
                        )) + 
                        geom_bar(stat = "identity") +
                        facet_grid(pop ~ sample_type, scales = "free", space = "free") + 
                        theme_bw() +
                        labs(fill = "Frequency",
                             x = "",
                             y = "Percent") +
                        # theme(strip.text.y = element_text(size = 7)) +
                        scale_fill_brewer(palette="Set2") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              strip.text.x = element_text(size = 8),
                              strip.text.y = element_text(size = 8),
                              strip.background = element_rect(fill="white"),
                              legend.position="bottom") + 
                        scale_y_continuous(labels = scales::percent) -> p
                
                ggplotly(p, tooltip = "text")
                
        })
        
        # **************************************************************************************************
        plot_exposure_all <- reactive({
                colors <- c("Open Drain Water" = '#e41a1c',
                            "Municipal Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Ocean" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
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
                                # strip.text.y = element_text(size = 12),
                                strip.background = element_rect(fill="white")) +
                        scale_fill_manual(values = colors)
        })
        output$plot_exposure_all <- renderPlot({plot_exposure_all()})
        
        
        output$plot_exposure_multi <-renderPlot({
          colors <- c("Open Drain Water" = '#e41a1c',
                      "Municipal Drinking Water" = '#377eb8',
                      "Raw Produce" = '#4daf4a',
                      "Floodwater" = '#984ea3',
                      "Bathing Water" = '#ff7f00',
                      "Ocean" = '#ffff33',
                      "Surface Water" = '#a65628',
                      "Public Latrine" = '#f781bf',
                      "Street Food" = '#999999')
          
          
          df.exposure %>% 
            mutate(city = replace(city, city == "Atlanta", "Atl")) %>%
            ggplot(., aes(x=factor(neighb_id), y=perDose, fill=pathway) ) +
            geom_bar(stat="identity") +
            facet_grid(age~ city, scales = "free_x", space = "free_x") +
            labs(fill = "Pathway",
                 x = "Neighborhood",
                 y = "Total Exposure (log10)") +
            theme_bw() +
            theme(#legend.position="bottom",
              strip.text.x = element_text(size = 12),
              # strip.text.y = element_text(size = 12),
              strip.background = element_rect(fill="white")) +
            scale_fill_manual(values = colors)
        })
        
        
        
        # **************************************************************************************************       
        output$mapcountries <- renderLeaflet(
                leaflet(meta_dply) %>% 
                        addTiles() %>% 
                        addMarkers(lng = ~long,
                                   lat = ~lat, 
                                   label = paste0(meta_dply$city, ", ", meta_dply$country),
                                   popup = ~country,
                                   options = markerOptions(draggable = F, riseOnHover = TRUE))
        )
        # **************************************************************************************************
        
        mapneighborhoods <- reactive({
          factpal <- colorFactor("plasma", meta_neighb$deployment)
          df11 <- left_join(meta_neighb, meta_dply[, c(1,2,4)], by=c("deployment_id" = "id"))
          df11 <- filter(df11, city %in% citychoice())
          leaflet(df11) %>% 
            addTiles() %>%
            addCircleMarkers(lng = ~long,
                             lat = ~lat, 
                             label = paste0(df11$neighborhood, ", ", df11$city, " (", df11$country, ")"),
                             popup = ~deployment,
                             options = markerOptions(draggable = F, riseOnHover = TRUE),
                             color = ~factpal(deployment))
          
        })
        output$mapneighborhoods <- renderLeaflet(mapneighborhoods())

        
        

        # Casey New Things
        ecoli_map <- reactive({
          df.ecdata<- filter(df.ecdata, city %in% citychoice() & sample_type_name %in% samplechoice())
          
          df <- left_join(df.ecdata, df.col[, c("col_UID", "lat" = "X_col_location_latitude", "X_col_location_longitude")],
                          by = c("UID" = "col_UID"))
          
          colnames(df)[colnames(df)=="X_col_location_latitude"] <- "lat"
          colnames(df)[colnames(df)=="X_col_location_longitude"] <- "lon"
          
          ###### Commenting out wolfgang's code
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
          cutoff1 <- c(paste0("<=", cutoff[[2]]), paste0(cutoff[[2]], " - ", cutoff[[3]]),
                       paste0(cutoff[[3]], " - ", cutoff[[4]]), paste0(cutoff[[4]], " - 1"))
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
                      title = "Ecoli Value Cutoff") %>%
            addScaleBar("bottomleft")
          
          
          
          
          
        })
        output$mapecoli <- renderLeaflet(ecoli_map())
        
        
        # Plotting the density ridge plots for ecoli
        ecoli_plot <- reactive({
          if(is.null(input$city)){
            return(NULL)
          }
          
          
          colourCount = length(unique(meta_dply$city))
          getPalette = colorRampPalette(brewer.pal(9, "Set3"))
          colScale <- scale_fill_manual(values=getPalette(colourCount))
          
          df.ecdata %>% filter(., city %in% citychoice() & sample_type_name %in% samplechoice()) %>%
            ggplot(., aes(y=factor(hood), x=log10(ec_conc))) +
            geom_density_ridges(aes(fill=city), quantile_lines=TRUE, quantiles=2, panel_scaling=FALSE) +
            # geom_point(aes(color=city) ) +
            # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
            facet_wrap( ~ sample_type_name, scales = "fixed", nrow=4, ncol=3) +
            labs(fill = "City",
                 x = "E. coli (Log10)",
                 y = "") +
            theme_bw() +
            scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                  axis.text=element_text(size=8),
                  strip.background = element_rect(fill="white"),
                  legend.position="bottom") +
            colScale
          
        })
        output$plot_ecoli <- renderPlot(ecoli_plot())


        
   
   
        
}

# Run the application 
shinyApp(ui = ui, server = server)

