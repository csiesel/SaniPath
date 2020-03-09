library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)
library(RColorBrewer)


# load data
source("helper_dataload.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
        dashboardPage(
                dashboardHeader(title = "SaniPath Dashboard"),
                dashboardSidebar(
                        sidebarMenu(
                        menuItem("Overview", tabName = "taboverview", icon = icon("home")),
                        menuItem("Environmental Contamination", tabName = "tabenviron", icon = icon("leaf")),
                        menuItem("Behavior Frequency", tabName = "tabbehav", icon = icon("pie-chart")),
                        menuItem("Exposure", tabName = "tabexposure", icon = icon("asterisk")),
                        menuItem("Map", tabName = "tabmap", icon = icon("map-marker"))
                )
                ),
                dashboardBody(
                        tabItems(
                                # **************************************************************************************************
                                # Tab 1 
                                tabItem(tabName = "taboverview",
                                        h2("Overview"),
                                        p("Here is an overview of the Current SaniPath deployment statistics."),
                                        
                                        fluidRow(
                                                # valueBoxOutput("boxdply"),
                                                valueBoxOutput("boxcountries"),
                                                valueBoxOutput("boxcity"),
                                                valueBoxOutput("boxneighb")
                                        ),
                                        
                                        fluidRow(
                                                valueBoxOutput("boxsamples")
                                                ),
                                        fluidRow(
                                                valueBoxOutput("boxhhsurveys"),
                                                valueBoxOutput("boxccsurveys"),
                                                valueBoxOutput("boxsssurveys")
                                        )
                                ),
                                
                                # **************************************************************************************************
                                # # Tab 2 
                                # tabItem(tabName = "tabstats",
                                #         h2("Statistics"),
                                #         p("asdf")
                                # ),
                                # **************************************************************************************************
                                # Tab 3 
                                tabItem(tabName = "tabenviron",
                                        h2("Environmental Contamination"),
                                        
                                        p("Static Graph:"),
                                        plotOutput("plot_ecoli", height = "500px"),
                                        hr(),
                                        
                                        p("Interactive Graph:"),
                                        plotlyOutput("plot_ecoli_plotly1"),
                                        hr(),
                                        
                                        plotlyOutput("plot_ecoli_plotly2")
                                        
                                        
                                ),
                                # **************************************************************************************************
                                # Tab 4 
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
                                # Tab 5 
                                tabItem(tabName = "tabexposure",
                                        h2("Exposure and Dominant Pathways"),
                                        
                                        p("Static Graph, Adults and Children:"),
                                        plotOutput("plot_exposure_all", height = "700px"),
                                        hr(),
                                        
                                        p("Static Graph, Adults:"),
                                        plotOutput("plot_exposure_adults", height = "500px"),
                                        hr(),
                                        
                                        p("Interactive Graph:"),
                                        plotlyOutput("plot_exposure_adults_plotly", height = "500px"),
                                        
                                        hr(),
                                        plotlyOutput("plot_exposure_bubbles", height = "700px"), #, width = "75%")
                                        
                                        hr(),
                                        p("Static Graph, Children:"),
                                        plotOutput("plot_exposure_children", height = "500px"),
                                        hr(),
                                        
                                        p("Interactive Graph:"),
                                        plotlyOutput("plot_exposure_children_plotly", height = "500px"),
                                        
                                        hr(),
                                        plotlyOutput("plot_exposure_bubbles_c", height = "700px")
                                        
                                        

                                        
                                        
                                ), 
                                # **************************************************************************************************
                                # Tab 6 
                                tabItem(tabName = "tabmap",
                                        h2("Map"),
                                        p("interactive maps"),
                                        hr(),
                                        
                                        p("Countries and Cities"),
                                        leafletOutput("mapcountries"),
                                        hr(), 
                                        
                                        p("Neighborhoods"),
                                        leafletOutput("mapneighborhoods"),
                                        hr(), 
                                        
                                        p("Environmental Samples, Ecoli"),
                                        leafletOutput("mapecoli"),
                                        hr()
                                )
                        ))
        )
        
   
        
        
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        # **************************************************************************************************
        output$boxdply <- renderValueBox({
                valueBox(
                        nrow(meta_dply), "Deployments", icon = icon("info-sign", lib = "glyphicon"),
                        color = "teal")
        })
        
        output$boxcity <- renderValueBox({
                valueBox(
                        length(unique(meta_dply$city)), "Cities", icon = icon("info-sign", lib = "glyphicon"),
                        color = "teal")
        })
        
        output$boxneighb <- renderValueBox({
                valueBox(
                        length(unique(meta_neighb$neighborhood)), "Neighborhoods", icon = icon("list"),
                        color = "teal")
        })
        
        output$boxcountries <- renderValueBox({
                valueBox(
                        length(unique(meta_dply$country)), "Countries", icon = icon("flag", lib = "glyphicon"),
                        color = "teal")
        })
        
        output$boxsamples <- renderValueBox({
                valueBox(
                        nrow(df.ecdata), "Environmental Samples", icon = icon("flask"),
                        color = "light-blue")
        })
        
        output$boxhhsurveys <- renderValueBox({
                valueBox(
                        "3786", "Household Surveys", icon = icon("clipboard"),
                        color = "aqua")
        })
        
        output$boxccsurveys <- renderValueBox({
                valueBox(
                        "96", "Community Surveys", icon = icon("clipboard"),
                        color = "aqua")
        })
        
        output$boxsssurveys <- renderValueBox({
                valueBox(
                        "92", "School Surveys", icon = icon("clipboard"),
                        color = "aqua")
        })
        
 
        
        # **************************************************************************************************
        
        colourCount = length(unique(meta_dply$city))
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))
        colScale <- scale_fill_manual(values=getPalette(colourCount))
        # myColors <- brewer.pal(13, "Set1")   #ifelse(prop == TRUE, "Set2", "Set1")
        # names(myColors) <- unique(meta_dply$city)
        # colScale <- scale_fill_manual(values = myColors)
        
        
        output$plot_ecoli <- renderPlot({
                df.ecdata %>% filter(sample_type != 33) %>%
                ggplot(., aes(x=factor(city), y=log10(ec_conc))) +
                        geom_boxplot(aes(fill=city) ) +
                        # geom_point(aes(color=city) ) +
                        facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") + 
                        labs(fill = "City",
                             x = "",
                             y = "E.coli (Log10)") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                              axis.text=element_text(size=8),
                              strip.background = element_rect(fill="white")) +
                        colScale
                })
        
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
        output$plot_exposure_adults_plotly <- renderPlotly({
                colors <- c("Open Drains" = '#e41a1c',
                            "Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Oceans" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
                df.exposure %>% filter(age == "Adults") %>%
                        mutate(city = replace(city, city == "Atlanta", "Atl"),
                               city = replace(city, city == "Lusaka", "Lka")) %>%
                        ggplot(., aes(x=neighb_pop_label, y=perDose, fill=pathway, group = 1,
                                      text = paste('City: ', city,
                                                   '<br>Neighborhood:', neighborhood,
                                                   '<br>Neighborhood ID:', neighb_id, 
                                                   '<br>Dose:', round(perDose,2), 
                                                   '<br>Pathway:', pathway)
                                      ) ) +
                        geom_bar(stat="identity") +
                        geom_vline(aes(xintercept = 7.5)) +
                        geom_vline(aes(xintercept = 8.5)) +
                        geom_vline(aes(xintercept = 18.5)) +
                        geom_vline(aes(xintercept = 23.5)) +
                        geom_vline(aes(xintercept = 27.5)) +
                        geom_vline(aes(xintercept = 28.5)) +
                        geom_vline(aes(xintercept = 32.5)) +
                        geom_vline(aes(xintercept = 37.5)) +
                        # facet_grid(~ city, scales = "free_x", space = "free_x") +
                        labs(fill = "Pathway",
                             x = "Neighborhood",
                             y = "Total Exposure (log10)") +
                        theme_bw() +
                        theme(legend.position="bottom",
                              axis.text.x = element_text(angle = 90),
                              strip.text.x = element_text(size = 12),
                              # strip.text.y = element_text(size = 12),
                              strip.background = element_rect(fill="white")) +
                        scale_fill_manual(values = colors) -> p
                ggplotly(p, tooltip = "text")
                
        })
        
        # **************************************************************************************************
        output$plot_exposure_adults <- renderPlot({
                colors <- c("Open Drains" = '#e41a1c',
                            "Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Oceans" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
                df.exposure %>% filter(age == "Adults") %>%
                        mutate(city = replace(city, city == "Atlanta", "Atl"),
                               city = replace(city, city == "Lusaka", "Lka")) %>%
                        ggplot(., aes(x=factor(neighb_id), y=perDose, fill=pathway) ) +
                        geom_bar(stat="identity") +
                        facet_grid(~ city, scales = "free_x", space = "free_x") +
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
        output$plot_exposure_bubbles <- renderPlotly({
                
                df.exposure %>% filter(age == "Adults") %>%
                        ggplot(., aes(x=log10(dose), y=exposure, group = 1,
                                      text = paste('City:', city,
                                                   '<br>Dose:', round(log10(dose), 2), "(log10)", 
                                                   '<br>Exposure:', round((exposure*100), 0), "%", 
                                                   '<br>Neighborhood ID:', neighb_id, 
                                                   '<br>Neighborhood:', neighborhood))) +

                        # ggplot(., aes(x=log10(dose), y=exposure, text = paste("Neighborhood:", neighb_id))) +
                        # geom_point(data = transform(df.a, pathway = NULL), colour = "grey85") +

                        geom_point(aes(color=city), size = 3, alpha = .8) +
                        facet_grid(sample_type_name ~ age, labeller = label_wrap_gen(10)) + #wrap label 
                        theme_bw() +
                        labs(title = "Exposure by Pathway",
                             color = "City",
                             x = "E.coli Dose (log10)",
                             y = "Percent") +
                        scale_y_continuous(breaks = c(0,.5,1),
                                           labels = scales::percent) +
                        scale_color_brewer(palette="Set1") +
                        theme(strip.text.y = element_text(size = 8),
                              strip.background = element_rect(fill = "white"),
                              legend.position = "bottom") -> p
                ggplotly(p, tooltip = "text")
        })

        
        # **************************************************************************************************
        
        # **************************************************************************************************
        output$plot_exposure_children_plotly <- renderPlotly({
                colors <- c("Open Drains" = '#e41a1c',
                            "Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Oceans" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
                df.exposure %>% filter(age == "Children") %>%
                        mutate(city = replace(city, city == "Atlanta", "Atl"),
                               city = replace(city, city == "Lusaka", "Lka")) %>%
                        ggplot(., aes(x=neighb_pop_label, y=perDose, fill=pathway, group = 1,
                                      text = paste('City: ', city,
                                                   '<br>Neighborhood:', neighborhood,
                                                   '<br>Neighborhood ID:', neighb_id, 
                                                   '<br>Dose:', round(perDose,2), 
                                                   '<br>Pathway:', pathway)
                        ) ) +
                        geom_bar(stat="identity") +
                        geom_vline(aes(xintercept = 7.5)) +
                        geom_vline(aes(xintercept = 8.5)) +
                        geom_vline(aes(xintercept = 18.5)) +
                        geom_vline(aes(xintercept = 23.5)) +
                        geom_vline(aes(xintercept = 27.5)) +
                        geom_vline(aes(xintercept = 28.5)) +
                        geom_vline(aes(xintercept = 32.5)) +
                        geom_vline(aes(xintercept = 37.5)) +
                        # facet_grid(~ city, scales = "free_x", space = "free_x") +
                        labs(fill = "Pathway",
                             x = "Neighborhood",
                             y = "Total Exposure (log10)") +
                        theme_bw() +
                        theme(legend.position="bottom",
                              axis.text.x = element_text(angle = 90),
                              strip.text.x = element_text(size = 12),
                              # strip.text.y = element_text(size = 12),
                              strip.background = element_rect(fill="white")) +
                        scale_fill_manual(values = colors) -> p
                ggplotly(p, tooltip = "text")
                
        })
        
        # **************************************************************************************************
        output$plot_exposure_children <- renderPlot({
                colors <- c("Open Drains" = '#e41a1c',
                            "Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Oceans" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
                df.exposure %>% filter(age == "Children") %>%
                        mutate(city = replace(city, city == "Atlanta", "Atl"),
                               city = replace(city, city == "Lusaka", "Lka")) %>%
                        ggplot(., aes(x=factor(neighb_id), y=perDose, fill=pathway) ) +
                        geom_bar(stat="identity") +
                        facet_grid(~ city, scales = "free_x", space = "free_x") +
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
        output$plot_exposure_bubbles_c <- renderPlotly({
                
                df.exposure %>% filter(age == "Children") %>%
                        ggplot(., aes(x=log10(dose), y=exposure, group = 1,
                                      text = paste('City:', city,
                                                   '<br>Dose:', round(log10(dose), 2), "(log10)", 
                                                   '<br>Exposure:', round((exposure*100), 0), "%", 
                                                   '<br>Neighborhood ID:', neighb_id, 
                                                   '<br>Neighborhood:', neighborhood))) +
                        
                        # ggplot(., aes(x=log10(dose), y=exposure, text = paste("Neighborhood:", neighb_id))) +
                        # geom_point(data = transform(df.a, pathway = NULL), colour = "grey85") +
                        
                        geom_point(aes(color=city), size = 3, alpha = .8) +
                        facet_grid(sample_type_name ~ age, labeller = label_wrap_gen(10)) + #wrap label 
                        theme_bw() +
                        labs(title = "Exposure by Pathway",
                             color = "City",
                             x = "E.coli Dose (log10)",
                             y = "Percent") +
                        scale_y_continuous(breaks = c(0,.5,1),
                                           labels = scales::percent) +
                        scale_color_brewer(palette="Set1") +
                        theme(strip.text.y = element_text(size = 8),
                              strip.background = element_rect(fill = "white"),
                              legend.position = "bottom") -> p
                ggplotly(p, tooltip = "text")
        })
        
        
        # **************************************************************************************************
        output$plot_exposure_all <- renderPlot({
                colors <- c("Open Drains" = '#e41a1c',
                            "Drinking Water" = '#377eb8',
                            "Raw Produce" = '#4daf4a',
                            "Floodwater" = '#984ea3',
                            "Bathing Water" = '#ff7f00',
                            "Oceans" = '#ffff33',
                            "Surface Water" = '#a65628',
                            "Public Latrine" = '#f781bf',
                            "Street Food" = '#999999')
                
                
                df.exposure %>% 
                        mutate(city = replace(city, city == "Atlanta", "Atl"),
                               city = replace(city, city == "Lusaka", "Lka")) %>%
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
        factpal <- colorFactor("plasma", meta_neighb$deployment)
        df11 <- left_join(meta_neighb, meta_dply[, c(1,2,4)], by=c("deployment_id" = "id"))

        output$mapneighborhoods <- renderLeaflet(
                leaflet(df11) %>% 
                addTiles() %>%
                addCircleMarkers(lng = ~long,
                                 lat = ~lat, 
                                 label = paste0(df11$neighborhood, ", ", df11$city, " (", df11$country, ")"),
                                 popup = ~deployment,
                                 options = markerOptions(draggable = F, riseOnHover = TRUE),
                                 color = ~factpal(deployment)) 
                )
        # **************************************************************************************************
        df <- left_join(df.ecdata, df.col[, c("col_UID", "lat" = "X_col_location_latitude", "X_col_location_longitude")],
                        by = c("UID" = "col_UID"))
        
        colnames(df)[colnames(df)=="X_col_location_latitude"] <- "lat"
        colnames(df)[colnames(df)=="X_col_location_longitude"] <- "lon"
        
        df$ec_log <- log10(df$ec_conc)
        df <- df %>% filter_all(all_vars(!is.na(ec_conc)))
        cutoff <- quantile(df$ec_log, c(0, .33, .66, 1)) 

        df <- df %>% mutate(ghana_label = case_when(ec_log <= cutoff[[2]] ~ 1, 
                                                    ec_log > cutoff[[2]] & ec_log <= cutoff[[3]] ~ 2,
                                                    ec_log > cutoff[[3]] & ec_log <= cutoff[[4]] ~ 3))
        df <- df %>% mutate(ghana_color = case_when(ghana_label == 1 ~ "green", 
                                                    ghana_label == 2 ~ "yellow",
                                                    ghana_label == 3 ~ "red"))
        cutoff1 <- c(0, cutoff[[2]], cutoff[[3]])
        cutoff1 <- sprintf("%.2f", round(cutoff1, 2))
        color1 <- c("green", "yellow", "red")
        
        # outlier 
        df$lat[df$UID == "11_02_1018"] <- 12.915
        df$lon[df$UID == "11_02_1018"] <- 79.141
        
        output$mapecoli <- renderLeaflet(
                leaflet(df) %>% 
                        addTiles() %>%
                        # addProviderTiles(providers$MtbMap) %>%
                        # addProviderTiles(providers$Stamen.TonerLines,
                        #                  options = providerTileOptions(opacity = 0.35)) %>%
                        # addProviderTiles(providers$Stamen.TonerLabels) %>%
                        addCircles(~lon, ~lat,
                                   popup = paste0(df$sample_type_name, ", ", sprintf("%.2f", round(df$ec_log, 2)), " E.coli", " (log10)"),
                                   weight = 3, radius=20, color=~ghana_color, stroke = TRUE, fillOpacity = 1) %>%
                        addLegend("bottomright", colors = color1, labels = cutoff1,
                                  title = "Ecoli Value Cutoff") %>%
                        addScaleBar("bottomleft")
                
        )
        # **************************************************************************************************
        # **************************************************************************************************
        # output$out3 <- renderPrint(input$deployment)
        # **************************************************************************************************
        # output$dplyname <- renderPrint(as.character(meta_dply$citylabel[meta_dply$id == input$deployment]))
        
        # **************************************************************************************************
        # meta_neighb_subset <- reactive({
                # meta_neighb %>% filter(deployment_id == input$deployment) %>%
                        # select(neighb_UID, neighborhood)})
        
        # output$profiletable <- renderTable({meta_neighb_subset()})
        
        # **************************************************************************************************
        # output$samplenr <- length(df.ecdata$UID[df.ecdata$dply_num == input$deployment])


        
   
   
        
}

# Run the application 
shinyApp(ui = ui, server = server)

