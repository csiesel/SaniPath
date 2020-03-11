library(shiny)
library(shinydashboard)
library(fullPage)
library(plotly)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(forcats)
library(ggridges)


# load data
source("helper_dataload.R")

ui <- 

    shinyUI(navbarPage(theme=shinytheme("flatly"),
    "SaniPath Dashboard",
    tabPanel("E. coli Contamination", icon=icon("bar-chart"),
        fluidRow(
            column(2,
                   wellPanel(
                       h4("Select City/Cities"),
                       checkboxGroupInput("city", NULL, c(cities))
                    )
                   ),
            column(10,
                   h4("Map of samples and sample contamination score"),
                   leafletOutput("mapecoli")
                   )
            ), #end of fluidRow1
        fluidRow(
            wellPanel(
            h4("E. coli contamination by sample type", align="center"),
            plotOutput("ecoli_boxplot"),
            HTML("<p align=center>
                    <i>
                    <font size=2 color=darkred>
                    NOTE: Units are as Log10 E. coli/100mL except for the following: Street Food and Raw Produce (Log10 E. coli/serving),
                    Latrine Swabs (Log10 E. coli/swab), Soil (Log10 E. coli/gram)
                    </font>
                    </i>
                    </p>"))
        ) #end of fluidRow2
    ),#end of E. coli tabpanel
    
    tabPanel("Behavior", icon=icon("pie-chart"),
        fluidRow(plotlyOutput("bx_plot"))
    ) #end of Bx tabpanel
    ))



# Define server logic required to draw a histogram
server <- function(input, output) {
    citychoice <- reactive({input$city})
    ################# E. coli Maps and graphs (city-specific) #####
    # Plotting the map for ecoli
    ecoli_map <- reactive({
        df.ecdata<- filter(df.ecdata, city %in% citychoice())

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
        cutoff1 <- c(0, cutoff[[2]], cutoff[[3]], cutoff[[4]])
        cutoff1 <- sprintf("%.2f", round(cutoff1, 2))
        color1 <- c("green", "yellow", "orange", "red")
        
        # outlier 
        df$lat[df$UID == "11_02_1018"] <- 12.915
        df$lon[df$UID == "11_02_1018"] <- 79.141
        
        leaflet(df) %>% 
            addTiles() %>%
            # addProviderTiles(providers$MtbMap) %>%
            # addProviderTiles(providers$Stamen.TonerLines,
            #                  options = providerTileOptions(opacity = 0.35)) %>%
            # addProviderTiles(providers$Stamen.TonerLabels) %>%
            addCircles(~lon, ~lat,
                       popup = paste0(df$sample_type_name, ", ", sprintf("%.2f", round(df$std_ec_conc, 2)), " Normalized E.coli", " (log10)"),
                       weight = 3, radius=20, color=~dot_color, stroke = TRUE, fillOpacity = 1) %>%
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
    
        df.ecdata %>% filter(city %in% citychoice()) %>%
            

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
                  strip.background = element_rect(fill="white")) +
            colScale

    })
    output$ecoli_boxplot <- renderPlot(ecoli_plot())
    
    
    
    ################# Behavior graphs (city-specific) #####
    # Plotting the bx graph by city/neighborhood
    bx_plot <- reactive({
        df.behav %>% 
            filter(., city %in% citychoice()) %>%
            melt(., id.vars = c("city", "sample_type", "pop")) %>%
            na.omit(value) %>%
            #!!!!!Change below to x=hood when I figure this out !!!!!!!!!!!!!!!!!!!!!
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
    output$bx_plot <- renderPlotly(bx_plot())
}

# Run the application 
shinyApp(ui = ui, server = server)

