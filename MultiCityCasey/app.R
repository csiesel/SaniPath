library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(reshape2)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(forcats)
library(ggridges)


# load data
source("helper_dataload.R")


ui <- shinyUI(navbarPage("SaniPath Dashboard",
    tabPanel("E. coli Contamination Map"),
        fluidRow(column(6,leafletOutput("mapecoli"), sidebarPanel(
                                                                  h4("Select City/Cities"),
                                                                  checkboxGroupInput("city", NULL,
                                                                                     c(cities)))),
                column(6, plotOutput("ecoli_boxplot", height="1200px")))
    
    
    
    
))



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    ##### E. coli Maps and graphs (city-specific)
    citychoice <- reactive({input$city})
    ecoli_map <- reactive({
        df.ecdata<- filter(df.ecdata, city %in% citychoice())

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
            
        
        
        
        
    })
    output$mapecoli <- renderLeaflet(ecoli_map())
    
    ecoli_plot <- reactive({
        if(is.null(input$city)){
                return(NULL)
            }

        colourCount = length(unique(meta_dply$city))
        getPalette = colorRampPalette(brewer.pal(9, "Set3"))
        colScale <- scale_fill_manual(values=getPalette(colourCount))
    
        df.ecdata %>% filter(city %in% citychoice()) %>%
            

            ggplot(., aes(y=factor(hood), x=log10(ec_conc), fill=city)) +
            stat_density_ridges(aes(fill=city), quantile_lines=TRUE, quantiles=2) +
            # geom_point(aes(color=city) ) +
            # facet_grid( ~ sample_type_name, scales = "free_x", space = "free_x") +
            facet_wrap( ~ sample_type_name, scales = "free", nrow=4, ncol=3) +
            labs(fill = "City",
                 x = "E. coli (Log10)",
                 y = "") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                  axis.text=element_text(size=8),
                  strip.background = element_rect(fill="white")) +
            colScale
    
    
    
    
    })
    output$ecoli_boxplot <- renderPlot(ecoli_plot())
}

# Run the application 
shinyApp(ui = ui, server = server)

