library(shiny)
library(fullPage)

ui <- fullPage(
    fullSection(
        menu = "first",
        center = TRUE,
        h1("Callbacks")
    ),
    fullSection(
        menu = "second",
        center = TRUE,
        h3("Slice")
    )
)

server <- function(input, output){
    
    output$slide <- renderPrint({
        input$slide_origin # returns menu
    })
    
}

shinyApp(ui, server)