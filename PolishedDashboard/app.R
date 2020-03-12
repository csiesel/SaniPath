library(shiny)
library(CaseyGentelellaShiny)
options(shiny.jquery.version = 1)


shinyApp(
  ui = gentelellaPageCustom(
    title = "SaniPath MultiCountry Dashboard",
    sidebar_collapsed = FALSE,
    navbar = gentelellaNavbar(

    ),
    sidebar = gentelellaSidebar(
      sidebarMenu(
        #title = "test",
        sidebarItem("Boxes", tabName = "boxes", icon = tags$i(class = "fas fa-clone"), badgeName = "New"),
        sidebarItem("Tabs", tabName = "tabs", icon = tags$i(class = "fas fa-tasks")),
        sidebarItem("Charts", tabName = "charts", icon = tags$i(class = "fas fa-chart-bar")),
        sidebarItem("Other Items", tabName = "others", icon = tags$i(class = "fas fa-bug"))
      )
    )
  ),
  server = function(input, output) {
    

  }
  )
