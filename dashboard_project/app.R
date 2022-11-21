library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "NHS Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demographics", icon = icon("people-group")),
      menuItem("Waiting Times", tabName = "waiting", icon = icon("clock")),
      menuItem("Beds", tabName = "beds", icon = icon("bed")),
      menuItem("Covid", tabName = "covid", icon = icon("virus-covid"))
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "demographics"
        # Lewis 
      ),
      tabItem(
        tabName = "waiting"
        #Conal
      ),
      tabItem(
        tabName = "beds"
      ),
      tabItem(
        tabName = "covid"
        #Nacho
      )
    )
    
  )
  

  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}
# Run the application 
shinyApp(ui = ui, server = server)
