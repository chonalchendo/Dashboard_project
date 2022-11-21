library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)

demographics_data <- read_csv(here::here("clean_data/demographics.csv"))

healthboards <- unique(demographics_data$healthboard)

age_choices <-  sort(unique(demographics_data$age))


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
        tabName = "demographics",
        h2("NHS Demographics 2016 - 2021"),
        h3("Across All Health Boards"),
        fluidRow(
          column(
            6,
            plotOutput("totaleps")
          ),
          column(
            6, 
            plotOutput("lengtheps")
          )
        ),
        h3("Select Healthboard"),
        fluidRow(
          column(
            2,
            checkboxGroupInput(
              "healthboard",
              "Healthboard",
              choices = healthboards,
              selected = healthboards
            )
          ),
          column(
            5,
            plotOutput("totaleps_hb")
            
          ),
          column(
            5,
            plotOutput("lengtheps_hb")
          )
        ),
        h3("Select Demographics"),
        fluidRow(
          column(
            6,
            plotOutput("totaleps_chosen")
            
          ),
          column(
            6,
            plotOutput("lengtheps_chosen")
          )
        ),
        selectInput(
          "age",
          "Age",
          choices = age_choices
        ),
        selectInput(
          "gender",
          "Gender",
          choices = c("Male", "Female")
        )
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
  output$totaleps <- renderPlot(
    demographics_data %>% 
      group_by(age, sex) %>% 
      summarise(age_total = sum(episodes)) %>% 
      ggplot(aes(x = age, y = age_total)) +
      geom_col(aes(fill = sex), position = "dodge") +
      ylab("Total Episodes") +
      xlab("Age Range") +
      labs(title  = "Total Episodes by Age and Gender") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )  
  
  output$lengtheps <- renderPlot(
    demographics_data %>% 
      group_by(age, sex) %>% 
      summarise(average_length_of_episode = mean(average_length_of_episode, na.rm = TRUE)) %>% 
      ggplot(aes(x = age, y = average_length_of_episode)) +
      geom_col(aes(fill = sex), position = "dodge") +
      ylab("Average Length of Episode") +
      xlab("Age Range") +
      labs(title  = "Average Length of Episode by Age and Gender") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  output$totaleps_hb <- renderPlot(
    demographics_data %>% 
      mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>% 
      filter(healthboard %in% input$healthboard) %>% 
      group_by(quarter) %>% 
      ggplot(aes(x = quarter, y = episodes)) +
      geom_col(aes(fill = winter, color = winter)) +
      ylab("Total Episodes") +
      xlab("Quarter") +
      labs(title  = "Total Episodes by Healthboard") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  output$lengtheps_hb <- renderPlot(
    demographics_data %>% 
      mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>% 
      filter(healthboard %in% input$healthboard) %>% 
      group_by(quarter) %>% 
      summarise(mean_episode = mean(average_length_of_episode, na.rm = TRUE),
                winter = winter) %>% 
      ggplot(aes(x = quarter, y = mean_episode)) +
      geom_col(aes(fill = winter, color = winter)) +
      ylab("Average Length of Episode") +
      xlab("Quarter") +
      labs(title  = "Average Length of Episode by Healthboard") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  output$totaleps_chosen <- renderPlot(
    demographics_data %>%
    mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>% 
      filter(sex == input$gender, age == input$age) %>% 
      group_by(quarter) %>% 
      ggplot(aes(x = quarter, y = episodes)) +
      geom_col(aes(fill = winter, colour = winter)) +
      ylab("Total Episodes") +
      xlab("Quarter") +
      labs(title  = "Total Episodes by chosen demographics") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  output$lengtheps_chosen <- renderPlot(
    demographics_data %>% 
      mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>% 
      filter(sex == input$gender, age == input$age) %>% 
      group_by(quarter) %>% 
      summarise(average_length_of_episode = mean(average_length_of_episode, na.rm = TRUE),
                winter = winter) %>% 
      ggplot(aes(x = quarter, y = average_length_of_episode))+
      geom_col(aes(fill = winter, colour = winter)) +
      ylab("Average episode length") +
      xlab("Quarter") +
      labs(title  = "Average Episode Length by chosen demographics") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )

}
# Run the application 
shinyApp(ui = ui, server = server)
