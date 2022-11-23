library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(tsibble)
library(urca)
library(fable)
library(prophet)
library(lubridate)

#demographics code
demographics_data <- read_csv(here::here("clean_data/demographics.csv"))

healthboards <- unique(demographics_data$healthboard)

age_choices <-  sort(unique(demographics_data$age))

#waiting times code

wait_times_clean <- read_csv(here::here("clean_data/wait_times_clean.csv"))

wait_times_map <- read_csv(here::here("clean_data/map_data_waiting_times.csv"))

department_type <- sort(unique(wait_times_clean$department_type))

year <- sort(unique(wait_times_clean$year))

healthboard_wait_times <- sort(unique(wait_times_clean$healthboard))

month_or_year <- wait_times_clean %>% select(month, year)

map_variables <- dropped_joined_waiting %>% select(percent_target_met, percent_target_met_ep)


#beds code




#covid code

covid_age_sex <- read_csv(here::here("clean_data/covid_age_sex.csv"))
covid_tsibble <- as_tsibble(covid_age_sex, index = week_ending)




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
        tabName = "waiting",
        h2("NHS Wait Times 2007 - 2022"),
        h3("Distribution of NHS Attendances"),
        fluidRow(
          column(
            6,
            plotOutput("sumaggattendance")
          ),
          column(
            6, 
            plotOutput("sumepattendance")
          )
        ),
        selectInput(
          inputId = "department_type",
          label = "Select a Department Type",
          choices = department_type
        ),
        selectInput(
          inputId = "year", 
          label = "Select a Year",
          choices = year
        ),
        
        h3("Attendance Targets"),
        fluidRow(
          column(
            6,
            plotOutput("attendancetarget")
          ),
          column(
            6,
            plotOutput("targetmap")
          ),
          varSelectInput(
            inputId = "variable",
            label = "Select variable",
            data = map_variables,
            multiple = FALSE
          )
        ),
        varSelectInput(
        inputId = "timeseriess",
        label = "Select Month or Year",
        data = month_or_year,
        multiple = FALSE
        ),
        
        selectInput(
          inputId = "healthboard", 
          label = "Select a Healthboard",
          choices = healthboard_wait_times
        ),
        
        h3("Attendances Greater than 8hrs or 12hrs"),
        fluidRow(
          column(
            6,
            plotOutput("attendance8hrs")
          ),
          column(
            6,
            plotOutput("attendance12hrs")
          )
        ),
        varSelectInput(
          inputId = "timeseries",
          label = "Select Time Measurement",
          data = month_or_year,
          multiple = FALSE,
            
        ),
        selectInput(
          inputId = "healthboards", 
          label = "Select a Healthboard",
          choices = healthboard_wait_times
        ),
        
        
      ),
        
      tabItem(
        tabName = "beds"
      ),
      tabItem(
        tabName = "covid",
        h2("Impact of Covid on the NHS"),
        fluidRow(
          column(
            6,
            plotOutput("forecast")
          ),
          column(
            6,
            plotOutput("prophet")
          )
        )
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
  
  #Demographics Output
  
  output$lengtheps_hb <- renderPlot(
    demographics_data %>% 
      filter(healthboard %in% input$healthboard) %>% 
      group_by(quarter) %>% 
      summarise(mean_episode = mean(average_length_of_episode, na.rm = TRUE)) %>% 
      mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>%
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
      filter(sex == input$gender, age == input$age) %>% 
      group_by(quarter) %>% 
      summarise(average_length_of_episode = mean(average_length_of_episode, na.rm = TRUE)) %>% 
      mutate(winter = if_else(str_detect(quarter, "Q[1,4]"), TRUE, FALSE)) %>% 
      ggplot(aes(x = quarter, y = average_length_of_episode))+
      geom_col(aes(fill = winter, colour = winter)) +
      ylab("Average episode length") +
      xlab("Quarter") +
      labs(title  = "Average Episode Length by chosen demographics") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  # Waiting Times Output
  
  output$sumaggattendance <- renderPlot(
    wait_times_clean %>% 
      filter(department_type == input$department_type, 
             year == input$year) %>% 
      group_by(healthboard) %>% 
      summarise(attendance = sum(number_of_attendances_aggregate)) %>% 
      ggplot(aes(reorder(healthboard, attendance), attendance, fill = healthboard)) +
      geom_col(show.legend = FALSE) +
      theme_classic() +
      coord_flip() +
      labs(x = "Healthboard", y = "attendances", title = "Total Attendances by Healthboard (Aggregate)")
  )

  output$sumepattendance <- renderPlot(
    wait_times_clean %>% 
      drop_na() %>% 
      filter(department_type == input$department_type, 
             year == input$year) %>% 
      group_by(healthboard) %>% 
      summarise(attendance_ep = sum(number_of_attendances_episode)) %>% 
      ggplot(aes(reorder(healthboard, attendance_ep), attendance_ep, fill = healthboard)) +
      geom_col(show.legend = FALSE) +
      theme_classic() +
      coord_flip() +
      labs(x = "Healthboard", y = "Attendances (Epiosdes)", title = "Total Attendances by Healthboard (Episode)")  
      
  )
  
  output$attendancetarget <- renderPlot(
    wait_times_clean %>% 
      filter(healthboard %in% input$healthboard,
             department_type == input$department_type) %>% 
      group_by(!!input$timeseriess) %>% 
      summarise(n = mean(percent_target_met)) %>% 
      ggplot(aes(!!input$timeseriess, n, colour = input$healthboard, group = input$healthboard)) +
      scale_y_continuous(labels=scales::percent) +
      geom_line(show.legend = FALSE) +
      theme_classic() +
      labs(x = "Year", y = "Percentage", title = "Yearly Healthboard Attendance Target Met (Percentage)") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
  
  output$targetmap <- renderPlot(
    dropped_joined_waiting %>% 
      ggplot(aes(fill = !!input$variable)) +
      geom_sf() +
      theme_map() +
      theme(legend.position = "right")
      
  )
  
  output$attendance8hrs <- renderPlot(
    wait_times_clean %>%
      filter(healthboard %in% input$healthboards) %>% 
      group_by(!!input$timeseries) %>%
      summarise(avg_attendance_greater_8hrs = mean(attendance_greater8hrs, na.rm = TRUE)) %>%
      ggplot(aes(!!input$timeseries, avg_attendance_greater_8hrs, colour = input$healthboards, group = input$healthboards)) +
      geom_line(show.legend = FALSE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
    )
  
  output$attendance12hrs <- renderPlot(
    wait_times_clean %>%
      filter(healthboard %in% input$healthboards) %>%
      group_by(!!input$timeseries) %>%
      summarise(avg_attendance_greater_12hrs = mean(attendance_greater12hrs, na.rm = TRUE)) %>%
      ggplot(aes(!!input$timeseries, avg_attendance_greater_12hrs, group = input$healthboards, colour = input$healthboards)) +
      geom_line(show.legend = FALSE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    )

  
  
  
  
  
  
  
  
  #Beds Output
  
  
  
  
  
  
  
  
  
  #Covid Output
  
  output$forecast <- renderPlot({
    fit <- covid_tsibble %>% 
      model(
        snaive = SNAIVE(total_admissions ~ lag("year")),
        arima = ARIMA(total_admissions)
      )
    
    forecast <- fit %>% 
      fabletools::forecast(h = 100)
    
    forecast %>% 
      autoplot(covid_tsibble) +
      xlab("Time") +
      ylab("Total admissions") +
      labs(title = "Forecasts of total admissions")
  }
  )
  
  output$prophet <- renderPlot(
    {
      covid_prophet <- covid_tsibble %>% 
        mutate(ds = week_ending,
               y = total_admissions)
      
      covid_prophet <- column_to_rownames(covid_prophet, var = "ds") 
      
      covid_prophet <-  mutate(covid_prophet, ds = week_ending)
      
      prophet <- prophet(covid_prophet)
      
      future <- make_future_dataframe(prophet, periods = 600)
      
      forecast_p <- predict(prophet, future)
      
      plot(prophet, forecast_p) +
        xlab("Time") +
        ylab("Total admissions") +
        labs(title = "Prophet forecast of total admissions")
    }

  )

}
# Run the application 
shinyApp(ui = ui, server = server)
