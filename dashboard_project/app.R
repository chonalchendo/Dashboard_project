library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(tsibble)
library(urca)
library(fable)
library(prophet)
library(lubridate)
library(sf)
library(leaflet)
library(htmltools)
library(ggthemes)

#demographics code
demographics_data <- read_csv(here::here("clean_data/demographics.csv"))

healthboards <- unique(demographics_data$healthboard)

age_choices <-  sort(unique(demographics_data$age))

#waiting times code

wait_times_clean <- read_csv(here::here("clean_data/wait_times_clean.csv"))

wait_times_map <- st_read(here::here("clean_data/wait_times_map_data.geojson"))

department_type <- sort(unique(wait_times_clean$department_type))

year <- sort(unique(wait_times_clean$year))

healthboard_wait_times <- sort(unique(wait_times_clean$healthboard))

month_or_year <- wait_times_clean %>%
  select(month, year) 

dropped_joined_waiting <- read_csv(here::here("../Dashboard_project/clean_data/dropped_joined_waiting.csv"))

map_variables <- dropped_joined_waiting %>% select(percent_target_met, percent_target_met_ep)

long_wait_variables <- wait_times_clean %>% select(attendance_greater8hrs, attendance_greater12hrs)

target_not_met_tsib <- read_csv(here::here("../Dashboard_project/clean_data/new_wait_tsib.csv")) %>%
  mutate(year_month = yearmonth(year_month)) %>% 
  as_tsibble(index = year_month, key = id) %>% 
  summarise(avg_target_not_met = mean(diff_between_target_met_agg))


#beds code

beds <- read_csv(here::here("clean_data/beds_16_to_21.csv")) %>% 
  mutate(quarter = yearquarter(quarter))

beds_specialty <- sort(unique(beds$specialty_name))

beds_healthboard <- sort(unique(beds$healthboard))

beds_map <- st_read(here::here("clean_data/map_beds_4.geojson"))

pal <- colorBin("YlOrRd", domain = beds_map$mean_percent_occ_board, bins = 7)

labels <- sprintf(
  "<strong>%s</strong><br/>%g &#x25 occupied beds on average <br> from Q4 2017 to Q3 2021",
  beds_map$healthboard, beds_map$mean_percent_occ_board
) %>% lapply(htmltools::HTML)

pal2 <- colorBin("YlOrRd", domain = beds_map$mean_wards_over_ninety, bins =5)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g &#x25 of wards over 90 <br> percent occupied from <br>Q4 2017 to Q3 2021",
  beds_map$healthboard, beds_map$mean_wards_over_ninety
) %>% lapply(htmltools::HTML)


#covid code

covid_age_sex <- read_csv(here::here("clean_data/covid_age_sex.csv"))
covid_tsibble <- as_tsibble(covid_age_sex, index = week_ending)

#Kids_admission_Nacho

admin_hb_speciality <- read_csv(here::here("clean_data/admin_hb_speciality.csv"))


kids_hb_noscot <- admin_hb_speciality %>% 
  filter(!hb %in%  c("Scotland", NA_character_, "NHS Orkney", "NHS Shetland"))

kids_hb <- unique(kids_hb_noscot$hb)

total_mean_kids_quarters_no_scotland<- admin_hb_speciality %>% 
  mutate(quarters = yearquarter(week_ending))%>% 
  group_by(hb, quarters) %>% 
  filter(specialty %in% c('Paediatrics (medical & surgical)','Paediatrics (medical)')) %>% 
  filter(hb!= 'Scotland') %>%
  summarise(mean_kids = mean(number_admissions)) %>% 
  arrange(desc(mean_kids))


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
        fluidRow(
          column(
            6,
            h2("Across All Health Boards")),
          column(
            3,
            icon("mars", "fa-3x"),
            tags$style(".fa-mars {color:#00BFC4}"),
            h4("Male life expectancy in Scotland 76.6")
          ),
          column(
            3, 
            icon("venus", "fa-3x"),
            tags$style(".fa-venus {color:#F8766D}"),
            h4("Female life expectancy in Scotland 80.8")
          )
        ),
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
        
        h3("Attendance Targets Met"),
        fluidRow(
          column(
            6,
            plotOutput("attendancetarget")
          ),
          column(
            6,
            plotOutput("targetmap")
          )
        ),
        fluidRow(
          column(
            6,
            varSelectInput(
              inputId = "timeseriess",
              label = "Select Month or Year",
              data = month_or_year,
              multiple = FALSE
            ),
            selectInput(
              inputId = "healthboard2", 
              label = "Select a Healthboard",
              choices = healthboard_wait_times
            )),
          
          column(
            6,
            varSelectInput(
              inputId = "variable",
              label = "Select Aggregate or Episode Data",
              data = map_variables,
              multiple = FALSE
  
            )
          )
          
        ),
        
        h3("Attendances Greater than 8hrs or 12hrs"),
        fluidRow(
          column(
            12,
            plotOutput("attendance8hrs")
          )
        ),
        varSelectInput(
          inputId = "timeseries",
          label = "Select Time Measurement",
          data = month_or_year,
          multiple = FALSE
            
        ),
        selectInput(
          inputId = "healthboards", 
          label = "Select a Healthboard",
          choices = healthboard_wait_times
        ),
        varSelectInput(
          inputId = "which_hr",
          label = "Select 8hr or 12hr Wait Time",
          data = long_wait_variables,
          multiple = FALSE
        
        
      ),
      
      h3("Forecast For Wait Time Targets Not Met"),
      
      fluidRow(
        column(
          12,
          plotOutput("wait_time_prophet")
        )
      )
      
      ),
        
      tabItem(
        tabName = "beds",
        h2("Bed occupancy across healthboards and specialities"),
        fluidRow(
          column(
            6,
            h4("Map showing overall bed occupancy by healthboard"),
            leafletOutput("map_occ")
          ),
          column(
            6,
            h4("Map showing percentage of wards in healthboard with an occupancy over 90%"),
            leafletOutput("map_over_ninety")
          )
        ),
        fluidRow(
        column(
          6,
        selectInput(
          inputId = "beds_healthboard", 
          label = "Select a Healthboard",
          choices = beds_healthboard
        ),
        selectInput(
          inputId = "beds_specialty",
          label = "Select a Specialty",
          choices = beds_specialty
        )
        ),
        column(
          6,
          selectInput(
            inputId = "beds_healthboard_2", 
            label = "Select a Healthboard",
            choices = beds_healthboard
          )
        )),
        fluidRow(
          column(6,
                 h4("Percentage occupancy of beds for chosen healthboard and specialty"),
                h5("Points showing individual department occupancy, Lines showing average occupancy")),
          
          column(6,
                 h4("Percentage of speciality wards in healthboard with over 90% occupancy")
          )
        ),
        
        fluidRow(
          column(
            6,
            plotOutput("beds_board_spec"),
            h6("Note - Not all departments available in all time periods and healthboards")
          ),
          column(
            6,
            plotOutput("beds_over_ninety")
          )
        )
      ),
      tabItem(
        tabName = "covid",
        h2("Impact of Covid on the NHS"),
        tags$br(),
        fluidRow(
          column(2,
                 checkboxGroupInput(
                   inputId = ("kids_admission"), 
                   label = "Healthboard",
                   choices = kids_hb, 
                   selected = kids_hb
                 )
                 ),
          column(10,
                 plotOutput("admission_kidsPlot"))
        ),
        h3("Total admissions forecasts"),
        tags$br(),
        fluidRow(
          column(
            6,
            plotOutput("forecast")
          ),
          column(
            6,
            plotOutput("prophet")
          ),
          h5("All data used from NHS Scotland and covered by the Open Government License"),
          h5("https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/")
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
      scale_y_continuous(labels = scales::comma) +
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
      scale_y_continuous(labels = scales::comma) +
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
      geom_col(aes(fill = winter)) +
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
      scale_y_continuous(labels = scales::comma) +
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
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Healthboard", y = "Attendances (Epiosdes)", title = "Total Attendances by Healthboard (Episode)")  
      
  )
  
  output$attendancetarget <- renderPlot(
    wait_times_clean %>% 
      filter(healthboard %in% input$healthboard2,
             department_type == input$department_type) %>% 
      mutate(month = factor(month, levels = month.name)) %>% 
      arrange(month) %>% 
      group_by(!!input$timeseriess) %>% 
      summarise(n = mean(percent_target_met)) %>% 
      ggplot(aes(!!input$timeseriess, n, colour = input$healthboard2, group = input$healthboard2)) +
      scale_y_continuous(labels=scales::percent) +
      geom_line(show.legend = FALSE) +
      theme_classic() +
      labs(y = "Percentage", title = "Yearly Healthboard Attendance Target Met (Percentage)") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) 
  )
  
  output$targetmap <- renderPlot(
    wait_times_map %>%
      ggplot(aes(fill = !!input$variable)) +
      geom_sf() +
      theme_map() +
      theme(legend.position = "right") +
      scale_fill_distiller(palette = "YlOrRd") +
      labs(title = "Targets Met as a Percentage by Region")

  )
  
  output$attendance8hrs <- renderPlot(
    wait_times_clean %>%
      filter(healthboard %in% input$healthboards) %>% 
      mutate(month = factor(month, levels = month.name)) %>% 
      arrange(month) %>% 
      group_by(!!input$timeseries) %>%
      summarise(eight_or_12_avg = mean(!!input$which_hr, na.rm = TRUE)) %>%
      ggplot(aes(!!input$timeseries, eight_or_12_avg, colour = input$healthboards, group = input$healthboards)) +
      geom_line(show.legend = FALSE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(y = "Attendances")
    
    )
  
  
  output$wait_time_prophet <- renderPlot({
    
    wait_not_targ_prophet <- target_not_met_tsib %>% 
      mutate(ds = year_month,
             y = avg_target_not_met)
    
    wait_not_targ_prophet <- column_to_rownames(wait_not_targ_prophet, var = "ds") 
    
    wait_not_targ_prophet <-  mutate(wait_not_targ_prophet, ds = year_month)
    
    prophet_no_targ <- prophet(wait_not_targ_prophet)
    
    future_no_targ <- make_future_dataframe(prophet_no_targ, periods = 600)
    
    forecast_p_no_targ <- predict(prophet_no_targ, future_no_targ)
    
    plot(prophet_no_targ, forecast_p_no_targ) +
      xlab("Year") +
      ylab("Targets Not Being Met") +
      labs(title = "Prophet forecast of Targets Not Being Met Across Scotland") +
      theme_classic()
  })
  
  
  #Beds Output
  
  output$map_occ <- renderLeaflet({
    leaflet(beds_map) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal(mean_percent_occ_board),
                  color = "white",
                  fillOpacity = 0.7,
                  label = labels,
                  weight = 2) %>% 
      addLegend(pal = pal,
                values = ~mean_percent_occ_board,
                opacity = 0.7,
                title = NULL,
                position = "bottomright") %>% 
      setView(lat = 58.0000, lng = -5.0000, zoom = 5)
  })
  
  output$map_over_ninety <- renderLeaflet({
    leaflet(beds_map) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal2(mean_wards_over_ninety),
                  color = "white",
                  fillOpacity = 0.7,
                  label = labels2,
                  weight = 2) %>% 
      addLegend(pal = pal2,
                values = ~mean_wards_over_ninety,
                opacity = 0.7,
                title = NULL,
                position = "bottomright")%>% 
      setView(lat = 58.0000, lng = -5.0000, zoom = 5)
  })
  
  output$beds_board_spec <- renderPlot({
    beds%>% 
      filter(healthboard %in% input$beds_healthboard) %>% 
      filter(specialty_name %in% input$beds_specialty) %>% 
      ggplot() +
      geom_point(aes(x = quarter, y = percentage_occupancy, group = quarter), alpha = 0.2)+
      geom_line(aes(x = quarter, y = percentage_occupancy_for_speciality))+
      labs(y = "Percent occupancy of beds",
           x = "Year/Quarter")+
      geom_hline(yintercept = 90, colour = "red")+
      theme_bw()+
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())+
      ylim(NA, 100)
  })
  
  output$beds_over_ninety <- renderPlot({
    beds %>% 
      filter(percent_wards_over_ninety != 0) %>% 
      filter(healthboard %in% input$beds_healthboard_2) %>% 
      ggplot(aes(x = quarter, y = percent_wards_over_ninety, group = healthboard)) +
      geom_line(show.legend = FALSE)+
      labs(y = "Percent of wards with over 90% occupancy",
           x = "Year/Quarter")+
      theme_bw()+
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  
  
  
  
  
  
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
    #Kids_nacho
    output$admission_kidsPlot <- renderPlot(
      total_mean_kids_quarters_no_scotland %>% 
        filter(hb %in% input$kids_admission) %>% 
        ggplot(aes(x= quarters , y = mean_kids, colour = hb))+
        geom_line()+
        labs(
          title = "Kids Number Admission by Quarters/Healthboard",
          x = "Quarters",
          y = "Admission Numbers"
        )+
        geom_vline(xintercept = as.numeric(as.Date (yearquarter("2020 Q2"))),col ="firebrick", lwd = 0.10)+
        annotate("text", x= as.numeric(as.Date (yearquarter("2020 Q2"))), y= 175, label="COVID Lockdown", angle=90, size=5, color="blue") +
        geom_vline(xintercept =as.numeric(as.Date(yearquarter("2022 Q2"))),col ="firebrick", lwd = 0.10)+
        annotate("text", x= as.numeric(as.Date (yearquarter("2022 Q2"))), y= 180, label="summer", angle=90, size=5, color="blue")
      
    )
    
}
# Run the application 
shinyApp(ui = ui, server = server)
