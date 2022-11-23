
library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(ggplot2)
library(tsibble)


admin_hb_speciality <- read_csv("raw_data/hospital_admissions_hb_specialty_20221102.csv")


#start cleaning. Dates from 20200105 to 20221023

admin_hb_speciality <- admin_hb_speciality %>% clean_names()





# Delete columns : HBFQ - NA/ d , admission_type_qf : NA, specialty_qf : NA

admin_hb_speciality <- admin_hb_speciality %>% 
  select(-c(3,5,7))

view(admin_hb_speciality)


#Change column  'hb' to 'healthboard'
admin_hb_speciality <- admin_hb_speciality %>% 
  rename(hb = hb) %>%
  mutate(hb = recode(hb,
                     "S08000015" = "NHS Ayrshire and Arran",
                     "S08000016" = "NHS Borders",
                     "S08000017" = "NHS Dumfries and Galloway",
                     "S08000019" = "NHS Forth Valley",
                     "S08000020" = "NHS Grampian",
                     "S08000022" = "NHS Highland",
                     "S08000024" = "NHS Lothian",
                     "S08000025" = "NHS Orkney",
                     "S08000026" = "NHS Shetland",
                     "S08000028" = "NHS Western Isles",
                     "S08000029" = "NHS Fife",
                     "S08000030" = "NHS Tayside",
                     "S08000031" = "NHS Greater Glasgow and Clyde",
                     "S08000032" = "NHS Lanarkshire",
                     "S92000003" = "Scotland",
                     "SB0801"    = "The Golden Jubille National",
                     "S08000018" = "NHS Fife",
                     "S08000021" = "NHS Greater Glasgow and Clyde",
                     "S08000023" = "NHS Lanarkshire",
                     "S08000027" = "NHS Tayside"))

view(admin_hb_speciality)


# modify date
admin_hb_speciality <-  admin_hb_speciality %>% 
  mutate(week_ending = ymd(week_ending),
         month_enter = month(week_ending, label = TRUE, abbr = FALSE),
         year_enter = year(week_ending),
         day_enter = day(week_ending),
  )

admin_hb_speciality <- admin_hb_speciality %>% 
  mutate(quarters = case_when(
    month_enter %in% c('January','February','March')~ "Q1",
    month_enter %in% c('April','May', 'June') ~ "Q2",
    month_enter %in% c('July','August','September') ~ "Q3",
    month_enter %in% c('October', 'November', 'December')~ "Q4",
  ))
view(admin_hb_speciality)



write_csv(admin_hb_speciality, "clean_data/admin_hb_speciality.csv")

