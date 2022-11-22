covid_demo <- read_csv("raw_data/hospital_admissions_hb_agesex_20221102.csv") %>% 
  clean_names()

covid_age_sex <- covid_demo %>%
  filter(admission_type == "All", sex == "All", age_group == "All ages") %>% 
  group_by(week_ending) %>% 
  summarise(total_admissions = sum(number_admissions)) %>% 
  mutate(week_ending = ymd(week_ending))

write_csv(covid_age_sex, "clean_data/covid_age_sex.csv")
