demographics_data <- read_csv("raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv") %>% 
  clean_names()

demographics_data <- demographics_data %>% 
  rename(healthboard = hb) %>%
  mutate(healthboard = recode(healthboard,
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
                              "SB0801"    = "The Golden Jubilee National Hospital",
                              "S27000001" = "Non-NHS Provider/Location",
                              "SN0811" = "Unknown"))



write_csv(demographics_data, "clean_data/demographics.csv")
