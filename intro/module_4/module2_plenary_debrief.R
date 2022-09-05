
# Plenary debrief for Module 1:
###############################

# What makes for clean data?  

pacman::p_load(
     rio,
     here,
     janitor,
     epikit,
     tidyverse
)


# Import and save raw data
surv_raw <- import(here("intro_course", "surveillance_linelist_20141201.csv"))



# Descriptive checks
#####################
# column classes
# distribution of numeric and date columns
# character values
# missing values

# Cleaning pipeline


# case_when()
##############

surv_raw %>% 
  tabyl(hospital)


# mutates within one command (commas)

     
# tidylog
##########
pacman::p_load(tidylog)
     

surv <- surv_raw %>% 
  select(age, gender, case_id) %>% 
  mutate(is_child = age < 18) %>% 
  filter(is_child == TRUE)


surv <- surv_raw %>% 
     
     # automatically clean column names
     clean_names() %>% 
     
     # manually clean column names   
     rename(
          date_onset = onset_date,
          date_report = date_of_report,
          district_res = adm3_name_res,
          district_det = adm3_name_det) %>%
     
     # remove unnecessary column
     select(-row_num) %>% 
     
     # de-duplicate rows  
     distinct() %>% 
     
     # convert date_onset to date class
     mutate(date_onset = mdy(date_onset)) %>% 
     mutate(date_report = mdy(date_report)) %>% 
     
     # create epiweek columns  
     mutate(week_onset = floor_date(date_onset, unit = "week", week_start = 1)) %>% 
     mutate(week_report = floor_date(date_report, unit = "week", week_start = 1)) %>% 
     
     # convert age to numeric class
     mutate(age = as.numeric(age)) %>% 
     
     # properly record missing values
     mutate(across(.cols = where(is.character), .fns = na_if, "")) %>% 
     
     # Make date-difference column  
     mutate(diff = date_report - date_onset) %>% 
     
     # convert negative values to NA
     mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg),
            bmi   = ifelse(bmi < 0,   NA, bmi)) %>% 
     
     # convert gender values to full words
     mutate(gender = case_when(               # re-define gender as: 
          gender == 'm' ~ 'male',                # when "m", change to "male"   
          gender == 'f' ~ 'female',              # when "f", change to "female" 
          TRUE          ~ gender)) %>%           # any other value, remain as before
     
     # create age-in-years
     mutate(age_years = case_when(
          age_unit == "years"  ~ age,            # if age is given in years
          age_unit == "months" ~ age/12,         # if age is given in months
          is.na(age_unit)      ~ age,            # if age unit is missing, assume years
          TRUE                 ~ NA_real_)) %>%  # any other circumstance, assign missing
     
     # create age category column
     mutate(age_cat = age_categories(         # create new column
          age_years,                             # numeric column to make groups from
          lower = 0,
          upper = 70,
          by = 10)) %>% 
     
     # create column marking TRUE if district of residence and detection differ
     mutate(moved = district_res != district_det) %>% 
     
     # create new column that prioritizes district of detection
     mutate(district = coalesce(district_det, district_res)) %>% 
     
     # re-code hospital column
     mutate(hospital = recode(hospital,
                              # for reference: OLD = NEW
                              "Mitilary Hospital"  = "Military Hospital",
                              "Port"               = "Port Hospital",
                              "Port Hopital"       = "Port Hospital",
                              "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>% 
     
     # remove suspect cases
     filter(case_def == "Confirmed")




# "break glass in case of emergency" folder in "learning_resources" folder
# has versions of the Ebola script after each module's edits  

