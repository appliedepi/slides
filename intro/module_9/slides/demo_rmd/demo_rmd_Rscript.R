
###############
# My R script #
###############
# Author: Me
# Purpose: An analysis that I want to repeat
# Last updated: 15 March 2022


# Load packages
###############
pacman::p_load(
     rio,
     here, 
     janitor,
     apyramid,
     lubridate,
     epikit,
     scales,
     flextable,
     tidyverse,
     )


# Parameters
############
report_week <- as.Date("2022-03-14")



# Import data
#############
surv_raw <- import(here("intro_course", "surveillance_linelist_12012014.csv"))



# Clean data
############
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
     mutate(week_onset = floor_date(date_onset, unit = "week")) %>% 
     mutate(week_report = floor_date(date_report, unit = "week")) %>% 
     
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
     filter(case_def == "Confirmed") %>% 
     
     # convert NA to "Missing" for hospitals
     mutate(hospital = fct_explicit_na(hospital))







# Descriptive tables
####################

surv %>% 
     group_by(hospital) %>% 
     summarise(
          n_rows  = n(),                               # n() counts the number of rows per group
          age_avg = mean(age_years, na.rm = T),        # mean age in the group
          max_onset = max(date_onset, na.rm=T),        # latest onset date
          n_female = sum(gender == 'female', na.rm=T), # number female in group
          pct_female = percent(n_female / n_rows)) %>% # number female in group
     arrange(desc(n_rows)) %>% 
     
     qflextable()



# Plots
#######

# Height
ggplot(data = surv,
       mapping = aes(
            x = age_years,
            y = ht_cm),    
       alpha = 0.3)+
     geom_point()+
     labs(
          title = "Age and height among cases",
          x = "Age (years)",
          y = "Height (cm)")+
     theme_light(base_size = 18)
  





# Age/sex pyramid
age_pyramid(data = surv, age_group = age_cat, split_by = gender)






# Epidemic curve
ggplot(
     data = surv,
     mapping = aes(
          x = date_onset,
          fill = hospital)) +
     geom_histogram() +
     scale_x_date(
          date_breaks = "2 weeks",
          labels = label_date_short() )+
     scale_fill_brewer(type = "qual",
                       na.value = "grey50")+
     labs(
          title = "Epidemic curve of Ebola outbreak",
          subtitle = "Confirmed cases, 2014",
          x = "Date",
          y = "Number of cases",
          caption = "Fictional Ebola data",
          fill = "District"
     ) +
     theme_grey(base_size = 18)
