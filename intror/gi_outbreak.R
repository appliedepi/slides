
# Packages
pacman::p_load(rio, here, janitor, dplyr, stringr, lubridate, gtExtras, gt, tidyr, ggplot2, glue)


# Lab data -------------------------------

# Labs
df_lab_luh <- import(here("data/gi_2021-07-07_lab_luh.csv"))
df_lab_vnrl <- import(here("data/gi_2021-07-07_lab_vnrl.csv"))
df_lab_ghc <- import(here("data/gi_2021-07-07_lab_ghc.csv"))


# Load data as needed
df_raw <- import("data/gi_2021-07-07_linelist.xlsx")

# Clean linelist data ---------------------------

df <- df_raw |>

  # Clean column names
  clean_names() |>
  rename(symptoms_started = date_onset) |>

  # Change character class
  mutate(admission = mdy(admission),
         discovered = dmy(discovered),
         found_on = mdy(found_on),
         reported = ydm(reported),
         told_on = mdy(told_on),
         report  = mdy(report)) |>

  # Clean column content
  mutate(region = str_to_title(region),
         sex = str_to_title(sex),
         sex = case_match(sex,
                          c("F", "Fem", "Female") ~ "Female",
                          c("M","Man", "Male") ~ "Male",
                          .default = NA),
         region = case_match(region,
                             c("Bexar") ~ "Bexar",
                             c("Keda", "Kedah") ~ "Keda",
                             c("Liora", "Lioraah") ~ "Liora",
                             c("Nellus", "Nelos") ~ "Nelos",
                             c("New Ardin", "New Arden") ~ "New Arden",
                             c("Sorel", "Sorell") ~ "Sorel",
                             c("Taris") ~ "Taris",
                             c("Varek") ~ "Varek",
                             c("West Morin") ~ "West Morin",
                             .default = NA_character_))  |>

  # Deduplicate to one row per case, based on case ID
  distinct(id, .keep_all = TRUE) |>

  # Combine some columns
  unite("date_of_birth",
        c("birthyear", "month_of_birth", "daybirth"), sep = "-") |>
  mutate(date_of_birth = ymd(date_of_birth)) |>
  mutate(date_report =  coalesce(told_on, reported, discovered, found_on, report)) |>
  select(-told_on, -reported, -discovered, -found_on, -report) |>

  # Create columns labelling diarrhoea or dehydration
  mutate(symptoms = str_to_lower(symptoms),
         symp_diarrhea = str_detect(symptoms, "diarrhoea|diarrhea|watery stool|loose stool|awd"),
         symp_dehydration = str_detect(symptoms, "dehydra|lack of fluids|lack fluids")) |>

  # Create case definition column
  mutate(case_def = case_when(cholera_rdt_positive==1 ~ "Probable",
                              symp_diarrhea ==TRUE & symp_dehydration==TRUE ~ "Suspected",
                              TRUE ~ "Unclear")) |>

  mutate(case_def = factor(case_def, levels = c("Unclear", "Suspected", "Probable"))) |>

  # Create week column for onset date
  # mutate(date_onset_week = floor_date(date_onset, "week")) |>

  # Create week column for report date
  mutate(date_report_week = floor_date(date_report, "week"))


# Clean and append lab data------------------------

df_lab_luh <- df_lab_luh |>
  clean_names()

df_lab_ghc <- df_lab_ghc |>
  rename(cholera_pcr_result = pcr_result,
         cholera_pcr_positive = pcr_positive,
         cholera_pcr_date_test = pcr_date_test,
         cholera_pcr_date_result = pcr_date_result)

df_lab <- bind_rows(df_lab_luh, df_lab_ghc, df_lab_vnrl)

# Link to df -------------------------------------

df_joined <- df |>
  left_join(df_lab, by = "id")

df_joined <- df_joined |>
  mutate(case_def = case_when(cholera_pcr_positive==1 ~ "Confirmed",
                              cholera_rdt_positive==1 ~ "Probable",
                              symp_diarrhea ==TRUE & symp_dehydration==TRUE ~ "Suspected",
                              TRUE ~ "Unclear")) |>
  mutate(case_def = factor(case_def, levels = c("Unclear", "Suspected", "Probable", "Confirmed")))
