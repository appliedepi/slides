# Intro course
# Title of script: Cleaning and analysis of suspected cholera outbreak data
# Date started: 17th June 2021
# Name: Paula Blomquist

# Load packages-----------------------------

pacman::p_load(rio, here, janitor, dplyr, stringr, lubridate, tidyr, glue, ggplot2, flextable, forcats)


# JUST FOR ME - REMOVE THIS EVENTUALLY ------------
# Labs
df_lab_luh <- import(here("intror/cholera_2021-06-17_lab_luh.csv"))
df_lab_vnrl <- import(here("intror/cholera_2021-06-17_lab_vnrl.csv"))
df_lab_ghc <- import(here("intror/cholera_2021-06-17_lab_ghc.csv"))


# Load data as needed
df_raw <- import(here("intror/cholera_2021-06-17_linelist.xlsx"))

# Other symptoms
df_symptoms <- import(here("intror/cholera_2021-06-17_symptoms.xlsx"))

# Import data ------------------------------

# # Linelist
# df_raw <- import(here("data/cholera_2021-06-17_linelist.xlsx"))
#
# # Labs
# df_lab_luh <- import(here("data/cholera_2021-06-17_lab_luh.csv"))
# df_lab_vnrl <- import(here("data/cholera_2021-06-17_lab_vnrl.csv"))
# df_lab_ghc <- import(here("data/cholera_2021-06-17_lab_ghc.csv"))
#
# # Other symptoms
# df_symptoms <- import(here("data/cholera_2021-06-17_symptoms.xlsx"))

# Clean linelist data ---------------------------

df <- df_raw |>

  # Clean column names
  clean_names() |>
  rename(date_onset = symptoms_started,
         date_admission = admission) |>

  # Change character class
  mutate(date_admission = mdy(date_admission),
         date_onset = mdy(date_onset),
         discovered = dmy(discovered),
         found_on = mdy(found_on),
         reported = ydm(reported),
         told_on = mdy(told_on),
         report  = ymd(report)) |>

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

  # Bring data saved across multiple columns together
  mutate(date_report =  coalesce(told_on, reported, discovered, found_on, report)) |>
  select(-told_on, -reported, -discovered, -found_on, -report) |>

  # Separate outcome and date (tidyr)
  separate_wider_delim(reported_outcome,
                       delim = ":",
                       names = c("outcome", "date_outcome")) |>
  mutate(date_outcome = dmy(date_outcome)) |>

  # Create date of birth and age
  mutate(date_of_birth = str_glue("{birthyear}-{month_of_birth}-{daybirth}")) |>
  select(-birthyear, -month_of_birth, -daybirth) |>

  mutate(date_of_birth = ymd(date_of_birth)) |>

  # Calculate age
  mutate(age_days = date_report - date_of_birth,
         age_years = age_days / 365.25,
         age = floor(age_years),
         age = as.numeric(age)) |>

  # Create columns labelling diarrhoea or dehydration
  mutate(symptoms = str_to_lower(symptoms),
         symp_diarrhea = str_detect(symptoms, "diarrhoea|diarrhea|watery stool|loose stool|awd"),
         symp_dehydration = str_detect(symptoms, "dehydr|lack of fluids|lack fluids|dry mouth")) |>

  # Create case definition column
  mutate(case_def = case_when(cholera_rdt_positive==1 ~ "Probable",
                              symp_diarrhea ==TRUE & symp_dehydration==TRUE ~ "Suspected",
                              TRUE ~ "Unclear")) |>

  mutate(case_def = fct_relevel(case_def, "Unclear", "Suspected", "Probable")) |>

  # Create week column for onset date
  mutate(date_onset_week = floor_date(date_onset, "week")) |>

  # Create week column for report date
  mutate(date_report_week = floor_date(date_report, "week")) |>

  # Create column for whether cases are new
  mutate(date_report_7 = if_else(date_report >=  ymd("2021-06-17")-7, "new", "previous"))

# Check case definition and filter to just Probable and Suspected cases ----------

# Before removing unclear category
df |> tabyl(region, case_def)

# Filter
df <- df |>
  filter(case_def %in% c("Probable", "Suspected"))


# Clean and append lab data------------------------

df_lab_luh <- df_lab_luh |>
  clean_names()

df_lab_ghc <- df_lab_ghc |>
  rename(cholera_pcr_result = pcr_result,
         cholera_pcr_positive = pcr_positive,
         cholera_pcr_date_test = pcr_date_test,
         cholera_pcr_date_result = pcr_date_result)

df_lab <- bind_rows(df_lab_luh, df_lab_ghc, df_lab_vnrl)

# Link datasets  -------------------------------------

# Linkage
df_joined <- df |>
  left_join(df_lab, by = "id") |>
  left_join(df_symptoms, by = "id")

# Check linkage
df_lab |>
  tabyl(cholera_pcr_positive)

df_joined |>
  tabyl(region, cholera_pcr_positive)


# Update case definitions and filter to just Confirmed, Probable, and Suspected cases ----------

# Case definition update
df_joined <- df_joined |>
  mutate(case_def = case_when(cholera_pcr_positive==1 ~ "Confirmed",
                              cholera_pcr_positive==0 ~ "Discarded",
                              cholera_rdt_positive==1 ~ "Probable",
                              symp_diarrhea ==TRUE & symp_dehydration==TRUE ~ "Suspected",
                              TRUE ~ "Unclear")) |>
  mutate(case_def = fct_relevel(case_def, "Discarded", "Suspected", "Probable", "Confirmed"))

# Check categories
df_joined |>
  tabyl(region, case_def)

# Filter
df_joined <- df_joined |>
  filter(case_def %in% c("Confirmed", "Probable", "Suspected"))


# Plots ------------------------------------------

# By report date and case definition breakdown
ggplot() +
  geom_histogram(data = df_joined,
           aes(x=date_report_week,
               fill = case_def), binwidth = 7, color = "white") +
  scale_fill_manual(values = c("Suspected" = "lightblue",
                               "Probable" = "seagreen",
                               "Confirmed" = "navy")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Cases by onset week, presented by case category and region",
       subtitle = glue("Data as of {max(df$date_report, na.rm=T)+1}"),
       fill = "Case category",
       x = "Week of onset (week starting)",
       y = "Weekly case count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By report date and case definition breakdown and region
ggplot() +
  geom_histogram(data = df_joined,
                 aes(x=date_report_week,
                     fill = case_def), binwidth = 7, color = "white") +
  facet_wrap(.~region) +
  scale_fill_manual(values = c("Suspected" = "lightblue",
                               "Probable" = "seagreen",
                               "Confirmed" = "navy")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Cases by onset week, presented by case category and region",
       subtitle = glue("Data as of {max(df$date_report, na.rm=T)+1}"),
       fill = "Case category",
       x = "Week of onset (week starting)",
       y = "Weekly case count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table of symptoms -----------------------------

# Pivot longer
df_symptoms_long <- df_joined |>
  pivot_longer(cols = starts_with("symp_"),
               values_to = "symptom_experienced",
               names_to = "symptom") |>
  select(id, case_def, region, symptom, symptom_experienced) |>
  mutate(symptom = str_to_title(str_replace(symptom, "symp_", ""))) |>
  mutate(symptom = str_replace(symptom, "_", " "))

# Simple table
table_symptoms <- df_symptoms_long |>
  group_by(symptom) |>
  summarize(
    experienced = sum(symptom_experienced == 1, na.rm = TRUE),
    percent = scales::percent(experienced / sum(!is.na(symptom_experienced)))) |>
  arrange(desc(experienced))

# Back to wide
df_symptoms_long %>%
  filter(symptom_experienced == 1) |>
  count(region, symptom) %>%
  pivot_wider(names_from = symptom, values_from = n, values_fill = 0)

# Table by case definition -------------------------------
table_casedefs <- df_joined |>
  tabyl(region, case_def) |>
  adorn_totals() |>
  adorn_percentages(denominator = "row") |>
  adorn_pct_formatting() |>
  adorn_ns()

table_casedefs |>
  flextable() |>
  autofit() %>%
  bold(part = "header") %>%
  bold(part = "body", i = 3) %>%
  bg(part = "header", bg = "gray90")

# Table of overall cases by recency ---------------------------

table_total <- df_joined |>
  group_by(region) |>
  summarize(Cases_total = n(),
            Cases_7days = sum(date_report_7=="new", na.rm=T),
            Deaths_total = sum(outcome=="died"),
            Deaths_percent = scales::percent(Deaths_total/Cases_total, 0.1)) |>
  adorn_totals()

table_total |>
  flextable() |>
  separate_header() |>
  set_header_labels(         # Rename the columns in original header row
    region = "Region",
    Cases_total = "Total",
    Cases_7days = "Reported in past 7 days",
    Deaths_total = "No. deaths",
    Deaths_percent = "% deaths") |>
  autofit() |>
  align(align = "center", j = c(2:5), part = "all")%>%
  bold(part = "header", i = 1) %>%
  bold(part = "body", i = 3) %>%
  bg(part = "header", bg = "gray90") |>
  bg(j = 5, i = ~ Deaths_percent >= 5, bg = "#ebbab7")






