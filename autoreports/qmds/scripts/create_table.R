# Code to create the summary table

table_cat <- linelist |> 
  group_by(case_cat) |> 
  summarise(cases = n(),
            onset_earliest = min(date_onset, na.rm = T),
            onset_recent = max(date_onset, na.rm = T)) |> 
  flextable() |> 
  set_header_labels( 
    case_cat = "Category",
    cases = "Number",
    onset_earliest = "Earliest onset",
    onset_recent = "Most recent onset") |> 
  autofit()



