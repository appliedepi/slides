
# For screen shot
quarto_render(
  input = "scripts/surveillance_report.qmd",
  output_file = "outputs/Surveillance Report.docx"
)


# For real - run version 4 (male/female - although no need to show the male/female params output, just the YAML)
library(quarto)

quarto_render(
  input = "scripts/example_exercise_4.qmd",
  output_file = "Surveillance Report.docx",
)

# For real - run version 5 where code is sourced. Note that the cross-refs are removed

quarto_render(
  input = "scripts/example_exercise_5.qmd",
  output_file = "Surveillance Report.docx",
)



# For real - example at the end with dynamic report
quarto_render(
  input = "scripts/example_exercise_4.qmd",
  output_file = paste0("Surveillance Report_", Sys.Date(), ".docx"),
)