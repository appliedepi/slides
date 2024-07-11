pacman::p_load(
  readxl,
  skimr,
  janitor,
  tidyverse
)

# Function to translate an R script file
translate_r_script <- function(file_path, translation_data) {
  # Read the content of the script file
  script_content <- readLines(file_path)

  # Translate the content line by line
  translated_content <- lapply(script_content, function(line) {
    # Check if the line contains any English column names
    for (i in 1:nrow(translation_data)) {
      if (grepl(paste0("\\b", translation_data$Original[i], "\\b"), line)) {
        # Translate the matched English column name to Spanish
        line <- gsub(paste0("\\b", translation_data$Original[i], "\\b"),
                     translation_data$Traduccion[i], line)
      }
    }
    return(line)
  })

  # Convert translated_content to character vector
  translated_content <- sapply(translated_content, as.character)

  # Write the translated content back to the original file
  writeLines(translated_content, file_path)
}


# Example usage:
# Read the translation data
translation_data <- read_excel("eng_to_es.xlsx")

# Get a list of all R script files in the folder
script_files <- list.files(path = "intro/intro05-2_es/",
                           pattern = "\\.Rmd$",
                           full.names = T)

# Translate each R script file
for (script_file in script_files) {
  translate_r_script(script_file, translation_data)
}
