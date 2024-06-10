# NEW LANGUAGE TRANSLATION PROTOCOL ------------------------------------------------------------------------

# 0. Create a new branch for a new language

# 1. First install the babeldown package
#install.packages('babeldown', repos = c('https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))


# 2.Setup the environment variable
Sys.setenv("DEEPL_API_URL" = "https://api.deepl.com")
Sys.setenv(DEEPL_API_KEY = "287d5481-9d96-8500-228c-6f98cfb3c576")



# 3. Indicate modules to be translated
module_list = c(
  "intro01",
  # "intro02",
  "intro03",
  "intro04",
  # "intro05-1",
  "intro05-2",
  # "intro06",
  # "intro07",
  "intro08-1",
  "intro08-2",
  "intro10"
  )

base_directory <- here::here("intro")

languages <- c("pt") # "fr", "pt" Add more languages as needed

# 4. Write a loop to run the translation for each module
for (i in 1:length(module_list)) {
  for (j in 1:length(languages)) {
    babeldown::deepl_translate_quarto(
      book_path = paste0(base_directory, "/", module_list[i]),
      chapter = paste0(module_list[i], ".Rmd"),
      force = TRUE,
      render = FALSE, # Whether to run babelquarto::render_bool() after translation.
      source_lang = "EN",
      target_lang = toupper(languages[j]),
      formality = "less")
  }
}


# 5. Create new folder for language versions and move files


# Function to process each module
create_LanguageModule <- function(base_directory, module_name, languages) {
  for (lang in languages) {
    # Construct the file and folder names
    file_name <- paste0(module_name, ".", lang, ".Rmd")
    file_name_new <- paste0(module_name, "_", lang, ".Rmd")
    folder_name <- paste0(module_name, "_", lang)
    file_path <- file.path(base_directory, module_name, file_name)
    new_folder_path <- file.path(base_directory, folder_name)
    css_file_path <- file.path(base_directory, module_name, "xaringan-themer.css") # Path to the CSS file

    # Check if the file exists
    if (file.exists(file_path)) {
      # Create language-specific folder if it doesn't exist
      if (!dir.exists(new_folder_path)) {
        dir.create(new_folder_path, recursive = TRUE)
      }

      # Move the .Rmd files
      file.rename(from = file_path, to = file.path(new_folder_path, file_name_new))

      # Check and copy the xaringan-themer.css file if it exists
      if (file.exists(css_file_path)) {
        file.copy(from = css_file_path, to = new_folder_path, recursive = TRUE)
      }
    }
  }
}

# Apply the function to each module
for (module in module_list) {
  create_LanguageModule(base_directory, module, languages)
}

# 6. Review the translation





