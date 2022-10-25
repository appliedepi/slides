

# Load packages
pacman::p_load(rio, googlesheets4, tidyr, stringr, janitor)

# list all langs desired
langs <- c("fr", "en")
slides_folders <- list.files(here::here("intro", "en"), recursive = FALSE)

# Create function
update_slides <- function(folder = "module_10", rmd = "module_10.Rmd", lang_to = "fr"){

  # define folder paths
  current_folder <- here::here("intro", "skeleton", folder)
  destination_folder <- here::here("intro", lang_to, folder)

  # render the rmd
  xaringan::infinite_moon_reader(moon = here::here("intro", "skeleton", folder, rmd),
                                 params = list(lang = lang_to))

  # ensure the folder exists, or is erased then re-created
  if(!file.exists(destination_folder)) {
    # If the folder does not exist, create a new one
    dir.create(destination_folder, recursive = TRUE)
  } else {
    # If it existed, delete and replace with a new one
    unlink(destination_folder, recursive = TRUE)
    dir.create(destination_folder, recursive=TRUE)
    }

  # move all files and subfolders
  files <- list.files(path = current_folder, full.names = TRUE, recursive = TRUE)
  for (f in files) file.copy(list.files(current_folder, full.names = TRUE),
                             destination_folder,
                             recursive = TRUE)
}



# Run for each language and module ----------------------------------------

for (l in langs) {
  for (slide in slides_folders) {
    update_slides(folder = slide,
                  rmd = paste0(slide, ".Rmd"),
                  lang_to = l)
  }
}







#Import from google sheet; save as dataframes ----------------------------

# gen <- googlesheets4::read_sheet(
#   "https://docs.google.com/spreadsheets/d/1-Xqv5xvakmhKSxipVP6a9GF3H54RN-rbilhdNhPHx6M/edit#gid=998182281",
#   range = "generic") %>%
#   clean_names()
#
# mod1 <- googlesheets4::read_sheet(
#   "https://docs.google.com/spreadsheets/d/1-Xqv5xvakmhKSxipVP6a9GF3H54RN-rbilhdNhPHx6M/edit#gid=998182281",
#   range = "mod1", col_types = "c") %>%
#   clean_names()
