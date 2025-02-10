# Download data produced by aesim project and uploaded to google drive--------------------

# Start with authentication
library(googledrive)
drive_auth()

# Deleting old data files ---------------
local_folder <- "intror/"
files_local <- list.files(local_folder, full.names = TRUE, pattern = "^gi")

if (readline("Are you sure you want to delete files? Press 1 to proceed: ") == "1") {
  cat("Running code...\n")
  file.remove(files_local)
} else {
  cat("Canceled.\n")
}

# Download google drive files ----------------
gfolder <- "https://drive.google.com/drive/u/0/folders/1gZyQVAJqJPSG6lij-rEFooSdEoRSawAO"
files_in_drive <- drive_ls(as_id(gfolder))

lapply(files_in_drive$id, function(file_id) {
  drive_download(as_id(file_id), path = file.path(local_folder, files_in_drive$name[files_in_drive$id == file_id]), overwrite = TRUE)
})
