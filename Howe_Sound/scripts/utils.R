

downloadGdriveFolder <- function (id, file_path, skip_if_exists = TRUE) {
  exists <- file.exists(file_path)
  if (!exists || !skip_if_exists) {
    if (!exists) {
      dir.create(file_path)
    }
    # folder link to id
    folder_drib = idToDrib(id)
    
    # find files in folder
    files = drive_ls(folder_drib)
    
    cat("Fetching ", nrow(files), " files in folder ", folder_drib$name, "\n")
    
    # loop dirs and download files inside them
    for (i in seq_along(files$name)) {
      resource <- files$drive_resource[[i]]
      
      target <- str_c(file_path, "/", resource$name)
      if (resource$mimeType == "application/vnd.google-apps.folder") {
        cat (resource$name, " is a folder\n")
        downloadGdriveFolder(resource$id, target, skip_if_exists)
        # If there were subfolders, this would list them:
        # i_dir = drive_ls(files[i, ])
      }
      else {
        
        try({
          if (file.exists(target)) {
            wg("File {target} already exists, skipping download")
          } else {
            drive_download(as_id(files$id[i]), path = target)
          } 
        })
      }
    }
  } else {
    wg("Path {file_path} already exists, skipping download\n")
  }
}