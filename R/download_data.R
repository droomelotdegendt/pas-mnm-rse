#' Download all spatial_samples.csv files from the PAS Drive folder
#'
#' Downloads every `poc_*` folder that contains a `samples/spatial_samples.csv`
#' file and saves it into the local `data/` directory.
#'
#' @param drive_folder_id Google drive folder where the poc data is stored.
#' @param samples_folder_name The folder in which the samples file is saved. Default "samples".
#' @param samples_file_name The name of the spatial samples file. Default "spatial_samples.csv".
#' @param dest_dir Local directory where the downloaded data will be saved. Default "inst/extdata".
#'
#' @export
download_all_poc_spatial_samples <- function(drive_folder_id = "1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT",
                                             samples_folder_name = "samples",
                                             samples_file_name = "spatial_samples.csv",
                                             dest_dir = "inst/extdata") {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required.")
  }
  
  root <- googledrive::drive_get(googledrive::as_id(drive_folder_id))
  versions <- googledrive::drive_ls(root, pattern = "^poc_")
  
  if (nrow(versions) == 0) {
    stop("No poc_* folders found in Drive folder.")
  }
  
  message("Found", nrow(versions), "versions.")
  
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (i in seq_len(nrow(versions))) {
    version_name <- versions$name[i]
    message("Downloading:", version_name)
    
    samples_folder <- googledrive::drive_ls(versions[i, ], pattern = paste0("^", samples_folder_name, "$"))
    
    if (nrow(samples_folder) == 0) {
      warning("No samples folder'",
              samples_folder_name,
              "'for",
              version_name)
      next
    }
    
    samples_file <- googledrive::drive_ls(samples_folder, pattern = paste0("^", samples_file_name, "$"))
    
    if (nrow(samples_file) == 0) {
      warning("No samples file'",
              samples_file_name,
              "'for",
              version_name)
      next
    }
    
    local_dir <- file.path(dest_dir, version_name, samples_folder_name)
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
    
    googledrive::drive_download(
      file = samples_file,
      path = file.path(local_dir, samples_file_name),
      overwrite = TRUE
    )
  }
  
  message("All csv files downloaded into:", dest_dir)
}
