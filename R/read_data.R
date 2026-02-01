#' Read spatial_samples.csv for a given poc folder.
#'
#' Expected path: <data_dir>/<poc_folder_name>/samples/spatial_samples.csv
#' (If needed the samples folder name and file name can be adjusted.)
#'
#' @param poc_folder_name Folder name of the poc version, (e.g. "poc_0.14.0").
#' @param base_dir Base folder containing the poc data. Default "inst/extdata".
#' @param samples_folder_name The folder in which the samples file is saved. Default "samples".
#' @param samples_file_name The name of the spatial samples file. Default "spatial_samples.csv".
#' @param required_cols List of required columns in the input file.
#'
#' @return A table containing the spatial samples data.
#' @export
read_spatial_samples <- function(poc_folder_name,
                                 base_dir = "inst/extdata",
                                 samples_folder_name = "samples",
                                 samples_file_name = "spatial_samples.csv",
                                 required_cols = c("scheme", "stratum", "grts_address")) {
  check_character(poc_folder_name, "poc_folder_name")
  
  path <- file.path(base_dir,
                    poc_folder_name,
                    samples_folder_name,
                    samples_file_name)
  
  if (!file.exists(path)) {
    stop(sprintf("File not found: '%s'", path))
  }
  
  samples_file <- utils::read.csv(path, stringsAsFactors = FALSE)
  
  check_required_columns(required_cols, samples_file, path)
  
  dplyr::as_tibble(samples_file)
}
