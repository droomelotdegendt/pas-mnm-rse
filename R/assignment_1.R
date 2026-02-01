#' Read and Filter spatial samples by scheme and hydr_class.
#'
#' Helper function to perform assignment 1 in one run.
#'
#' @param version poc folder name (e.g. "poc_0.14.0").
#' @param scheme Meetnet identifier (e.g. "GW_03.3")
#' @param hydr_class Waterhuishoudingstypes identifier (e.g. c("HC1","HC12"))
#'
#' @return Filtered tibble of spatial samples.
#' @export
read_and_filter_dataset <- function(version, scheme, hydr_class) {
  spatial_samples <- read_spatial_samples(version)
  filtered_spatial_samples <- filter_spatial_samples(
    spatial_samples = spatial_samples,
    scheme = scheme,
    hydr_class = hydr_class
  )
  
  cat("Total rows for", version, ":", nrow(spatial_samples), "\n")
  cat("Filtered rows for scheme", scheme, "and hydr_class", hydr_class, ":", nrow(filtered_spatial_samples), "\n")
  print(utils::head(filtered_spatial_samples[, c("scheme", "stratum", "type", "hydr_class", "grts_address")]))
  
  filtered_spatial_samples
}



