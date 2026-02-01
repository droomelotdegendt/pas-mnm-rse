#' Filter spatial samples by scheme and hydr_class.
#'
#' The hydr_class per type is obtained via `n2khab::read_types()[, c("type","hydr_class")]`.
#' More info can be found on: https://github.com/inbo/n2khab
#'
#' The hydr_class data is matched to the input dataframe by `type`, which can be
#' retrieved from `stratum` by extracting the part before the first underscore
#' (if present). If not present, type is equal to stratum.
#'
#' @param spatial_samples A data.frame containing spatial samples data.
#' @param scheme The scheme ('meetnet') you want to retrieve data for.
#' @param hydr_class A list of hydr_classes ('waterhuishoudingstypes') you want to retrieve data for.
#'
#' @return The filtered input table for the given scheme and hydr_class.
#' @export
filter_spatial_samples <- function(spatial_samples, scheme, hydr_class) {
  check_dataframe(spatial_samples, "spatial_samples")
  check_character(scheme, "scheme")
  check_character_vector(hydr_class, "hydr_class")
  check_required_columns(c("scheme", "stratum"), spatial_samples, "spatial_samples")
  
  if (!requireNamespace("n2khab", quietly = TRUE)) {
    stop("Package 'n2khab' is required for hydr_class filtering.")
  }
  
  types <- n2khab::read_types()[, c("type", "hydr_class")]
  
  out <- tibble::as_tibble(spatial_samples) |>
    dplyr::mutate(type = stringr::str_replace(.data$stratum, "_.*$", "")) |>
    dplyr::left_join(types, by = "type")
  
  unmatched <- out |>
    dplyr::filter(is.na(.data$hydr_class)) |>
    dplyr::distinct(.data$type) |>
    dplyr::pull(.data$type)
  
  if (length(unmatched) > 0L) {
    warning(
      "Some stratum types could not be matched to hydr_class (showing up to 10): ",
      paste(utils::head(unmatched, 10), collapse = ", "),
      call. = FALSE
    )
  }
  
  out <- out |>
    dplyr::filter(.data$scheme == !!scheme, .data$hydr_class %in% !!hydr_class)
  
  check_empty_dataframe(
    out,
    sprintf(
      "No rows after filtering for scheme = '%s' and hydr_class = %s.",
      scheme,
      paste(hydr_class, collapse = ", ")
    )
  )
  
  out
}
