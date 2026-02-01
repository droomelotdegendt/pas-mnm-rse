#' Summarise sample sizes by stratum
#'
#' Sample size = number of unique grts_address values by stratum.
#'
#' @param spatial_samples Dataset containing spatial samples data with required columns 'stratum' and 'grts_address'.
#' @param group_by Column name used to summarize the data. Default "stratum".
#' @param output_column Column name for the output column (sample size). Default "n".
#' 
#' @return Tibble with columns stratum and n (= number of unique samples)
#' @export
summarise_sample_sizes <- function(spatial_samples, group_by="stratum", output_column="n") {
  check_dataframe(spatial_samples, "spatial_samples")
  check_character(output_column, "output_column")
  check_required_columns(c(group_by, "grts_address"), spatial_samples, "spatial_samples")
  
  group_by_sym <- rlang::sym(group_by)
  output_sym   <- rlang::sym(output_column)
  
  dplyr::as_tibble(spatial_samples) |>
    dplyr::group_by(!!group_by_sym) |>
    dplyr::summarise(
      !!output_sym := dplyr::n_distinct(.data$grts_address),
      .groups = "drop"
    )
}
