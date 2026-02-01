#' Compare sample sizes by stratum between two versions
#' 
#' Returns a merged dataframe of the two input tables with an extra column
#' `delta` containing the difference between the input columns (b-a).
#' 
#' @param dataset_a Dataset containing summarized data of source a.
#' @param dataset_b Dataset containing summarized data of source b.
#' @param column_a Column in dataset_a containing the summarized value.
#' @param column_b Column in dataset_b containing the summarized value.
#' @param join_by Column used to join both datasets. Default "stratum".
#' 
#' @return Tibble with columns stratum and n (= number of unique samples)
#'
#' @export 
compare_sample_sizes <- function(
    dataset_a,
    dataset_b,
    column_a,
    column_b,
    join_by="stratum"
) {
  check_dataframe(dataset_a, "dataset_a")
  check_dataframe(dataset_b, "dataset_b")
  check_required_columns(c(column_a, join_by), dataset_a, "dataset_a")
  check_required_columns(c(column_b, join_by), dataset_b, "dataset_b")
  
  dplyr::full_join(dataset_a, dataset_b, by = join_by) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c(column_a, column_b)), ~ tidyr::replace_na(.x, 0)),
      delta = .data[[column_b]] - .data[[column_a]]
    )
}
