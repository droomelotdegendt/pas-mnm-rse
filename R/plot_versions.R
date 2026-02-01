#' Dumbbell plot for comparing two versions by stratum.
#'
#' @param dataset A dataframe with all necessary info.
#' @param column_a Column containing sample size for version a.
#' @param column_b Column containing sample size for version b.
#' @param column_delta Column containing difference between column a and b. Default "delta".
#' @param comparison_by Column used to group the plot. Default "stratum".
#' @param label_a Label for version a. Defaults to column_a name.
#' @param label_b Label for version b. Defaults to column_b name.
#' @param top_n Optional integer: keep only top_n for readability (by abs(delta)).
#'
#' @return A ggplot object.
#' @export
plot_sample_size_dumbbell <- function(
    dataset,
    column_a,
    column_b,
    column_delta = "delta",
    comparison_by = "stratum",
    label_a = NULL,
    label_b = NULL,
    top_n = NULL
) {
  check_dataframe(dataset, "dataset")
  check_required_columns(c(column_a, column_b, column_delta, comparison_by), dataset, "dataset")
  
  if (is.null(label_a)) label_a <- column_a
  if (is.null(label_b)) label_b <- column_b
  
  df <- dplyr::as_tibble(dataset) |>
    dplyr::transmute(
      plot_by = .data[[comparison_by]],
      n_a = .data[[column_a]],
      n_b = .data[[column_b]],
      delta = .data[[column_delta]]
    )
  
  if (!is.null(top_n)) {
    df <- df |>
      dplyr::slice_max(order_by = abs(.data$delta), n = top_n, with_ties = FALSE)
  }
  
  # order data by delta
  df <- df |>
    dplyr::arrange(.data$delta) |>
    dplyr::mutate(plot_by = factor(.data$plot_by, levels = .data$plot_by))
  
  # long data for points (legend)
  pts <- df |>
    tidyr::pivot_longer(
      cols = c(.data$n_a, .data$n_b),
      names_to = "which",
      values_to = "n"
    ) |>
    dplyr::mutate(
      version = dplyr::case_when(
        which == "n_a" ~ label_a,
        which == "n_b" ~ label_b
      )
    )
  
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(y = .data$plot_by, yend = .data$plot_by, x = .data$n_a, xend = .data$n_b),
      linewidth = 1,
      alpha = 0.7
    ) +
    ggplot2::geom_point(
      data = pts,
      ggplot2::aes(x = .data$n, y = .data$plot_by, color = version),
      size = 2.5
    ) +
    ggplot2::labs(
      title = sprintf("Sample sizes per %s: %s vs %s", comparison_by, label_a, label_b),
      subtitle = "Dots show each version; line connects the same stratum",
      x = "Sample size",
      y = comparison_by,
      color = "Version"
    ) +
    ggplot2::theme_minimal()
}



