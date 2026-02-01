#' Create a comparison plot of sample sizes between two versions
#'
#' Helper function to perform assignment 2 in one run.
#'
#' @param version_a poc folder name (e.g. "poc_0.13.1").
#' @param version_b poc folder name (e.g. "poc_0.14.0").
#' @param scheme Meetnet identifier (e.g. "GW_03.3")
#' @param hydr_class Waterhuishoudingstypes identifier (e.g. c("HC1","HC12"))
#' @param top_n Optional integer. If provided, keeps only the top `top_n` groups.
#'
#' @return A ggplot object.
#' @export

create_graph_comparison <- function(version_a,
                                    version_b,
                                    scheme,
                                    hydr_class,
                                    top_n = NULL) {
  samples_a <- read_spatial_samples(version_a)
  filtered_a <- filter_spatial_samples(samples_a, scheme, hydr_class)
  summary_a <- summarise_sample_sizes(filtered_a, output_column = "n_a")
  
  samples_b <- read_spatial_samples(version_b)
  filtered_b <- filter_spatial_samples(samples_b, scheme, hydr_class)
  summary_b <- summarise_sample_sizes(filtered_b, output_column = "n_b")
  
  comparison <- compare_sample_sizes(summary_a, summary_b, "n_a", "n_b")
  p <- plot_sample_size_dumbbell(
    comparison,
    "n_a",
    "n_b",
    label_a = version_a,
    label_b = version_b,
    top_n = top_n
  )
  print(p)
}
