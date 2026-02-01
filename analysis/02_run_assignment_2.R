library(pasmnmrse)

version_a <- "poc_0.13.1"
version_b <- "poc_0.14.0"
scheme <- "GW_03.3"
hydr_class <- c("HC1", "HC12", "HC2")

assignment_2 <- create_graph_comparison(version_a, version_b, scheme, hydr_class)
