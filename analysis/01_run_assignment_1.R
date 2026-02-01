library(pasmnmrse)

version <- "poc_0.14.0"
scheme <- "GW_03.3"
hydr_class <- c("HC1", "HC12", "HC2")

assignment_1 <- read_and_filter_dataset(version, scheme, hydr_class)
