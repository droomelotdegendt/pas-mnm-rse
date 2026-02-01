check_character <- function(char_value, name) {
  if (!is.character(char_value) || length(char_value) != 1 || !nzchar(char_value)) {
    stop(name, "must be a non-empty string.")
  }
}

check_character_vector <- function(char_value, name) {
  if (!is.character(char_value) || length(char_value) == 0) {
    stop(name, "must be a non-empty character vector.")
  }
}

check_dataframe <- function(dataset, name) {
  if (!is.data.frame(dataset)) {
    stop(name, "must be a data.frame.")
  }
}

check_required_columns <- function(required_cols, dataset, dataset_name) {
  missing <- setdiff(required_cols, names(dataset))
  if (length(missing) > 0L) {
    stop(
      sprintf(dataset_name, "is missing required columns: %s", paste(missing, collapse = ", "))
    )
  }
}

check_empty_dataframe <- function(dataset, message) {
  if (nrow(dataset) == 0L) {
    stop(message)
  }
}