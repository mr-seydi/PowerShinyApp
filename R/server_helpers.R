# Server helpers for file input, pilot-data analysis, and input validation.

detect_delimiter <- function(file_path) {
  first_line <- readLines(file_path, n = 1)

  if (grepl(";", first_line)) {
    return(";")
  }

  ","
}

estimate_fwhm <- function(R) {
  ssq <- colSums(R^2)
  eps <- .Machine$double.eps

  n_rows <- nrow(R)
  n_cols <- ncol(R)
  dx <- matrix(NA, nrow = n_rows, ncol = n_cols)

  for (i in 1:n_rows) {
    dx[i, 1] <- R[i, 2] - R[i, 1]
    dx[i, n_cols] <- R[i, n_cols] - R[i, n_cols - 1]

    if (n_cols > 2) {
      dx[i, 2:(n_cols - 1)] <- (R[i, 3:n_cols] - R[i, 1:(n_cols - 2)]) / 2
    }
  }

  v <- colSums(dx^2)
  v <- v / (ssq + eps)
  v <- v[!is.na(v)]
  resels_per_node <- sqrt(v / (4 * log(2)))

  1 / mean(resels_per_node)
}

integrated_sd <- function(pointwise_sd) {
  sqrt(mean(pointwise_sd^2))
}

check_data <- function(y) {
  if (any(is.na(y))) {
    warning("Data contains NA values, please complete the data before power analysis.")
  }
  if (dim(y)[1] != 101) {
    warning("All columns of data domain must have 101 equally spaced observations.")
  }
  if (dim(y)[2] == 1) {
    warning("You need at least two columns of data to estimate standard deviation.")
  }
}

prepare_pilot_groups <- function(data, group1_columns, group2_columns) {
  group1_selected <- data[, group1_columns, drop = FALSE]
  group2_selected <- data[, group2_columns, drop = FALSE]

  check_data(group1_selected)
  check_data(group2_selected)

  group1_data <- as.matrix(group1_selected)
  group2_data <- as.matrix(group2_selected)

  group1_centered <- group1_data - matrix(
    rowMeans(group1_data), nrow = nrow(group1_data), ncol = ncol(group1_data)
  )
  group2_centered <- group2_data - matrix(
    rowMeans(group2_data), nrow = nrow(group2_data), ncol = ncol(group2_data)
  )

  list(
    data = cbind(Group1 = rowMeans(group1_data), Group2 = rowMeans(group2_data)),
    fwhm = estimate_fwhm(rbind(t(group1_centered), t(group2_centered))),
    group1_sd = integrated_sd(apply(group1_centered, 1, sd)),
    group2_sd = integrated_sd(apply(group2_centered, 1, sd))
  )
}

validate_input <- function(input_value, min_value, default_value, error_message) {
  if (!is.numeric(input_value) || input_value < min_value) {
    return(list(valid = FALSE, value = default_value, message = error_message))
  }

  list(valid = TRUE, value = input_value)
}

calculate_default_sigma <- function(data) {
  if (is.null(data)) {
    return(0)
  }

  round((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / 2, digits = 1)
}
