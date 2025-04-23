# Computes a 1-D Gaussian (or its derivative) kernel.
# sigma: standard deviation
# order: 0 for the standard Gaussian, positive integers for derivatives
# radius: half-width of the kernel; if not given, use truncate * sigma.
gaussian_kernel1d <- function(sigma, order = 0, radius) {
  if (order < 0)
    stop("order must be non-negative")
  
  # Create grid: from -radius to radius.
  x <- seq(-radius, radius)
  
  # Compute the basic Gaussian (unnormalized)
  phi_x <- exp(-0.5 * (x / sigma)^2)
  phi_x <- phi_x / sum(phi_x)  # normalize so that sum(phi_x) == 1
  
  if (order == 0) {
    return(phi_x)
  } else {
    # For derivatives we need to multiply the Gaussian by a polynomial.
    # The SciPy code computes the polynomial coefficients via a matrix method.
    # Here we mimic that process.
    expo <- 0:order  # exponents 0,1,...,order
    q <- numeric(order + 1)
    q[1] <- 1  # q[0] = 1  (R indexing: first element corresponds to exponent 0)
    
    # Build the “differentiation” matrices D (superdiagonal) and P (subdiagonal)
    D <- matrix(0, nrow = order + 1, ncol = order + 1)
    for (i in 1:order) {
      D[i, i + 1] <- expo[i + 1]
    }
    P <- matrix(0, nrow = order + 1, ncol = order + 1)
    for (i in 2:(order + 1)) {
      P[i, i - 1] <- 1 / (-sigma^2)
    }
    Q_deriv <- D + P
    # Apply the operator repeatedly (order times)
    for (i in 1:order) {
      q <- Q_deriv %*% q
    }
    # For each x value, evaluate the polynomial:
    # Compute sum_{j=0}^{order} q[j] * x^j.
    # (outer(x, expo, `^`) builds a matrix whose (i,j) entry is x[i]^(expo[j]).)
    poly_val <- as.vector(outer(x, expo, `^`) %*% q)
    return(poly_val * phi_x)
  }
}

# Performs a 1-D correlation using periodic ("wrap") boundary conditions.
# input: a numeric vector.
# weights: the 1-D kernel (assumed to have odd length).
correlate1d_wrap <- function(input, weights) {
  n <- length(input)
  k <- length(weights)
  # Assume kernel size is odd so that it is symmetric around its center:
  half <- (k - 1) / 2
  result <- numeric(n)
  
  # For each element of the output, sum over the kernel with periodic indexing.
  for (i in seq_len(n)) {
    # Offsets: from -half to +half.
    offsets <- (-half):half
    # Compute wrapped indices: in R indices run 1..n.
    idx <- ((i - 1 + offsets) %% n) + 1
    result[i] <- sum(input[idx] * weights)
  }
  return(result)
}

# The main function: a 1-D Gaussian filter with periodic (wrap) boundary conditions.
#
# Arguments:
#  - input: numeric vector to filter.
#  - sigma: standard deviation of the Gaussian.
#  - order: the order of the derivative (default 0 means no derivative).
#  - truncate: how many sigmas to include in the kernel (ignored if radius is provided).
#  - radius: if provided, the half-length of the kernel; otherwise computed as round(truncate * sigma).
#
# Note: In the SciPy version the kernel is reversed before calling the correlate1d.
# We do the same here.
gaussian_filter1d <- function(input, fwhm, order = 0, truncate = 4.0, radius = NULL) {
  sigma <- fwhm_to_sigma(fwhm)
  if (!is.numeric(input)){
    stop("input must be numeric")
  }
    
  # Determine kernel half-width (radius)
  if (is.null(radius)) {
    radius <- round(truncate * sigma)
  } else if (radius < 0 || radius != as.integer(radius)) {
    stop("radius must be a nonnegative integer")
  }
  
  # Create the kernel.
  weights <- gaussian_kernel1d(sigma, order, radius)
  # In SciPy, the kernel is reversed (because of the use of correlation rather than convolution).
  weights <- rev(weights)
  
  # Apply the filter using periodic (wrap) boundary conditions.
  output <- t(apply(input, 1, function(x) correlate1d_wrap(x, weights)))
  return(output)
}


smoothed_gussian_curves_1 <- function(data, mu, sig, fwhm) {
  
  # Step 1: Smooth each curve in the data
  smoothed_data <- gaussian_filter1d(data, fwhm)
  
  
  # Step 2: Normalize the smoothed data to have unit variance
  nNodes <- ncol(smoothed_data)
  SD <- fwhm_to_sigma(fwhm)
  scale_factor <- set_scale(nNodes, SD)
  
  # Step 3: Scale the smoothed data
  smoothed_data_scaled <- smoothed_data * scale_factor
  
  # Step 4: Transform to have mean = mu and standard deviation = sig
  smoothed_data_final <- smoothed_data_scaled * sig + mu
  
  return(t(smoothed_data_final))
}

source("basic_functions.R")

set.seed(123)
noise <- noise_guassian_curve(number_of_curves = 10,
                               continuum_size = 101)
noise1 <- smoothed_gussian_curves(noise, 0, 1, 20)
noise2 <- smoothed_gussian_curves_1(noise, 0, 1, 20)
matplot(t(noise), type = "l", col = "red")
matplot((noise1), type = "l",col = "blue")
matplot((noise2), type = "l",col = "green")

########################################################
pointwise_t_test <- function(data1, data2, var.equal = FALSE, c_level = 0.05) {

  
  # For each domain point (column), perform a t-test comparing the two groups
  res <- sapply(1:ncol(data1), function(i) {
    test <- t.test(data1[, i], data2[, i], var.equal = var.equal,
                   conf.level = 1 - c_level)
    
    # Return the t-statistic, degrees of freedom, and p-value for this column
    c(statistic = test$statistic,
      df = test$parameter, 
      p_value = test$p.value)
  })
  
  # Transpose and convert to list
  results_list <- as.list(as.data.frame(t(res)))
  names(results_list) <- c("statistic", "df", "p_value")
  
  return(results_list)
}

###################################################
estimate_fwhm <- function(R) {
  
  # Sum of squares across rows for each column
  ssq <- colSums(R^2)
  
  # Machine epsilon for numerical stability
  eps <- .Machine$double.eps
  
  n_rows <- nrow(R)
  n_cols <- ncol(R)
  
  # Initialize matrix to store approximated derivatives (gradient) along columns
  dx <- matrix(NA, nrow = n_rows, ncol = n_cols)
  
  # Compute gradient along columns for each row:
  for (i in 1:n_rows) {
    # Forward difference for the first column
    dx[i, 1] <- R[i, 2] - R[i, 1]
    # Backward difference for the last column
    dx[i, n_cols] <- R[i, n_cols] - R[i, n_cols - 1]
    # Central differences for interior columns (if available)
    if (n_cols > 2) {
      dx[i, 2:(n_cols - 1)] <- (R[i, 3:n_cols] - R[i, 1:(n_cols - 2)]) / 2
    }
  }
  
  # Sum the squared gradients across rows for each column
  v <- colSums(dx^2)
  
  # Normalize by the column-wise sum of squares (plus eps for stability)
  v <- v / (ssq + eps)
  
  # Remove any NaN values (in case some columns had zero variance)
  v <- v[!is.na(v)]
  
  # Compute resels per node (using the relation with FWHM)
  reselsPerNode <- sqrt(v / (4 * log(2)))
  
  # The global FWHM estimate is the reciprocal of the average resels per node
  FWHM <- 1 / mean(reselsPerNode)
  
  return(FWHM)
}


############## residuals #################
residuals_data <- function(data1, data2) {
  # Subtract the column-wise means of each group
  r1 <- data1 - matrix(colMeans(data1), nrow = nrow(data1), ncol = ncol(data1), byrow = TRUE)
  r2 <- data2 - matrix(colMeans(data2), nrow = nrow(data2), ncol = ncol(data2), byrow = TRUE)
  return(rbind(r1, r2))
}


###################################################

SPM <- function(data1, data2, variance.equal = FALSE, Clevel = 0.05){
  
  ss1 <- dim(data1)[1]
  ss2 <- dim(data2)[1]
  Q <- dim(data1)[2]
  
  test_stat <- (pointwise_t_test(data1, data2, var.equal = variance.equal, c_level = Clevel)$statistic)^2

  D0 <- 1-pf(q = test_stat, df1 = 1, df2 = ss1+ss2-2)
  
  calculate_expression <- function(z, k, v) {
    FOUR_LOG2 <- 4 * log(2)
    TWO_PI <- 2 * pi
    a <- FOUR_LOG2 / TWO_PI
    b <- lgamma(v / 2) + lgamma(k / 2)
    
    result <- (
      sqrt(a) * exp(lgamma((v + k - 1) / 2) - b) * sqrt(2) *
        (k * z / v) ^ (0.5 * (k - 1)) *
        (1 + k * z / v) ^ (-0.5 * (v + k - 2))
    )
    
    return(result)
  }
  
  D1 <- calculate_expression(test_stat, 1, ss1+ss2-2)
  R1 <- (Q-1)/estimate_fwhm(residuals_data(data1, data2))
  p<- 1-exp(-(D0 + D1 * R1))
  
  #benferroni correction
  p_b <- 1-pf(q = test_stat, df1 = 1, df2 = ss1+ss2-2)
  correct_Q <- unlist(lapply(p_b, function(x) min(x*Q,1)))
  # Ensures P isn't too small by using Q=1 as a lower bound.
  correct_1 <- unlist(lapply(p_b, function(x) min(x*1,1)))
  # compare correct and p elementwise and return min
  p_final_step1 <- unlist(lapply(1:length(p), function(x) min(p[x],correct_Q[x])))
  # Apply Lower Bound
  p_final_step2 <- unlist(lapply(1:length(p), function(x) max(correct_1[x],p_final_step1[x])))
  
  
  return(p_final_step2)
}
