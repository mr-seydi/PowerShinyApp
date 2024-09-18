Fmax <- function(formula,
                 B = 1000,
                 method = 'residuals',
                 dx=NULL,
                 recycle=TRUE, DATA){
  
  extract_residuals <- function(regr) {
    return(regr$residuals)
  }
  
  extract_fitted <- function(regr) {
    return(regr$fitted)
  }
  
  # Extracting variables and covariates
  variables <- all.vars(formula)
  y.name <- variables[1]
  covariates.names <- colnames(attr(terms(formula), "factors"))
  
  # Creating the model frame with the full data
  data <- model.frame(formula, data = DATA)[[1]]
  
  # Handling functional data or matrix input
  if (is.fd(data)) { # If it's a functional data object
    rangeval <- data$basis$rangeval
    if (is.null(dx)) {
      dx <- (rangeval[2] - rangeval[1]) * 0.01
    }
    abscissa <- seq(rangeval[1], rangeval[2], by = dx)
    coeff <- t(eval.fd(fdobj = data, evalarg = abscissa))
  } else if (is.matrix(data)) { # If it's a matrix
    coeff <- data
  } else {
    stop("First argument of the formula must be either a functional data object or a matrix.")
  }
  
  # Creating formula for coefficients
  formula.const <- deparse(formula[[3]], width.cutoff = 500L)
  formula.discrete <- as.formula(paste('coeff ~', formula.const))
  
  # Construct the design matrix
  design_matrix <- model.matrix(formula.discrete, data = DATA)
  
  # Fit the initial model
  nvar <- dim(design_matrix)[2] - 1
  p <- dim(coeff)[2]
  n <- dim(coeff)[1]
  
  regr0 <- lm.fit(design_matrix, coeff)
  
  # Compute test statistics
  Sigma <- chol2inv(regr0$qr$qr)
  resvar <- colSums(regr0$residuals ^ 2) / regr0$df.residual
  se <- sqrt(matrix(diag(Sigma), nrow = nvar + 1, ncol = p, byrow = FALSE) 
             * matrix(resvar, nrow = nvar + 1, ncol = p, byrow = TRUE))
  T0_part <- abs(regr0$coeff / se)^2
  
  if (nvar > 0) {
    T0_glob <- colSums((regr0$fitted - matrix(colMeans(regr0$fitted),
                                              nrow = n, ncol = p, 
                                              byrow = TRUE)) ^ 2) / (nvar * resvar)
  } else {
    method <- 'responses'
    T0_glob <- numeric(p)
    T0_part <- t(as.matrix(T0_part))
  }
  
  # Compute residuals for permutations
  if (method == 'residuals') {
    residui <- array(dim = c(nvar + 1, n, p))
    fitted_part <- array(dim = c(nvar + 1, n, p))
    
    for (ii in 1:(nvar + 1)) {
      if (ii == 1) {
        # Intercept model
        regr0_part <- lm.fit(design_matrix, coeff)
        residui[ii, , ] <- regr0_part$residuals
        fitted_part[ii, , ] <- regr0_part$fitted
      } else {
        # Partial model with one variable removed
        reduced_design <- design_matrix[, -ii]
        regr0_part <- lm.fit(as.matrix(reduced_design), coeff)
        residui[ii, , ] <- regr0_part$residuals
        fitted_part[ii, , ] <- regr0_part$fitted
      }
    }
  }
  
  # Permutation test loop
  T_glob <- matrix(ncol = p, nrow = B)
  T_part <- array(dim = c(B, nvar + 1, p))
  
  for (perm in 1:B) {
    if (nvar > 0) {
      permutated_indices <- sample(n)
      coeff_perm <- coeff[permutated_indices, ]
    } else {
      # Permute signs for intercept-only model
      signs <- rbinom(n, 1, 0.5) * 2 - 1
      coeff_perm <- coeff * signs
    }
    
    regr_perm <- lm.fit(design_matrix, coeff_perm)
    Sigma_perm <- chol2inv(regr_perm$qr$qr)
    resvar_perm <- colSums(regr_perm$residuals ^ 2) / regr_perm$df.residual
    
    if (nvar > 0) {
      T_glob[perm, ] <- colSums((regr_perm$fitted - matrix(colMeans(regr_perm$fitted),
                                                           nrow = n, ncol = p,
                                                           byrow = TRUE)) ^ 2) / (nvar * resvar_perm)
    }
    
    if (method == 'residuals') {
      for (ii in 1:(nvar + 1)) {
        residui_perm <- residui[ii, permutated_indices, ]
        coeff_perm <- fitted_part[ii, , ] + residui_perm
        regr_perm_part <- lm.fit(design_matrix, coeff_perm)
        Sigma_perm_part <- chol2inv(regr_perm_part$qr$qr)
        resvar_perm_part <- colSums(regr_perm_part$residuals ^ 2) / regr_perm_part$df.residual
        se_perm_part <- sqrt(matrix(diag(Sigma_perm_part), nrow = nvar + 1, ncol = p, byrow = FALSE) 
                             * matrix(resvar_perm_part, nrow = nvar + 1, ncol = p, byrow = TRUE))
        T_part[perm, ii, ] <- abs(regr_perm_part$coeff / se_perm_part)[ii, ]^2
      }
    }
  }
  
  # Compute p-values
  pval_glob <- numeric(p)
  pval_part <- matrix(nrow = nvar + 1, ncol = p)
  pval_glob_adj <- numeric(p)
  pval_part_adj <- matrix(nrow = nvar + 1, ncol = p)
  
  maxF_glob <- apply(T_glob, 1, max)
  maxF_part <- apply(T_part, c(1, 2), max)
  
  for (i in 1:p) {
    pval_glob[i] <- sum(T_glob[, i] >= T0_glob[i]) / B
    pval_part[, i] <- colSums(T_part[, , i] >= matrix(T0_part[, i], nrow = B, ncol = nvar + 1, byrow = TRUE)) / B
    pval_glob_adj[i] <- sum(maxF_glob >= T0_glob[i]) / B
    pval_part_adj[, i] <- colSums(maxF_part >= matrix(T0_part[, i], nrow = B, ncol = nvar + 1, byrow = TRUE)) / B
  }
  
  result <- list(call = match.call(),
                 unadjusted_pval_F = pval_glob,
                 adjusted_pval_F = pval_glob_adj,
                 unadjusted_pval_part = pval_part,
                 adjusted_pval_part = pval_part_adj)
  
  return(result)
}
