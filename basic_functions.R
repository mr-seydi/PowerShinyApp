#TWT function
source("TWT2.R")
#path for vgrf_mean_data
path <- "vgrf_fig2.txt"


#############Loading Library#####################
library(fda)
library(fdatest)
#install.packages("reticulate")
library(reticulate) # to read python code
#pip install spm1d
spm1d <- reticulate::import("spm1d")
numpy <- reticulate::import("numpy")
#pip install power1d
power1d <- reticulate::import("power1d")

scipy.ndimage <- reticulate::import("scipy.ndimage")
################Data with NA#########################
completed_data <- function(x, y, defined_domain=c(0,100)) {
  
  if (length(defined_domain) != 2) {
    stop("Domain must have a form like c(starting point,ending point)")
  }
  start_domain <- defined_domain[1]
  end_domain <- defined_domain[2]

  # Perform linear interpolation using approx() function
  xnew <- seq(start_domain, end_domain, by = 1)  # New x values with steps of 1
  interpolated <- approx(x, y, xout = xnew)  # Interpolate y values
  
  # Return new y values
  return(interpolated$y)
}


###################Functions###########################

sigma_to_fwhm <- function(sigma){
  return(sigma*(2 * sqrt(2 * log(2))))
}

fwhm_to_sigma <- function(fwhm){
  return(fwhm / (2 * sqrt(2 * log(2))))
}

#Normal kernel smoothing function
# ksmooth_gaussian <- function(x, fwhm) { 
#   sigma <- fwhm_to_sigma(fwhm)
#   x_vals <- 1:length(x)
#   smoothed_vals <- numeric(length = length(x))
#   
#   
#   for (x_position in x_vals) {
#     kernel <- exp(-((x_vals - x_position)^2) / (2 * sigma^2))
#     #kernel <- exp(-((x_vals - x_position)^2) / (2 * sigma^2))/sqrt(2*pi*sigma^2)
#     #Results for both Kernels are the same
#     kernel <- kernel / sum(kernel)
#     smoothed_vals[x_position] <- sum(x * kernel)
#   }
#   
#   return(smoothed_vals)
# }
ksmooth_gaussian <- function(x, fwhm) {
  
  SD = fwhm_to_sigma(fwhm)
  smoothed_vals = ksmooth(x = 1:length(x), y = x, bandwidth = SD, kernel = "normal")$y
  #smoothed_vals = scipy.ndimage$gaussian_filter1d(x, SD, mode='wrap')$tolist()
}


#unsmoothed guassian noise curve
noise_guassian_curve <- function(number_of_curves, continuum_size){ 
  data <- matrix(rnorm(number_of_curves*continuum_size),
                       nrow = number_of_curves, ncol = continuum_size)
  return(data)
  
}


#smoothed gussian noise curves
smoothed_gussian_curves <- function(data, mu, sig, fwhm){
  smoothed_data <- apply(data, 1, FUN = function(x) ksmooth_gaussian(x,fwhm))
  standard_smoothed_data=apply(smoothed_data, 2, scale)
  trans_data=apply(standard_smoothed_data, 2, FUN = function(x){(mu+x*sig)})
  return(trans_data) #dim(output)=continuum_size*number_of_curves
}

# Generating data (Baseline+noise+signal) or (Baseline+noise) or (Baseline+signal)
data_generator <- function(data,signal,noise) {
  sample_size <- dim(noise)[2]
  data_baseline <- matrix(rep(data,time=sample_size),
                          ncol = sample_size)
  if (missing(signal)) {
    data_out <- data_baseline + noise
  } else if (missing(noise)) {
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline
  } else {
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline + noise 
  }
  return(data_out)
}



gaussian_pulse <- function(center, fwhm, continuum_size) {
  sigma = fwhm_to_sigma(fwhm)
  x_values = seq(0, continuum_size-1, by = 1)
  dens <- dnorm(x_values, mean = center, sd = sigma)
  return(list(density_val=dens, x_values=x_values))
}

amplitude_pulse <- function(data, amp){
  scaling_factor = amp / max(data)
  y_values = scaling_factor * data
  return(y_values)
}


##############Methods functions###########

Power_calculator <- function(Methods ,Sample_size, Iter_number, Baseline_data, Signal, Conti_size,
                             Noise_mu, Noise_sig, Noise_fwhm, Alpha){
  
  power_list <- list()
  
  for (M in Methods) {
    p_values <- matrix(NA,nrow = Conti_size, ncol = Iter_number)
    power_list[[M]] <- p_values
  }
  
  for (i in 1:Iter_number) {
    
    noise1 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    noise2 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    
    noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
    noise2 <- smoothed_gussian_curves(noise2, Noise_mu, Noise_sig, Noise_fwhm)
    
    data1 <- data_generator(data = Baseline_data, signal = Signal, noise = noise1)
    data2 <- data_generator(data = Baseline_data, noise = noise2)
    
    for (M in Methods) {
      power_list[[M]][,i] <- Power_method(sampel1 = t(data1), sample2 = t(data2), method = M)
    }
  }
  
  for (M in Methods) {
    pvalue_less_alpha=power_list[[M]]<Alpha
    power=sum(colSums(pvalue_less_alpha)>0)/Iter_number
    power_list[[M]] <- power
  }
  
  return(power_list)
}


# Power_calculator <- function(Method ,Sample_size, Iter_number, Baseline_data, Signal, Conti_size,
#                              Noise_mu, Noise_sig, Noise_fwhm, Alpha){
#   
#   p_values <- matrix(NA,nrow = Conti_size, ncol = Iter_number)
#   for (i in 1:Iter_number) {
#     
#     noise1 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
#     noise2 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
#     
#     noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
#     noise2 <- smoothed_gussian_curves(noise2, Noise_mu, Noise_sig, Noise_fwhm)
#     
#     data1 <- data_generator(data = Baseline_data, signal = Signal, noise = noise1)
#     data2 <- data_generator(data = Baseline_data, noise = noise2)
#     
#     p_values[,i] <- Power_method(sampel1 = t(data1), sample2 = t(data2), method = Method)
#     #p_values dimension is continuum_size*Iter_number
#   }
#   #dim(p_values)
#   pvalue_less_alpha=p_values<Alpha
#   power=sum(colSums(pvalue_less_alpha)>0)/Iter_number
#   return(power)
# }



Power_method <- function(sampel1,sample2,method) {
  if (method=="IWT") {
    pval <- IWT(sampel1,sample2)
  } else if (method=="TWT"){
    pval <- TWT(sampel1,sample2)
  } else if (method=="Parametric_SPM"){
    pval <- p_spm(sampel1,sample2)
  } else if (method=="Nonparametric_SPM"){
    pval <- p_snpm(sampel1,sample2)
  } else {
    stop("Choose a method between options")
  }
  return(pval)
}

IWT <- function(data1,data2){
  IWT2=IWT2(data1, data2)
  pvalue_adj_IWT2=IWT2$adjusted_pval
  return(pvalue_adj_IWT2)
}

TWT <- function(data1,data2){
  TWT2=TWT2_new(data1,data2)
  pvalue_adj_TWT2=TWT2$adjusted_pval
  return(pvalue_adj_TWT2)
}

p_spm <- function(data1, data2){
  spm  <- spm1d$stats$ttest2(data1, data2, equal_var=FALSE)
  p_val <- spm1d$rft1d$f$sf((spm$z)^2, spm$df, spm$Q, spm$fwhm, withBonf=TRUE)
  return(p_val)
}

p_snpm <- function(data1, data2, niter=1000){
  snpm       = spm1d$stats$nonparam$ttest2(data1, data2)
  if (niter > snpm$nPermUnique){
    niter      = niter-1
  } else {
    niter
  }
  snpmi      = snpm$inference(iterations=as.integer(niter))
  pdf        = numpy$abs(snpmi$PDF0)
  p <- sapply(abs(snpmi$z), function(z) mean(pdf > z))
  return(p)
} 



global_test_spm <- function(Sample_size, Iter_number, Baseline_data, Signal, Conti_size,
                            Noise_mu, Noise_sig, Noise_fwhm, Alpha){
  
  tstat_2sample_null<-matrix(NA,nrow = Iter_number  ,ncol = Conti_size)
  tstat_2sample_alt<-matrix(NA,nrow = Iter_number,ncol = Conti_size)
  
  for (i in 1:Iter_number) {
    
    noise_null_1 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    noise_null_2 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    
    noise_null_1 <- smoothed_gussian_curves(noise_null_1, Noise_mu, Noise_sig, Noise_fwhm)
    noise_null_2 <- smoothed_gussian_curves(noise_null_2, Noise_mu, Noise_sig, Noise_fwhm)
    
    data_null_1 <- data_generator(data = Baseline_data, noise = noise_null_1)
    data_null_2 <- data_generator(data = Baseline_data, noise = noise_null_2)
    
    noise_alt_1 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    noise_alt_2 <- noise_guassian_curve(number_of_curves = Sample_size, continuum_size = Conti_size)
    
    noise_alt_1 <- smoothed_gussian_curves(noise_alt_1, Noise_mu, Noise_sig, Noise_fwhm)
    noise_alt_2 <- smoothed_gussian_curves(noise_alt_2, Noise_mu, Noise_sig, Noise_fwhm)
    
    data_alt_1 <- data_generator(data = Baseline_data, noise = noise_alt_1)
    data_alt_2 <- data_generator(data = Baseline_data, signal = Signal, noise = noise_alt_2)
    
    tstat_2sample_null[i,] <- t_test(t(data_null_2), t(data_null_1), alternative = "greater",
                                     equal.var = FALSE)$t_statistic
    tstat_2sample_alt[i,] <- t_test(t(data_alt_2), t(data_alt_1), alternative = "greater",
                                    equal.var = FALSE)$t_statistic
  }
  
  
  # max_t_2sample_null<-apply((tstat_2sample_null), 1, function(x) { x[which.max( abs(x) )]})
  # max_t_2sample_alt<-apply((tstat_2sample_alt), 1, function(x) { x[which.max( abs(x) )]})
  max_t_2sample_null<-apply((tstat_2sample_null), 1, max)
  max_t_2sample_alt<-apply((tstat_2sample_alt), 1, max)
  
  threshold_2sample<-quantile(max_t_2sample_null,probs = (1-Alpha))
  
  power_2sample<-sum(max_t_2sample_alt>threshold_2sample)/Iter_number
  return(power_2sample)
  
}



t_test <- function(x, y, alternative, equal.var) {
  # Calculate the sample sizes and means
  n1 <- nrow(x)
  n2 <- nrow(y)
  mean_x <- colMeans(x)
  mean_y <- colMeans(y)
  
  # Calculate the sample variances
  var_x <- apply(x,2,var)
  var_y <- apply(y,2,var)
  
  
  if(equal.var){
    df <- n1 + n2 - 2
    # Calculate the pooled variance (assuming equal variances)
    pooled_var <- ((n1 - 1) * var_x + (n2 - 1) * var_y) / (n1 + n2 - 2)
    
    # Calculate the standard error
    se <- sqrt(pooled_var * (1/n1 + 1/n2))
    
    t_stat <- (mean_x - mean_y) / se
    
  } else if (!equal.var){
    # Calculate the degrees of freedom for Welch's t-test
    df <- ((var_x / n1 + var_y / n2)^2) / ((var_x^2 / (n1^2 * (n1 - 1))) +
                                             (var_y^2 / (n2^2 * (n2 - 1))))
    
    t_stat <- (mean_x - mean_y) / sqrt((var_x / n1) + (var_y / n2))
  }
  
  # Calculate the p-value
  
  if(alternative=="two_sided"){
    p_value <- 2 * pt(-abs(t_stat), df, lower.tail = FALSE)
  }
  else if (alternative == "greater") {
    p_value <- pt(t_stat, df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_value <- pt(t_stat, df, lower.tail = TRUE)
  }
  
  
  
  # Create a result list
  result <- list(
    t_statistic = t_stat,
    degrees_of_freedom = df,
    p_value = p_value
  )
  
  return(result)
}


#######Python codes##############

SmoothGaussian_noise <- function(sample_size, continuum_size, mu, sigma, fwhm){
  noise   <- power1d$noise$SmoothGaussian( J=as.integer(sample_size), 
                                           Q=as.integer(continuum_size), mu=mu,
                                           sigma=sigma, fwhm=fwhm, pad=FALSE)$value
  #dim(output)=sample_size*continuum_size
  return(noise)
}
GaussianPulse <- function(continuum_size, pulse_center, fwhm, amplitude){
  pulse <- power1d$geom$GaussianPulse( Q=as.integer(continuum_size), q=as.integer(pulse_center),
                              fwhm=fwhm, amp=amplitude )$value
  return(pulse)
}




  