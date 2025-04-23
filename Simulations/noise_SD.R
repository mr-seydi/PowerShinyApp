
#Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
#                Noise_sig = 20, Noise_fwhm = 45)
#Reapeat Noise for N time and then plot the pointwise mean and standard deviation of the noise



rm(list=ls())
source("basic_functions.R")

N <- 1000
SD_noise_45 <- c()
for (i in 1:N) {
  Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
                           Noise_sig = 20, Noise_fwhm = 45)$Mean
  SD_noise_45 <- rbind(SD_noise_45, Noise)
}



N <- 1000
SD_noise_5 <- c()
for (i in 1:N) {
  Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
                           Noise_sig = 20, Noise_fwhm = 5)$Mean
  SD_noise_5 <- rbind(SD_noise_5, Noise)
}

#plot the plots in one page
par(mfrow=c(2,1))
#plot pointwise boxplot of SD_noise
boxplot(SD_noise_45, col = "lightblue", main = "Pointwise Boxplot of Noise SD with FWHM=45", xlab = "", ylab = "SD", ylim=c(-20,20))

#plot pointwise boxplot of SD_noise
boxplot(SD_noise_5, col = "lightgreen", main = "Pointwise Boxplot of Noise SD with FWHM=5", xlab = "Pointwise", ylab = "SD", ylim=c(-20,20))


#plot the 6 plots in one page with 2 rows and 3 columns
# Set up a 3x2 plotting layout
par(mfrow = c(3, 2), oma = c(0, 4, 3, 0)) # Outer margins for titles

# Plotting boxplots with titles only on the first row
boxplot(SD_noise_45_1, col = "lightblue", main = "Noise SD with FWHM=45", xlab = "", ylab = "SD", ylim = c(0, 50))
boxplot(SD_noise_5_1, col = "lightgreen", main = "Noise SD with FWHM=5", xlab = "", ylab = "SD", ylim = c(0, 50))

boxplot(SD_noise_45_2, col = "lightblue", main = "", xlab = "", ylab = "SD", ylim = c(0, 50))
boxplot(SD_noise_5_2, col = "lightgreen", main = "", xlab = "", ylab = "SD", ylim = c(0, 50))

boxplot(SD_noise_45_3, col = "lightblue", main = "", xlab = "", ylab = "SD", ylim = c(0, 50))
boxplot(SD_noise_5_3, col = "lightgreen", main = "", xlab = "", ylab = "SD", ylim = c(0, 50))

# Adding outer titles for each column
mtext("Mechanism 3", side = 2, line = 2, outer = TRUE, at = 0.17, cex = 1.2)
mtext("Mechanism 2", side = 2, line = 2, outer = TRUE, at = 0.5, cex = 1.2)
mtext("Mechanism 1", side = 2, line = 2, outer = TRUE, at = 0.83, cex = 1.2)




#######################################################################################
rm(list=ls())

scipy.ndimage <- reticulate::import("scipy.ndimage")


sigma_to_fwhm <- function(sigma){
  return(sigma*(2 * sqrt(2 * log(2))))
}

fwhm_to_sigma <- function(fwhm){
  return(fwhm / (2 * sqrt(2 * log(2))))
}


ksmooth_gaussian <- function(x, fwhm) {
  
  SD = fwhm_to_sigma(fwhm)
  #smoothed_vals = ksmooth(x = 1:length(x), y = x, bandwidth = SD, kernel = "normal")$y
  smoothed_vals = scipy.ndimage$gaussian_filter1d(x, SD, mode='wrap')#$tolist()
}


#unsmoothed guassian noise curve
noise_guassian_curve <- function(number_of_curves, continuum_size){ 
  data <- matrix(rnorm(number_of_curves*continuum_size),
                 nrow = number_of_curves, ncol = continuum_size)
  return(data)
  
}


#smoothed gussian noise curves
smoothed_gussian_curves_1 <- function(data, mu, sig, fwhm){
  smoothed_data <- apply(data, 1, FUN = function(x) ksmooth_gaussian(x,fwhm))
  standard_smoothed_data=apply(smoothed_data, 2, scale)
  trans_data=apply(standard_smoothed_data, 2, FUN = function(x){(mu+x*sig)})
  return(trans_data) #dim(output)=continuum_size*number_of_curves
}


#apply this scaling factor to any smoothed data to ensure it has unit variance after smoothing
# from _set_scale function in https://github.com/0todd0000/power1d/blob/master/src/power1d/random.py#L35

set_scale <- function(nNodes, SD) {
  # Define a small epsilon to prevent division by zero
  eps <- .Machine$double.eps
  
  # Step 1: Define a Gaussian kernel
  t <- seq(-0.5 * (nNodes - 1), 0.5 * (nNodes - 1), length.out = nNodes)
  gf <- exp(-(t^2) / (2 * SD^2 + eps))
  
  # Step 2: Normalize the Gaussian kernel
  gf <- gf / sum(gf)
  
  # Step 3: Calculate the expected variance of the smoothed data
  # Perform FFT and compute power spectrum
  AG <- fft(gf)
  Pag <- Mod(AG)^2  # Equivalent to AG * Conj(AG)
  
  # Calculate the autocovariance by inverse FFT
  COV <- Re(fft(Pag, inverse = TRUE)) / length(Pag)
  svar <- COV[1]  # Variance of the smoothed field
  
  # Step 4: Calculate the scaling factor
  SCALE <- sqrt(1.0 / svar)
  
  return(SCALE)
}

smoothed_gussian_curves_2 <- function(data, mu, sig, fwhm) {
  # Step 1: Smooth each curve in the data
  smoothed_data <- apply(data, 1, function(curve) {
    smoothed_curve <- ksmooth_gaussian(curve, fwhm)
    return(smoothed_curve)
  })
  
  # Step 2: Normalize the smoothed data to have unit variance
  nNodes <- ncol(smoothed_data)
  SD <- fwhm_to_sigma(fwhm)
  scale_factor <- set_scale(nNodes, SD)
  
  # Step 3: Scale the smoothed data
  smoothed_data_scaled <- smoothed_data * scale_factor
  
  # Step 4: Transform to have mean = mu and standard deviation = sig
  smoothed_data_final <- (smoothed_data_scaled - mean(smoothed_data_scaled)) / sd(smoothed_data_scaled) * sig + mu
  
  
  return(smoothed_data_final)
}

power1d <- reticulate::import("power1d")
##############################
noise_data <- noise_guassian_curve(continuum_size = 101, number_of_curves = 1)

out=smoothed_gussian_curves(data = noise_data, mu = 0, sig = 20, fwhm = 90)
apply(out, 2, sd)
out=t(power1d$noise$SmoothGaussian( as.integer(5) ,as.integer(101) ,as.integer(0) , as.integer(20) ,fwhm = as.integer(90))$value)


sgc_1_fwhm5 <- smoothed_gussian_curves_1(data = noise_data, mu = 0, sig = 20, fwhm = 5)
#sgc_2_fwhm5 <- smoothed_gussian_curves_2(data = noise_data, mu = 0, sig = 20, fwhm = 5)
sgc_2_fwhm5 <- t(power1d$noise$SmoothGaussian( as.integer(5) ,as.integer(101) ,as.integer(0) , as.integer(20) ,fwhm = as.integer(5))$value)
sgc_1_fwhm45 <- smoothed_gussian_curves_1(data = noise_data, mu = 0, sig = 20, fwhm = 45)
#sgc_2_fwhm45 <- smoothed_gussian_curves_2(data = noise_data, mu = 0, sig = 20, fwhm = 45)
sgc_2_fwhm45 <- t(power1d$noise$SmoothGaussian( as.integer(5) ,as.integer(101) ,as.integer(0) , as.integer(20) ,fwhm = as.integer(45))$value)
sgc_1_fwhm90 <- smoothed_gussian_curves_1(data = noise_data, mu = 0, sig = 20, fwhm = 90)
#sgc_2_fwhm90 <- smoothed_gussian_curves_2(data = noise_data, mu = 0, sig = 20, fwhm = 90)
sgc_2_fwhm90 <- t(power1d$noise$SmoothGaussian( as.integer(5) ,as.integer(101) ,as.integer(0) , as.integer(20) ,fwhm = as.integer(90))$value)
#############################
# Set up the plot layout with space for outer margins
par(mfrow = c(3, 2), oma = c(0, 4, 3, 0))  # oma=c(bottom, left, top, right)

# Plot each of the six plots
matplot(sgc_1_fwhm5, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
matplot(sgc_2_fwhm5, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
mtext("FWHM = 5", side = 2, line = 2, outer = TRUE, at = 0.85, cex = 1.2)  # Row label for first row

matplot(sgc_1_fwhm45, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
matplot(sgc_2_fwhm45, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
mtext("FWHM = 45", side = 2, line = 2, outer = TRUE, at = 0.5, cex = 1.2)  # Row label for second row

matplot(sgc_1_fwhm90, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
matplot(sgc_2_fwhm90, type = "l", col = 3:7, ylim = c(-80, 80), lty = 1, lwd = 2, xlab = "")
mtext("FWHM = 90", side = 2, line = 2, outer = TRUE, at = 0.15, cex = 1.2)  # Row label for third row

# Add column labels at the top
mtext("Mechanism 1", side = 3, line = 1, outer = TRUE, at = 0.25, cex = 1.2)
mtext("Mechanism 2", side = 3, line = 1, outer = TRUE, at = 0.75, cex = 1.2)

####################################
rm(list=ls())
source("basic_functions.R")
rft1d <- reticulate::import("rft1d")
power1d <- reticulate::import("power1d")
W=c()
for (i in 1:10000) {
  noise_data <- noise_guassian_curve(continuum_size = 101, number_of_curves = 10)
  y = power1d$noise$SmoothGaussian( as.integer(1) ,as.integer(101) ,as.integer(0) , as.integer(20) ,fwhm = as.integer(90))$value
  y_py = reticulate::r_to_py(y)
  w = rft1d$geom$estimate_fwhm(y_py)
  W=c(W,w)
}
#boxplot of W
boxplot(W, col = "lightblue", main = "Boxplot of FWHM", xlab = "", ylab = "FWHM")



##########

