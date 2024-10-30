
#Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
#                Noise_sig = 20, Noise_fwhm = 45)
#Reapeat Noise for N time and then plot the pointwise mean and standard deviation of the noise
N <- 1000
SD_noise_45 <- c()
for (i in 1:N) {
  Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
                           Noise_sig = 20, Noise_fwhm = 45)$SD
  SD_noise_45 <- rbind(SD_noise_45, Noise)
}



N <- 1000
SD_noise_5 <- c()
for (i in 1:N) {
  Noise <- Noise_generator(Sample_size = 10,  Continuum_size = 101, Noise_mu = 0,
                           Noise_sig = 20, Noise_fwhm = 5)$SD
  SD_noise_5 <- rbind(SD_noise_5, Noise)
}

#plot the plots in one page
par(mfrow=c(2,1))
#plot pointwise boxplot of SD_noise
boxplot(SD_noise_45, col = "lightblue", main = "Pointwise Boxplot of Noise SD with FWHM=45", xlab = "", ylab = "SD", ylim=c(0,50))

#plot pointwise boxplot of SD_noise
boxplot(SD_noise_5, col = "lightgreen", main = "Pointwise Boxplot of Noise SD with FWHM=5", xlab = "Pointwise", ylab = "SD", ylim=c(0,50))




