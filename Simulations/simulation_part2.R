rm(list=ls())
source("Simulations/Parallel.R")
#"Parametric_SPM","TWT","IWT","Nonparametric_SPM"


Power_parallel(data = MF_data("both"),
               sample_size = 13,
               noise_mean = 0,
               noise_sd = 50,
               noise_fwhm = 45,
               signal = NULL,
               method = c("TWT"),
               n_iterations = 1000,
               Write_file = TRUE,
               file_name = "/Users/more0056/Desktop/temp_file/Power_MF_TWT_2.xlsx"
)


