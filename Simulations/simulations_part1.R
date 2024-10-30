rm(list=ls())
source("Simulations/Parallel.R")
#"Parametric_SPM","TWT","IWT","Nonparametric_SPM"


Power_parallel(data = Angle_data("both"),
               sample_size = 8,
               noise_mean = 0,
               noise_sd = 15,
               noise_fwhm = 5,
               signal = NULL,
               method = c("TWT"),
               n_iterations = 1000,
               Write_file = TRUE,
               file_name = "/Users/more0056/Desktop/temp_file/Power_Results_Angle_1ss5_TWT.xlsx"
)



