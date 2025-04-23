rm(list=ls())
source("Simulations/Parallel.R")
#"Parametric_SPM","TWT","IWT","Nonparametric_SPM"


Power_parallel(data = Angle_data("both"),
               sample_size = 27,
               noise_mean = 0,
               noise_sd = 15,
               noise_fwhm = 5,
               signal = NULL,
               method = c("Parametric_SPM"),
               n_iterations = 1000,
               Write_file = FALSE,
               file_name = "/Users/more0056/Desktop/temp_file/Power_Results_Angle_1ss5_TWT.xlsx"
)

