rm(list=ls())
source("Simulations/Parallel.R")
#"Parametric_SPM", "TWT", "IWT", "Nonparametric_SPM", "ERL", "IATSE"


Power_parallel(data = MF_data("both"),
               sample_size = 12,
               noise_mean = 0,
               noise_sd = 40,
               noise_fwhm = 15,
               signal = NULL,
               method = c( "ERL", "IATSE"),
               n_iterations = 1000,
               Write_file = FALSE,
               file_name = "/Users/more0056/Desktop/temp_file/Power_MF_TWT_2.xlsx"
)


