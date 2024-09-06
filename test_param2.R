source("basic_functions.R")
pulse <- gaussian_pulse(center = 8,fwhm = 20,continuum_size = 101)
signal_value <-  amplitude_pulse(data = pulse$density_val, amp = 300)
power_spm_pointwise <- Power_calculator(Method="Parametric_SPM" ,
                                        Sample_size=8, Iter_number=10000,
                                        Baseline_data=vgrf_mean_data(type = "mean"),
                                        Signal=signal_value, Conti_size=101,
                                        Noise_mu=0, Noise_sig=250, Noise_fwhm=20, Alpha=0.5)

power_glob <- global_test_spm(Sample_size=8, Iter_number=10000,
                                        Baseline_data=vgrf_mean_data(type = "mean"),
                                        Signal=signal_value, Conti_size=101,
                                        Noise_mu=0, Noise_sig=250, Noise_fwhm=20, Alpha=0.5)


power_spm_pointwise <- Power_calculator(Method=c("TWT","Parametric_SPM") ,
                                        Sample_size=8, Iter_number=100,
                                        Baseline_data=vgrf_mean_data(type = "mean"),
                                        Signal=signal_value, Conti_size=101,
                                        Noise_mu=0, Noise_sig=250, Noise_fwhm=20, Alpha=0.5)
