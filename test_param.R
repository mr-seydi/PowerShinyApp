Method="Parametric_SPM"
Sample_size=15
Iter_number=100
Baseline_data=vgrf_mean_data("mean")
Signal=amplitude_pulse(data = gaussian_pulse(center = 10,fwhm = 20,continuum_size = 101)$density_val,amp = 450)
Conti_size=101
Noise_mu=0
Noise_sig=200
Noise_fwhm=20
Alpha=0.05
