#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



source("basic_functions.R")



# Define server logic required to draw a histogram
function(input, output, session) {
  
  cont_size <- 101 #continuum_size (it must be equal to dimension of baseline data)
  
  
  #CHECK Possible inputs errors
  #############################################################################
  observeEvent(input$sample_size, {
    req(input$sample_size)
    if (!is.integer(input$sample_size) | (input$sample_size)<1) {
      updateNumericInput(session, "sample_size", value = 5)
      output$error_message_noise <- renderText({
        "Sample size must be a positive integer. Value set to 5."
      })
    }
  })
  
  observeEvent(input$sigma, {
    req(input$sigma)
    if (input$sigma < 0) {
      updateNumericInput(session, "sigma", value = 0)
      output$error_message_noise <- renderText({
        "Standard Deviation cannot be negative. Value set to 0."
      })
    }
  })
  
  observeEvent(input$fwhm, {
    req(input$fwhm)
    #if (input$fwhm <= 0|  input$fwhm_pulse > 100) {
    if (input$fwhm <= 0) {
      updateNumericInput(session, "fwhm", value = 1)
      output$error_message_noise <- renderText({
        "FWHM must be positive. Value set to 1."
      })
    }
  })
  
  observeEvent(input$fwhm_pulse, {
    req(input$fwhm_pulse)
    #if (input$fwhm_pulse <= 0 | input$fwhm_pulse > 100) {
    if (input$fwhm_pulse <= 0) {
      updateNumericInput(session, "fwhm_pulse", value = 1)
      output$error_message_pulse <- renderText({
        "FWHM must be positive. Value set to 1."
      })
    }
  })
  
  
  ############################################################################  
  
  
  # noise raw data, reactive because smoothness (fwhm) apply into the same data
  data_noise <- reactive({
    
    req(input$sample_size) #to not having error when box is blank
    
    noise_guassian_curve(number_of_curves = input$sample_size,
                         continuum_size = cont_size)
  })
  
  
  # Define smoothed_data as a reactive expression
  smoothed_data <- reactive({
    req(input$mu, input$sigma, input$fwhm)
    smoothed_gussian_curves(data = data_noise(), mu = input$mu, sig = input$sigma, fwhm = input$fwhm)
  })
  
  output$noise_plot <- renderPlot({
    
    # # req() does not work here properly
    # if (is.na(input$mu) ||
    #     is.na(input$sigma) ||
    #     is.na(input$fwhm)) {
    #   return(NULL)
    # } #to not having error when box is blank 
    # 
    # #do smoothness for noise
    # smoothed_data <- smoothed_gussian_curves(data = data_noise(), mu = input$mu,
    #                                          sig = input$sigma ,fwhm = input$fwhm)
    #plot noise smoothed curves
    matplot(x = 0:(cont_size-1), y = smoothed_data(), type = "l",
            main = "Smooth Gaussian Noise",
            xlab = "Index", ylab = "Value",lwd = 3,lty = 1)
  })
  
  
  ###############################################################################
  
  # 
  data_pulse <- reactive({
    
    req(input$center, input$fwhm_pulse) #to not having error when box is blank
    
    gaussian_pulse(center = input$center, fwhm = input$fwhm_pulse,
                   continuum_size = 101)
  })
  
  scaled_pulse <- reactive({
    req(input$amplitude)
    amplitude_pulse(data = data_pulse()$density_val, amp = input$amplitude)
  })
  
  
  output$pulse_plot <- renderPlot({
    
    # if (is.na(input$amplitude)) {
    #   return(NULL)
    # } #to not having error when box is blank
    # 
    # scaled_pulse <- amplitude_pulse(data = data_pulse()$density_val, amp = input$amplitude)
    # Plot the Gaussian pulse
    base_data <- vgrf_mean_data(type = "mean")
    plot(x = data_pulse()$x_values, y = scaled_pulse(), type = "l", main = "Gaussian Pulse",
         xlab = "Index", ylab = "Value", lwd = 3, lty = 1,
         ylim=c(0,max(base_data,base_data+scaled_pulse())))
    lines(0:(cont_size-1), base_data, col="red", lwd = 3, lty = 1)
    lines(0:(cont_size-1), base_data+scaled_pulse(),col="blue", lwd = 3, lty = 1)
    legend("topright", 
           legend = c("Pulse","Baseline data", "Baseline data + Pulse"), 
           col = c("black" ,"red", "blue"), 
           lwd = 3, 
           lty = 1)
  })
  
  ###############################################################################
  
  
  output$data_plot <- renderPlot({
    loop_data <- data_generator(data = vgrf_mean_data(type = "mean"), signal = scaled_pulse(), noise = smoothed_data())
    matplot( y = loop_data, type = "l", main = "One illustration of sample data",
             xlab = "Index", ylab = "Value", lwd = 3, lty = 1)
  })
  
  
  
  
  
  ################################################################################
  
  # We've replaced reactive({ ... }) with eventReactive(input$calculate, { ... }).
  # This ensures that the power calculation is only triggered when
  # the "Calculate Power" button (input$calculate) is clicked.
  # We've kept isolate({ ... }) to ensure that the power calculation 
  # is isolated from other inputs, preventing it from being recalculated when other parameters change.
  power <- eventReactive(input$calculate, {
    isolate({
      method_type <- input$test_type
      Power_calculator(Method = method_type, Sample_size = input$sample_size,
                       Iter_number = 100, Baseline_data = vgrf_mean_data(type = "mean"),
                       Signal = scaled_pulse(), Conti_size = 101,Noise_mu = input$mu,
                       Noise_sig = input$sigma, Noise_fwhm = input$fwhm, Alpha = 0.05)
    })
  })
  
  # Render the power output
  # output$powerOutput <- renderText({
  #   paste("Power:", round(power(), 2))
  # })
  
  output$powerOutput <- renderText({
    methods <- names(power())
    result <- ""
    for (m in methods) {
      result <- paste(result, "Power of ", m, ":", round(power()[[m]], 2))
    }
    result
  })
  
  
  
  
  
  
  
  
  ####################################  
}



