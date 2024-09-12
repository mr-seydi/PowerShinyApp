#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




source("Data_functions.R")
library(ggplot2)

cont_size <- 101 #continuum_size (must match the dimension of baseline data)


function(input, output, session) {
  
  # Reactive value to store the selected type input for baseline type
  type_input_value <- reactiveVal(NULL)
  
  selected_data <- reactive({
    
    req(input$data_selection_type)  # Ensure data_selection_type is available before proceeding
    
    ####Data selection####
    
    # If 'baseline' is selected, use baseline datasets
    if (input$data_selection_type == "two_sample") {
      ######------Data selection, two sample data------####
      # Reactive expression to handle two-sample data selection
      req(input$dataset_two_sample)  # Ensure dataset input is available before proceeding
      
      switch(input$dataset_two_sample,
             "vGRF_both" = vGRF_data_Phan(type = "both"),
             "JCF_both" = JCF_data(type = "both"),
             "Hip_Angle_both" = Angle_data(type = "both"),
             "Moment_both" = Moment_data(type = "both"),
             "MF_both" = MF_data(type = "both"),
             "EMG_both" = EMG_data(type = "both")
      )

  
    } else {
      
      ######------Data selection, baseline data------####
      req(input$dataset_baseline)  # Ensure dataset input is available before proceeding
      
      # Debugging lines
      print(paste("Selected dataset:", input$dataset_baseline))
      print(paste("Selected type:", type_input_value()))
      
      # Switch based on dataset selected
      switch(input$dataset_baseline,
             "vGRF_Robinson" = vGRF_data_Robinson(type = type_input_value()),
             "vGRF_Phan" = vGRF_data_Phan(type = type_input_value()),
             "JCF" = {
               print(paste("Calling JCF_data with type:", type_input_value()))  # Debugging line
               JCF_data(type = type_input_value())
             },
             "Hip_Angle" = Angle_data(type = type_input_value()),
             "Moment" = Moment_data(type = type_input_value()),
             "MF" = MF_data(type = type_input_value()),
             "EMG" = EMG_data(type = type_input_value())
      )
      
    }
    
  })
  

  

  # Create a reactive UI for the 'type' selection based on dataset choice
  output$type_selector <- renderUI({
    switch(input$dataset_baseline,
           "vGRF_Robinson" = selectInput("type_input", "Choose type:",
                                         choices = c("mean")),
           "vGRF_Phan" = selectInput("type_input", "Choose type:",
                                     choices = c("quiet", "normal")),
           "JCF" = selectInput("type_input", "Choose type:",
                               choices = c("lateral_wedge", "no_wedge")),
           "Hip_Angle" = selectInput("type_input", "Choose type:",
                                     choices = c("individual1", "individual2")),
           "Moment" = selectInput("type_input", "Choose type:",
                                  choices = c("DK", "IK")),
           "MF" = selectInput("type_input", "Choose type:",
                              choices = c("control", "diabetic")),
           "EMG" = selectInput("type_input", "Choose type:",
                               choices = c("adult", "young"))
    )
  })
  
  
  # Observe changes in dataset input and reset type_input
  observeEvent(input$dataset_baseline, {
    # Reset the type_input_value when dataset changes
    type_input_value(NULL)
  })
  
  # Update the reactive value whenever the type_input changes
  observeEvent(input$type_input, {
    type_input_value(input$type_input)
  })
    
  
  

  
  
  # Helper function for numeric input validation
  validate_input <- function(input_value, min_value, default_value, error_message) {
    if (!is.numeric(input_value) || input_value < min_value) {
      return(list(valid = FALSE, value = default_value, message = error_message))
    }
    return(list(valid = TRUE, value = input_value))
  }
  
  # Input validation observers
  observeEvent(input$sample_size, {
    validation <- validate_input(input$sample_size, 1, 5, "Sample size must be a positive integer. Value set to 5.")
    if (!validation$valid) {
      updateNumericInput(session, "sample_size", value = validation$value)
      output$error_message_noise <- renderText(validation$message)
    }
  })
  
  observeEvent(input$sigma, {
    validation <- validate_input(input$sigma, 0, 0, "Standard Deviation cannot be negative. Value set to 0.")
    if (!validation$valid) {
      updateNumericInput(session, "sigma", value = validation$value)
      output$error_message_noise <- renderText(validation$message)
    }
  })
  
  
  observeEvent(input$fwhm, {
    validation <- validate_input(input$fwhm, 1, 20, "FWHM must be positive. Value set to 20.")
    if (!validation$valid) {
      updateNumericInput(session, "fwhm", value = validation$value)
      output$error_message_noise <- renderText(validation$message)
    }
  })
  
  
  # Reactive expressions
  data_noise <- reactive({
    req(input$sample_size)
    noise_guassian_curve(number_of_curves = input$sample_size, continuum_size = cont_size)
  })
  
  smoothed_data <- reactive({
    req(input$mu, input$sigma, input$fwhm)
    smoothed_gussian_curves(data = data_noise(), mu = input$mu, sig = input$sigma, fwhm = input$fwhm)
  })
  
  # Output plot rendering
  output$noise_plot <- renderPlot({
    validate(
      need(input$mu, "Mu is required"),
      need(input$sigma, "Sigma is required"),
      need(input$fwhm, "FWHM is required")
    )
    
    # Create a data frame for ggplot
    plot_data <- data.frame(
      x_values = rep(0:(cont_size - 1), ncol(smoothed_data())),
      y_values = as.vector(smoothed_data()),  # Flatten the matrix
      line_group = factor(rep(1:ncol(smoothed_data()), each = cont_size))  # Create a group for each column
    )
    
    # Generate the plot using ggplot2
    ggplot(plot_data, aes(x = x_values, y = y_values, group = line_group, color = line_group)) +
      geom_line(size = 1.5) +  # Set line thickness
      scale_color_manual(values = colorRampPalette(c("darkblue", "lightblue"))(ncol(smoothed_data()))) +  # Navy blue shades
      labs(title = "Smooth Gaussian Noise", x = "Index", y = "Value") +  # Add labels
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
      theme(legend.position = "none")
      
    
  })
  
  
  
  # Reactive expression for generating Gaussian pulse data
  data_pulse <- reactive({
    req(input$data_selection_type == "baseline")  # Ensure "baseline" is selected
    req(input$center, input$fwhm_pulse)
    gaussian_pulse(center = input$center, fwhm = input$fwhm_pulse, continuum_size = cont_size)
  })
  
  
  
  
  # Reactive expression for computing the half-max value of selected data
  default_amplitude <- reactive({
    
    req(input$data_selection_type == "baseline")  # Ensure "baseline" is selected
    req(selected_data())  # Ensure selected_data() is available
    
    # Compute the half-max value
    half_max <- ( selected_data()[which.max(abs(selected_data()))] ) / 2 
    # To return the half-max value  with its original sign
    
    return(round( half_max,digits = 1))
    
  })
  
  # Observe changes to set the default value of amplitude input
  observe({
    
    req(input$data_selection_type == "baseline")  # Ensure "baseline" is selected
    req(default_amplitude())  # Ensure default_amplitude() is available
    
    updateNumericInput(session, "amplitude", value = default_amplitude())
  })
  
  
  # Reactive expression for scaling the pulse
  scaled_pulse <- reactive({
    req(input$data_selection_type == "baseline")  # Ensure "baseline" is selected
    req(input$amplitude, data_pulse())
    amplitude_pulse(data = data_pulse()$density_val, amp = input$amplitude)
  })
  
  
  
  
  
  output$pulse_plot <- renderPlot({
    
    
    
    # Create a data frame with all necessary values, including a 'Legend' variable
    
    if (input$data_selection_type == "baseline") {
      
      req(scaled_pulse(), data_pulse())  # Ensure both are available
      
      plot_data <- data.frame(
        x_values = rep(data_pulse()$x_values, 3),  # Repeat x_values for 3 lines
        y_values = c(scaled_pulse(), selected_data(), selected_data() +
                       scaled_pulse()),  # Combine all y-values
        legend = factor(rep(c("Pulse", "Baseline data", "Baseline data + Pulse"), 
                            each = length(data_pulse()$x_values)),
                        levels = c("Pulse", "Baseline data",
                                   "Baseline data + Pulse"))  # Control factor levels
      )
    } else {
      
      req(selected_data())  # Ensure selected_data is available for two_sample
      
      # Create a data frame with the two-sample data
      plot_data <- data.frame(
        x_values = rep(0:(cont_size-1), 3),  # Repeat x_values for 3 lines
        y_values = c(abs(selected_data()[,2]-selected_data()[,1]),
                     selected_data()[,1], selected_data()[,2]),  # Combine all y-values
        legend = factor(rep(c("Pulse", colnames(selected_data())[1],
                              colnames(selected_data())[2]), 
                            each = dim(selected_data())[1]),
                        levels = c("Pulse", colnames(selected_data())[1],
                                   colnames(selected_data())[2]))  # Control factor levels
      )
    }
    
    
    # Create the plot using ggplot
    ggplot(plot_data, aes(x = x_values, y = y_values, color = legend)) +
      geom_line(size = 1.5) +  # Plot lines for each group
      labs(title = "Selected Data", x = "Index", y = "Value") +  # Labels
      scale_color_manual(values = c("black", "tomato", "cadetblue")) +  # Line colors
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
      theme(legend.position = "bottom",legend.title = element_blank())
  })
  
  
  
  
  
  
  
  # Render the data plot with generated sample data
  output$data_plot <- renderPlot({
    # Generate data with and without pulse
    data_with_pulse <- data_generator(data = selected_data(), signal = scaled_pulse(), noise = smoothed_data())
    data_without_pulse <- data_generator(data = selected_data(), noise = smoothed_data())
    
    # Prepare data for ggplot (assuming both datasets have the same number of rows/columns)
    
    # Calculate mean for each group
    mean_with_pulse <- rowMeans(data_with_pulse)
    mean_without_pulse <- rowMeans(data_without_pulse)
    
    # Create a long format data frame for ggplot
    plot_data <- data.frame(
      x_values = rep(0:(cont_size-1), ncol(data_with_pulse) * 2),  # Repeat index for each column and dataset
      y_values = c(as.vector(data_with_pulse), as.vector(data_without_pulse)),  # Flatten both datasets
      label = factor(rep(c("With Pulse", "Without Pulse"), each = cont_size * ncol(data_with_pulse)), levels = c("With Pulse", "Without Pulse")),  # Labels
      line_group = factor(rep(1:ncol(data_with_pulse), each = cont_size, times = 2))  # Line group for each column
    )
    
    # Create a separate data frame for the mean lines
    mean_data <- data.frame(
      x_values = rep(0:(cont_size-1), 2),
      y_values = c(mean_with_pulse, mean_without_pulse),
      label = factor(c(rep("With Pulse", cont_size), rep("Without Pulse", cont_size)), levels = c("With Pulse", "Without Pulse"))
    )
    
    # Create the plot using ggplot2
    ggplot() +
      # First layer: Individual lines
      geom_line(data = plot_data, aes(x = x_values, y = y_values, group = interaction(line_group, label), color = label), size = 1, alpha=0.4) +  
      # Second layer: Mean lines without group aesthetic
      geom_line(data = mean_data, aes(x = x_values, y = y_values, color = label), size = 2.5) +  
      scale_color_manual(values = c("cadetblue", "tomato")) +  # Set colors for "With Pulse" and "Without Pulse"
      labs(title = "One illustration of sample data", x = "Index", y = "Value") +  # Add labels
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
      theme(legend.position = "bottom", legend.title = element_blank())  # Move legend to bottom
  })
  
  
  
  
  
  # Power calculation triggered by the "Calculate Power" button
  power <- eventReactive(input$calculate, {
    isolate({
      Power_calculator(
        Method = input$test_type,
        Sample_size = input$sample_size,
        Iter_number = 100,
        Baseline_data = selected_data(),
        Signal = scaled_pulse(),
        Conti_size = cont_size,
        Noise_mu = input$mu,
        Noise_sig = input$sigma,
        Noise_fwhm = input$fwhm,
        Alpha = 0.05
      )
    })
  })
  
  # Render the power output as text
  output$powerOutput <- renderText({
    methods <- names(power())
    result <- ""
    for (m in methods) {
      result <- paste(result, "Power of", m, ":", round(power()[[m]], 2))
    }
    result
  })
  
}
  