#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



source("basic_functions.R")
library(ggplot2)
cont_size <- 101 #continuum_size (must match the dimension of baseline data)
base_data <- vgrf_mean_data(type = "mean")
function(input, output, session) {
  

  
  
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
    req(input$center, input$fwhm_pulse)
    gaussian_pulse(center = input$center, fwhm = input$fwhm_pulse, continuum_size = cont_size)
  })
  
  # Reactive expression for scaling the pulse
  scaled_pulse <- reactive({
    req(input$amplitude)
    amplitude_pulse(data = data_pulse()$density_val, amp = input$amplitude)
  })
  

  
  output$pulse_plot <- renderPlot({
    
    
    # Create a data frame with all necessary values, including a 'Legend' variable
    plot_data <- data.frame(
      x_values = rep(data_pulse()$x_values, 3),  # Repeat x_values for 3 lines
      y_values = c(scaled_pulse(), base_data, base_data + scaled_pulse()),  # Combine all y-values
      legend = factor(rep(c("Pulse", "Baseline data", "Baseline data + Pulse"), 
                          each = length(data_pulse()$x_values)),
                      levels = c("Pulse", "Baseline data", "Baseline data + Pulse"))  # Control factor levels
    )
    
    # Create the plot using ggplot
    ggplot(plot_data, aes(x = x_values, y = y_values, color = legend)) +
      geom_line(size = 1.5) +  # Plot lines for each group
      labs(title = "Gaussian Pulse", x = "Index", y = "Value") +  # Labels
      scale_color_manual(values = c("black", "tomato", "cadetblue")) +  # Line colors
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
      theme(legend.position = "bottom",legend.title = element_blank())
  })
  
  
  
  
  # Render the data plot with generated sample data
  output$data_plot <- renderPlot({
    # Generate data with and without pulse
    data_with_pulse <- data_generator(data = base_data, signal = scaled_pulse(), noise = smoothed_data())
    data_without_pulse <- data_generator(data = base_data, noise = smoothed_data())
    
    # Prepare data for ggplot (assuming both datasets have the same number of rows/columns)
    
    # Calculate mean for each group
    mean_with_pulse <- rowMeans(data_with_pulse)
    mean_without_pulse <- rowMeans(data_without_pulse)
    
    # Create a long format data frame for ggplot
    plot_data <- data.frame(
      x_values = rep(1:cont_size, ncol(data_with_pulse) * 2),  # Repeat index for each column and dataset
      y_values = c(as.vector(data_with_pulse), as.vector(data_without_pulse)),  # Flatten both datasets
      label = factor(rep(c("With Pulse", "Without Pulse"), each = cont_size * ncol(data_with_pulse)), levels = c("With Pulse", "Without Pulse")),  # Labels
      line_group = factor(rep(1:ncol(data_with_pulse), each = cont_size, times = 2))  # Line group for each column
    )
    
    # Create a separate data frame for the mean lines
    mean_data <- data.frame(
      x_values = rep(1:cont_size, 2),
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
        Baseline_data = vgrf_mean_data(type = "mean"),
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
  