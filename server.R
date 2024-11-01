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
library(future)
library(promises)
library(bslib)
library(dplyr)
future::plan(multisession)


cont_size <- 101 #continuum_size (must match the dimension of baseline data)
# Status File
status_file <- "status.txt"

function(input, output, session) {
  
  # Reactive value to store the selected type input for baseline type
  type_input_value <- reactiveVal(NULL)
  
  # Reactive value to store submitted interpolated data
  submitted_data <- reactiveVal(NULL)
  
  selected_data <- reactive({
    
    req(input$data_selection_type)  # Ensure data_selection_type is available before proceeding
    
    ####Data selection####
    # If 'custom_curve' is selected, use the smoothed curve data from the first app
    if (input$data_selection_type == "custom_curve") {
      req(submitted_data())  # Ensure submitted_data() is available before proceeding
      return(as.vector(submitted_data()))
      
    } else if (input$data_selection_type == "two_sample") { # If 'baseline' is selected, use baseline datasets
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
           "vGRF_Robinson" = selectInput("type_input", h4("Choose type:"),
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
  
  
  
  
  ########-------Custom Curve-------########
  # Store additional points clicked by the user
  user_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  
  # Track if smoothing should be applied
  smoothing <- reactiveVal(FALSE)
  
  # Store 101 evaluated points (including 0 and 100) after smoothing
  smoothed_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  
  # Reactive expression for filtered plot data
  plot_data <- reactive({
    user_points() %>%
      dplyr::filter(x >= 0 & x <= 100 & y >= input$ymin  & y <= input$ymax)
  })
  
  # Reactive expression for filtered smoothed data
  smoothed_data_custom_curve <- reactive({
    if (input$show_smooth && smoothing() && nrow(smoothed_points()) == 101) {
      smoothed_points() %>%
        dplyr::filter(x >= 0 & x <= 100 & y >= input$ymin & y <= input$ymax)
    } else {
      NULL
    }
  })
  
  # Reactive expression for interpolated smoothed data
  interpolated_smoothed_data <- reactive({
    req(smoothed_data_custom_curve())
    if (is.null(smoothed_data_custom_curve())) {
      return(NULL)
    }
    V <- smoothed_data_custom_curve()$y
    x <- smoothed_data_custom_curve()$x
    x_new <- seq(0, 100, length.out = 101)
    approx(x, V, xout = x_new, na.rm = TRUE, rule = 2)$y
  })
  
  # Add new points when the user clicks on the plot
  observeEvent(input$plot_click, {
    new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
    
    # Ensure x is within the allowed range (between 0 and 100)
    if (new_point$x >= 0 && new_point$x <= 100) {
      user_points(rbind(user_points(), new_point))
      smoothing(FALSE) # Reset smoothing when a new point is added
    }
  })
  
  # Reset points when the reset button is clicked
  observeEvent(input$reset, {
    user_points(data.frame(x = numeric(), y = numeric()))
    smoothed_points(data.frame(x = numeric(), y = numeric()))
    smoothing(FALSE) # Reset smoothing
  })
  
  # Undo last point when the undo button is clicked
  observeEvent(input$undo, {
    current_points <- user_points()
    if (nrow(current_points) > 0) {
      user_points(current_points[-nrow(current_points), ])
    }
    smoothing(FALSE) # Reset smoothing
  })
  
  # Observe user points and apply smoothing if necessary
  observe({
    plot_data <- user_points()
    if (nrow(plot_data) > 3) {
      # Fit a LOESS model to the points, using the input smoothness span value
      tryCatch({
        loess_fit <- loess(y ~ x, data = plot_data, span = input$smoothness)
        
        # Generate 101 equally spaced points between 0 and 100
        x_seq <- seq(0, 100, length.out = 101)
        
        # Predict the corresponding y values from the LOESS model
        y_pred <- predict(loess_fit, newdata = data.frame(x = x_seq))
        
        # Store the evaluated points
        smoothed_points(data.frame(x = x_seq, y = y_pred))
        smoothing(TRUE) # Set smoothing to TRUE to display the smoothed curve
      }, error = function(e) {
        # If LOESS fitting fails, reset smoothing points
        smoothed_points(data.frame(x = numeric(), y = numeric()))
        smoothing(FALSE)
        showNotification("LOESS model failed. Try increasing the smoothing value.", type = "error")
      })
    } else {
      smoothed_points(data.frame(x = numeric(), y = numeric())) # No smoothing if not enough points
      smoothing(FALSE) # Reset smoothing
    }
  })
  
  # Render the plot with the drawn points and connecting lines
  output$plot <- renderPlot({
    p <- ggplot(plot_data(), aes(x = x, y = y)) +
      geom_point(color = "blue", size = 3) +
      xlim(0, 100) + ylim(input$ymin, input$ymax) +
      labs(x = "X-axis", y = "Y-axis", title = "Click to Draw a Curve") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
    
    # Add connecting lines if there are user points
    if (nrow(plot_data()) > 0) {
      p <- p + geom_line(data = plot_data(), color = "red")
    }
    
    # Add smoothing if checkbox is checked and smoothing is TRUE
    if (!is.null(smoothed_data_custom_curve())) {
      V <- smoothed_data_custom_curve()$y
      x <- scale_minmax(smoothed_data_custom_curve()$x, a = 0, b = 100)
      x_new <- seq(0, 100, length.out = 101)
      smoothed_data_interpolated <- approx(x, V, xout = x_new, na.rm = TRUE, rule = 2)$y
      submitted_data(smoothed_data_interpolated)
      smoothed_data_interpolated <- cbind(y = smoothed_data_interpolated, x = x_new)
      p <- p + geom_line(data = smoothed_data_interpolated, aes(x = x, y = y), color = "green", linewidth = 1)
    }
    
    # Return the plot
    p
  })
  
  
  
  
  
  
  ########------End of the Custom Curve------########
  
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
  
  # Reactive expressions
  data_noise_2 <- reactive({
    req(input$sample_size)
    noise_guassian_curve(number_of_curves = input$sample_size, continuum_size = cont_size)
  })
  
  
  # Reactive expression for computing the half-range value for noise SD 
  default_sigma <- reactive({
    if (is.null(selected_data())) {
      return(0)  # Return a default value if data is empty
    } else if (input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve") {
      # Compute the half-range value
      half_range <- ( max(selected_data(), na.rm = T)-
                      min(selected_data(), na.rm = T)) / 2 
    } else {
        # Compute the half-range value
        Max1 <- max(selected_data()[,1], na.rm = T)
        Max2 <- max(selected_data()[,2], na.rm = T)
        Min1 <- min(selected_data()[,1], na.rm = T)
        Min2 <- min(selected_data()[,2], na.rm = T)
        
      half_range <- ( max(c(Max1,Max2), na.rm = T)-min(c(Min1,Min2), na.rm = T) ) / 2
    }

    # To return the half-range
    
    return(round( half_range,digits = 1))
    
  })
  
  # Observe changes to set the default value of noise SD
  observe({
    
    req(default_sigma())  # Ensure default_amplitude() is available
    
    updateNumericInput(session, "sigma", value = default_sigma())
  })
  
  smoothed_data <- reactive({
    req(input$mu, input$sigma, input$fwhm)
    smoothed_gussian_curves(data = data_noise(), mu = input$mu, sig = input$sigma, fwhm = input$fwhm)
  })
  
  smoothed_data_2 <- reactive({
    req(input$mu, input$sigma, input$fwhm)
    smoothed_gussian_curves(data = data_noise_2(), mu = input$mu, sig = input$sigma, fwhm = input$fwhm)
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
      geom_line(linewidth = 1.5) +  # Set line thickness
      scale_color_manual(values = colorRampPalette(c("darkblue",
                         "lightblue"))(ncol(smoothed_data())),
                         labels = c(1:ncol(smoothed_data()))) +  # Navy blue shades
      labs(title = "Smooth Gaussian Noise", x = "Index", y = "Value") +  # Add labels
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
      theme(legend.position = "none")+
      #increase the font size of the labels and axis numbers
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16),
            #remove legend title
            legend.title = element_blank())
      
      
      
    
      
  })
  
  
  
  # Reactive expression for generating Gaussian pulse data
  data_pulse <- reactive({
    req(input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve")  # Ensure "baseline" is selected
    req(input$center, input$fwhm_pulse)
    gaussian_pulse(center = input$center, fwhm = input$fwhm_pulse, continuum_size = cont_size)
  })
  
  
  
  
  # Reactive expression for computing the half-max value of selected data
  default_amplitude <- reactive({
    
    req(input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve")  # Ensure "baseline" is selected
    req(selected_data())  # Ensure selected_data() is available
    
    # Compute the half-max value
    half_max <- ( selected_data()[which.max(abs(selected_data()))] ) / 2 
    # To return the half-max value  with its original sign
    
    return(round( half_max,digits = 1))
    
  })
  
  # Observe changes to set the default value of amplitude input
  observe({
    
    req(input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve")  # Ensure "baseline" is selected
    req(default_amplitude())  # Ensure default_amplitude() is available
    
    updateNumericInput(session, "amplitude", value = default_amplitude())
  })
  
  
  # Reactive expression for scaling the pulse
  scaled_pulse <- reactive({
    req(input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve")  # Ensure "baseline" is selected
    req(input$amplitude, data_pulse())
    amplitude_pulse(data = data_pulse()$density_val, amp = input$amplitude)
  })
  
  
  
  
  output$pulse_plot <- renderPlot({
    
    
    
    # Create a data frame with all necessary values, including a 'Legend' variable
    
    if (input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve") {
      
      req(scaled_pulse(), data_pulse())  # Ensure both are available


      plot_data <- data.frame(
        x_values = rep(data_pulse()$x_values, 3),  # Repeat x_values for 3 lines
        y_values = c(scaled_pulse(), selected_data(), selected_data() +
                       scaled_pulse()),  # Combine all y-values
        legend = factor(rep(c("Pulse", "Without pulse", "With pulse"), 
                            each = length(data_pulse()$x_values)))
      )
      

      
      
      color_values <- c("Pulse" = "black",
                        "Without pulse" = "cadetblue",
                        "With pulse" = "tomato")
      
    } else {
      
      req(selected_data())  # Ensure selected_data is available for two_sample
      
      # Create a data frame with the two-sample data
      plot_data <- data.frame(
        x_values = rep(0:(cont_size - 1), 3),  # Repeat x_values for 3 lines
        y_values = c((selected_data()[, 1] - selected_data()[, 2]), #first - second column
                     selected_data()[, 1], selected_data()[, 2]),  # Combine all y-values
        legend = factor(rep(c("Pulse", colnames(selected_data())[1], colnames(selected_data())[2]), 
                            each = dim(selected_data())[1]))  # Control factor levels
      )
      
      # Use setNames to create color_values dynamically
      color_values <- setNames(c("black", "cadetblue", "tomato"),
                               c("Pulse", 
                                 colnames(selected_data())[1], 
                                 colnames(selected_data())[2]))
      
    }
    
    
    # Create the plot using ggplot
    ggplot(plot_data, aes(x = x_values, y = y_values, color = legend)) +
      geom_line(linewidth = 1.5) +  # Plot lines for each group
      labs(title = "Selected Data", x = "Index", y = "Value") +  # Labels
      scale_color_manual(values = color_values) +  # Line colors
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
      theme(legend.position = "bottom")+
      #increase the font size of the labels and axis numbers
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.title = element_blank())
  })
  
  
  
  
  
  output$data_plot <- renderPlot({
    
    if (input$data_selection_type == "baseline" || input$data_selection_type == "custom_curve") {
      # Generate data with and without pulse
      Sample1 <- data_generator(data = selected_data(), signal = scaled_pulse(), noise = smoothed_data())
      Sample2 <- data_generator(data = selected_data(), noise = smoothed_data_2())
      
      sample_label_1 <- "With Pulse"
      sample_label_2 <- "Without Pulse"
      
      colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                   c( 
                                     sample_label_1, 
                                     sample_label_2))
      
    } else {
      # For two_sample case
      req(ncol(selected_data()) >= 2)  # Ensure selected_data() has at least 2 columns
      
      Sample1 <- data_generator(data = selected_data()[, 1], noise = smoothed_data())
      Sample2 <- data_generator(data = selected_data()[, 2], noise = smoothed_data_2())
      
      # Use the same labels as in pulse_plot
      sample_label_1 <- colnames(selected_data())[1]
      sample_label_2 <- colnames(selected_data())[2]
      
      colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                   c(sample_label_2, 
                                     sample_label_1))
    }
    
    # Calculate mean for each group
    sample1_mean <- rowMeans(Sample1)
    sample2_mean <- rowMeans(Sample2)
    
    # Create a long format data frame for ggplot
    plot_data <- data.frame(
      x_values = rep(1:nrow(Sample1), ncol(Sample1) * 2),  # Repeat index for each column and dataset
      y_values = c(as.vector(Sample1), as.vector(Sample2)),  # Flatten both datasets
      label = factor(rep(c(sample_label_1, sample_label_2),
                         each = nrow(Sample1) * ncol(Sample1))),  # Labels from selected_data()
      line_group = factor(rep(1:ncol(Sample1), each = nrow(Sample1), times = 2))  # Line group for each column
    )
    
    # Create a separate data frame for the mean lines
    mean_data <- data.frame(
      x_values = rep(1:nrow(Sample1), 2),
      y_values = c(sample1_mean, sample2_mean),
      label = factor(c(rep(sample_label_1, nrow(Sample1)), rep(sample_label_2, nrow(Sample1))), 
                     levels = c(sample_label_1, sample_label_2))
    )
    
    # Create the plot using ggplot2
    ggplot() +
      # First layer: Individual lines
      geom_line(data = plot_data, aes(x = x_values, y = y_values, group = interaction(line_group, label), color = label), linewidth = 1, alpha = 0.4) +
      # Second layer: Mean lines without group aesthetic
      geom_line(data = mean_data, aes(x = x_values, y = y_values, color = label), linewidth = 2.5) +
      scale_color_manual(values = colors_plot_data) +  # Set colors for both sample labels
      labs(title = "Generated Sample Data", x = "Index", y = "Value") +  # Add labels
      theme_minimal() +  # Use a minimal theme
      theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
      theme(legend.position = "bottom")+  # Move legend to bottom
      #increase the font size of the labels and axis numbers
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16),
            #increase legends font and element size
            legend.text = element_text(size = 12),
            legend.title = element_blank())
    
  })
  
  
  iteration_number <- 100
  


  
  # Delete file at end of session
  onStop(function(){
    print(status_file)
    if(file.exists(status_file))
      write("", status_file)
  })
  
  # Register user interrupt
  observeEvent(input$stop,{
    print("Cancel")
    fire_interrupt(status_file)
  })
  

  
  
  # Initialize the ExtendedTask for the power calculation
  power_task <- ExtendedTask$new(function(future_params, iteration_number) {
    future_promise({
      source("Data_functions.R") #source the functions in new session
      
      # Initialize method list for power calculation
      method_list <- Initialize_method_list(Methods = future_params$test_type,
                                            Conti_size = future_params$cont_size,
                                            Iter_number = iteration_number)
      write("start", status_file)
      
      for (i in 1:iteration_number) {
        
        # Check for user interrupts
        if(interrupted(status_file)){ 
          print("Stopping...")
          stop("User Interrupt")
        } else {
          # Generate data
          data <- Power_data_generator(future_params$sample_size,
                                       Data = future_params$selected_data,
                                       Signal = future_params$signal,
                                       Conti_size = future_params$cont_size,
                                       Noise_mu = future_params$noise_mu,
                                       Noise_sig = future_params$noise_sigma,
                                       Noise_fwhm = future_params$noise_fwhm)
          
          # Calculate p-values and update method_list
          method_list <- Pvalue_calculator(method_list, data$data1, data$data2)
        }
        

      } # for loop
      
      # Return the updated method_list
      method_list
      
    }, seed = TRUE) # seed = TRUE parallel-safe random numbers are produced via the L'Ecuyer-CMRG method
  }) |> bind_task_button("calculate")
  
  # Prepare the future parameters outside the future task
  observeEvent(input$calculate, {
    shinyjs::disable("calculate")  # Disable the button during task execution
    
    # Prepare the future parameters based on the input values.
    # Future does not support reactive values
    future_params <- list(
      sample_size = input$sample_size,
      selected_data = selected_data(),
      signal = if (input$data_selection_type == "baseline"||input$data_selection_type == "custom_curve") scaled_pulse() else NULL,
      cont_size = cont_size,
      noise_mu = input$mu,
      noise_sigma = input$sigma,
      noise_fwhm = input$fwhm,
      test_type = input$test_type
    )
    
    # Invoke the power calculation task
    power_task$invoke(future_params, iteration_number)
  })
  
  
  
  
  
  # Monitor the task and output the result once it's complete
  
  output$powerOutput <- renderUI({
    

    # Get the result from the power task
    task_result <- power_task$result()

    
    
    # Calculate the power based on the result
    power_results <- Power_calculator(task_result, iteration_number, Alpha = 0.05)
    
    # Re-enable the button after calculation
    shinyjs::enable("calculate")
    
    
    # Create a mapping between method names and display names
    method_names <- c("IWT" = "IWT", 
                      "TWT" = "TWT", 
                      "Parametric_SPM" = "SPM", 
                      "Nonparametric_SPM" = "SnPM",
                      "ERL" = "ERL",
                      "IATSE" = "IATSE")
    
    # Get the method names from power()
    methods <- names(power_results)
    
    # Create the result output, replacing method names with display names
    result <- sapply(methods, function(m) {
      display_name <- method_names[[m]] # Use display name
      paste("Power of", display_name, ":", round(power_results[[m]], 2))
    })
    HTML(paste(result, collapse = "<br>"))
  })
  
  
  output$status <- renderText({
    # Get the current status
    current_status <- power_task$status()
    
    # Map statuses to user-friendly messages
    if (current_status == "initial") {
      return("Not yet started...")
    } else if (current_status == "running") {
      # Display running status with ellipsis
      return("Running...")
    } else if (current_status == "success") {
      return("Finished")  # Or use "Done" if you prefer
    } else if (current_status == "error") {
      # Re-enable the button after getting error status (interrupt or other error)
      shinyjs::enable("calculate")
      return("Error occurred, please click on calculation button again or reload the app")  # Message for error status
      
    }
  })
  
  
}
  