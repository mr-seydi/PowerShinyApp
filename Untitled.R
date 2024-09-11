function(input, output, session) {
  
  # Observe changes in 'dataset' to reset 'type_input'
  observe({
    req(input$dataset)  # Ensure dataset input is available
    
    # Reset type_input to NULL when dataset changes
    updateSelectInput(session, "type_input", selected = NULL)
    print("Resetting type_input to NULL")
  })
  
  # Create a reactive UI for the 'type' selection based on dataset choice
  output$type_selector <- renderUI({
    # Define choices based on dataset
    choices <- switch(input$dataset,
                      "vGRF_Robinson" = c("mean"),
                      "vGRF_Phan" = c("quiet", "normal"),
                      "JCF" = c("lateral_wedge", "no_wedge"),
                      "Hip_Angle" = c("individual1", "individual2"),
                      "Moment" = c("DK", "IK"),
                      "MF" = c("control", "diabetic"),
                      "EMG" = c("adult", "young"),
                      NULL)  # Default to NULL if no dataset is selected
    
    # Debugging line to check choices
    print(paste("Choices for type_input based on dataset:", paste(choices, collapse = ", ")))
    
    # Render selectInput with choices
    selectInput("type_input", "Choose type:", choices = choices, selected = NULL)
  })
  
  # Reactive expression to load the appropriate data based on user input
  selected_data <- reactive({
    req(input$dataset)  # Ensure dataset input is available
    
    valid_types <- c("mean", "quiet", "normal", "lateral_wedge", "no_wedge", "individual1", "individual2", "DK", "IK", "control", "diabetic", "adult", "young")
    if (is.null(input$type_input) || !input$type_input %in% valid_types) {
      print(paste("Invalid type_input:", input$type_input))
      return(NULL)
    }
    
    # Debugging lines
    print(paste("Selected dataset:", input$dataset))
    print(paste("Selected type:", input$type_input))
    
    # Switch based on dataset selected
    switch(input$dataset,
           "vGRF_Robinson" = vGRF_data_Robinson(type = input$type_input),
           "vGRF_Phan" = vGRF_data_Phan(type = input$type_input),
           "JCF" = {
             print(paste("Calling JCF_data with type:", input$type_input))
             JCF_data(type = input$type_input)
           },
           "Hip_Angle" = Angle_data(type = input$type_input),
           "Moment" = Moment_data(type = input$type_input),
           "MF" = MF_data(type = input$type_input),
           "EMG" = EMG_data(type = input$type_input),
           NULL)  # Fallback if dataset or type is not matched
  })
}

