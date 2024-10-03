#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#In Shiny’s fluidPage layout system, column is a function that defines how much
#space a component should occupy within a row. This is based on a 12-column grid
#system that’s common in responsive design frameworks

# column(4, ...) which is one-third of the available width (12) for tables.
# column(8, ...) which is two-third of the available width (12) for plots

#In Shiny, h3() is used to create a header or title of level 3 in HTML, which is
#typically used to represent a subheading. It is part of a hierarchy of heading 
#levels in HTML, where h1 is the highest level (largest and most important), 
#and h6 is the lowest level (smallest and least important).


library(shiny)
library(shinyjs)



# Define UI for application
ui <- fluidPage(
  
  # Initialize shinyjs
  useShinyjs(),
  
  includeCSS("www/styles.css"),

  
  titlePanel("Power ShinyApp"),
  
  
  
  # Create a tabset panel to organize different sections
  tabsetPanel(
    tabPanel("Dataset Selection",
             # Create a fluid row with radioButtons on the left and dataset selection on the right
             fluidRow(
               column(6,
                      # User selects data type (baseline or two-sample)
                      radioButtons("data_selection_type", h3("Choose the data type:"),
                                   choices = c("Baseline Data" = "baseline", 
                                               "Two-Sample Data" = "two_sample"),
                                   selected = "baseline")
               ),
               
               column(6,
                      # Conditionally show baseline data selection inputs
                      conditionalPanel(
                        condition = "input.data_selection_type == 'baseline'",
                        fluidRow(
                          column(12,
                                 selectInput("dataset_baseline", h4("Choose a dataset:"),
                                             choices = c("vGRF Robinson" = "vGRF_Robinson",
                                                         "vGRF Phan" = "vGRF_Phan",
                                                         "JCF" = "JCF",
                                                         "Hip Flexion Angle" = "Hip_Angle",
                                                         "Moment" = "Moment",
                                                         "Muscle Force" = "MF",
                                                         "EMG" = "EMG")),
                                 
                                 uiOutput("type_selector")
                          )
                        )
                      ),
                      
                      # Conditionally show two-sample data selection inputs
                      conditionalPanel(
                        condition = "input.data_selection_type == 'two_sample'",
                        selectInput("dataset_two_sample", h4("Choose a dataset with two group:"),
                                    choices = c("vGRF (normal and quiet)" = "vGRF_both",
                                                "JCF (lateral wedge and no wedge)" = "JCF_both",
                                                "Hip Flexion Angle (two individuals)" = "Hip_Angle_both",
                                                "Moment (direct and inverse kinematic)" = "Moment_both",
                                                "Muscle Force (control and diabetic)" = "MF_both",
                                                "EMG (young and adult)" = "EMG_both"))
                      )
               )
             )
    ),
    
    
    tabPanel("Plots",
             fluidRow(
               # First row with parameter boxes
               column(6,
                      wellPanel(
                        h3("Noise Parameters"),
                        numericInput("sample_size", "Sample Size:", value = 5, min = 1),
                        numericInput("mu", "Mean (μ):", value = 0),
                        numericInput("sigma", "Standard Deviation (σ):", value = 200, min = 0),
                        numericInput("fwhm", "Full Width at Half Maximum (FWHM):", min = 0, value = 10),
                        textOutput("error_message_noise")
                      )
               ),
               column(6,
                      conditionalPanel(
                        condition = "input.data_selection_type == 'baseline'",
                        wellPanel(
                          h3("Pulse Parameters"),
                          numericInput("center", "Center", value = 50, min = 0, max = 100),
                          numericInput("fwhm_pulse", "Full Width at Half Maximum (FWHM):", min = 1, value = 20),
                          numericInput("amplitude", "Amplitude", value = 300, step = 0.1),
                          textOutput("error_message_pulse")
                        )
                      )
               )
             ),
             fluidRow(
               # Second row with plots
               column(4,
                      plotOutput("noise_plot")
               ),
               column(4,
                      plotOutput("pulse_plot")
               ),
               column(4,
                      plotOutput("data_plot")
               )
             )
    ),
    
    tabPanel("Power Calculator",
             fluidRow(
               column(4,
                      selectInput("test_type", h3("Method:"), 
                                  choices = c("IWT", "TWT",
                                              "SPM" = "Parametric_SPM",
                                              "SnPM"="Nonparametric_SPM"),
                                  multiple = TRUE, selected = "TWT"),
                      actionButton("calculate", "Calculate Power"),
                      actionButton("stop", "Stop Calculation"),  # Button to stop the calculation
                      # Wrap the status output in a div with a specific class
                      div(
                        strong("Calculation Status: "),
                        textOutput("status", inline = TRUE),
                        class = "status-output"  # Assign a class for styling
                      )
               ),
               column(8,
                      # To display the power output
                      div(
                      # div () to be able to change the style of the output in the CSS file
                          wellPanel(
                            htmlOutput("powerOutput")
                          ),
                          # named class = "power_output" to be able to change the
                          # style of the output in the CSS file by calling this class
                          class = "power_output"
                      )
               )
               
             )
    )
  )#tabsetPanel
)#fluidPage(