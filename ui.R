#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
fluidPage(
  
  titlePanel("Smooth Gaussian Noise"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sample_size", "Sample Size:", value = 5, min = 1),
      #numericInput("continuum_size", "Continuum Size:", value = 101),
      numericInput("mu", "Mean (μ):", value = 0),
      numericInput("sigma", "Standard Deviation (σ):", value = 200,min = 0),
      numericInput("fwhm", "Full Width at Half Maximum (FWHM):", min = 0, value = 10),
      textOutput("error_message_noise")
    ),
    mainPanel(
      plotOutput("noise_plot")
    )
  ),
  
  
  titlePanel("Gaussian Pulse"),
  sidebarLayout(
    sidebarPanel(
      numericInput("center", "Center", value = 50, min = 0, max = 100),
      numericInput("fwhm_pulse", "Full Width at Half Maximum (FWHM):", min = 1, value = 20),
      numericInput("amplitude", "Amplitude", value = 300, min = 0),
      textOutput("error_message_pulse")
    ),
    mainPanel(
      plotOutput("pulse_plot")
    )
  ),
  
  titlePanel("Baseline Data + Signal + Noise"),
    mainPanel(
      plotOutput("data_plot")
  ),
  
  
  titlePanel("Power Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("test_type", "Method:", 
                  choices = c("IWT", "TWT", "Parametric_SPM"#, "Nonparametric_SPM"
                              ), multiple = TRUE, selected = "TWT"),
      actionButton("calculate", "Calculate Power")
    ),
    mainPanel(
      wellPanel(
        textOutput("powerOutput")
      )
    )
  )

  
  
  
  
  
)






