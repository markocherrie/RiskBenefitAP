library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Optimal MPA Calculator, under different background PM conditions"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mpa", "MPA Duration (hours)", value = 0, min = 0, max = 112),
      numericInput("pm", "Background PM2.5 (μg/m3)", value = 2, min = 2)
    ),
    
    mainPanel(
      verbatimTextOutput("mort_ratio")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # static params
  InhalSleep <- 0.3
  InhalRest <- 0.6
  InhalMPA <- 3
  
  ConcFactor <- 1.5
  
  MortalityRiskRatio <- 1.089
  
  # These are the params to calculate the mortality risk ratio for 
  # physical activity benefit
  B0 <- 0.0009438848 
  B1 <- -0.0357964234
  B2 <- 0.0008082915 
  B3 <- -0.0000055871
 
  # Calculate mortality risk ratio
  
  MRR <-function(a, b){
  # eq 1
  #MPAduration = 112 - WeekRestDur
  
  # eq 2
  ExposurePM <- ConcFactor * b
  
  # eq 3
  InhaledDoseWithoutOutdoorMPA <- 
    (InhalSleep * 56 * b) +
    (InhalRest  * 112 * b)
  
  # eq 4
  InhaledDoseWithOutdoorMPA <- 
    (InhalSleep * 56 * b) +
    (InhalRest  * (112 - a) * b) +
    (InhalMPA   * a * ExposurePM)
  
  # Eq 5
  IncreasePM <- ((InhaledDoseWithOutdoorMPA/InhaledDoseWithoutOutdoorMPA-1))*b
  
  # eq 6
  MortalityRiskRatioPM <- exp(log(MortalityRiskRatio)*(IncreasePM/10))
  
  # eq 7 
  MortalityRiskRatioMPA <- exp(B0 + (B1*a) + (B2*a^2) + (B3*a^3))
  
  # eq8 
  MortalityRiskRatioOver <- MortalityRiskRatioMPA * MortalityRiskRatioPM 
 
  return(c(MortalityRiskRatioPM, MortalityRiskRatioMPA, MortalityRiskRatioOver))
  
  }
  
  
  a<-reactive({input$mpa})
  b<-reactive({input$pm})
  
  output$mort_ratio <- renderPrint({
    cat(paste0("Overall Mortality Risk Ratio: ", round(MRR(a(), b()), 2)[3], "\n", 
           "PM Mortality Risk Ratio: ", round(MRR(a(), b()), 2)[1], "\n", 
           "MPA Mortality Risk Ratio: ", round(MRR(a(), b()), 2)[2]))
  })
}

# Run the app
shinyApp(ui = ui, server = server)