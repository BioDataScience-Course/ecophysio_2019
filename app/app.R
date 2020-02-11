# Note: this is also a gist. To run it, make:
# runGist("https://gist.github.com/phgrosjean/cf2064a3ab06e8306552d713bc319a0a")

library(shiny)
library(simecol)

ui <- fluidPage(
  title = "Respirometer designer",
  plotOutput(outputId = "O2Plot"),
  hr(),
  fluidRow(
    column(3, offset = 0,
      # Respirometer characteristics
      h4("Respirometer"),
      sliderInput(inputId = "vol", label = "Volume (L):", min = 0.1, max = 10, step = 0.1, value = 1.5),
      sliderInput(inputId = "time", label = "Open/close duration (min):", min = 1, max = 180, step = 5, value = 60),
      sliderInput(inputId = "change", label = "Water change (%/min):", min = 0, max = 5, step = 0.25, value = 2)
    ),
    column(3, offset = 0,
      # Organism characteristics
      h4("Organism"),
      sliderInput(inputId = "mass", label = "Mass (g):", min = 0.1, max = 5, step = 0.1, value = 0.5),
      sliderInput(inputId = "respi", label = "Respiration (mg O2/g/min):", min = 0.01, max = 0.5, step = 0.01, value = 0.05),
      sliderInput(inputId = "photo", label = "Photosynthesis (mg O2/g/min):", min = 0, max = 1.5, step = 0.02, value = 0.12)
    ),
    column(3, offset = 0,
      # Water characteristics
      h4("Water"),
      sliderInput(inputId = "o2ini", label = "[O2] (mg/L):", min = 0, max = 15, step = 0.5, value = 8),
      sliderInput(inputId = "min", label = "Minimum [O2] (mg/L):",  min = 0, max = 15, step = 0.5, value = 6),
      sliderInput(inputId = "max", label = "Maximum [O2] (mg/L):",  min = 5, max = 20, step = 0.5, value = 12)
    ),
    column(3,
      # Time data
      h4("Timing"),
      sliderInput(inputId = "light", label = "Duration of light (h/day):", min = 0, max = 24, step = 0.5, value = 12),
      sliderInput(inputId = "timeini", label = "Start (h):", min = -12, max = 18, step = 0.5, value = 0),
      sliderInput(inputId = "timelength", label = "Duration (h):", min = 2, max = 52, step = 0.5, value = 26)
    )
  )
)

server <- function(input, output) {
  output$O2Plot <- renderPlot({
    library(simecol)
    RespVol    <- input$vol       # Volume of the respirometer [L]
    O2Ini      <- input$o2ini     # Initial O2 concentration in the respirometer [mg/L]
    RespFlow   <- input$change / 100 * RespVol  # Water change rate when respirometer in open [L/min]
    RespTime   <- input$time      # Length in time of open-closed phase [min]
    O2Flow     <- O2Ini           # O2 concentration in change water [mg/L]
    ## Organism characteristics
    RespRate   <- input$respi     # Respiration rate [mg O2/g/min].
    PhotRate   <- input$photo     # Photosynthesis rate [mg O2/g/min]
    Mass       <- input$mass      # Mass of the organism [g] 
    ## Day/night characteristic
    LightTime  <- input$light * 60  # Length of light phase (on 24h)
    ## Min-max tolerated O2 values
    O2Range    <- c(input$min, input$max) # Min-max tolerated O2 concentration [mg/L]
    ## Time info
    Times     <- c(from = input$timeini * 60, to = (input$timeini + input$timelength) * 60, by = 1) # Time [min]
    
    # The model
    respire <- new("odeModel",
      main = function(time, init, parms) {
        Vol <- init["Vol"]
        O2  <- init["O2"]
        with(as.list(parms), {
          dVol <- 0   # Volume of the respirometer does not change
          O2Rate <- RespRate
          # Is light on?
          LightOn <- time %% (24 * 60) <= LightTime
          # If yes, also consider photosynthesis
          if (LightOn) O2Rate <- RespRate - PhotRate
          dO2 <- -O2Rate / Vol
          # Is the respirometer closed?
          RespOpen <- as.logical(time %/% RespTime %% 2)
          if (is.na(RespOpen)) RespOpen <- FALSE
          # If the respirometer is open, also consider water change
          if (RespOpen) dO2 <- dO2 + RespFlow * (O2Flow - O2) / Vol
          # Avoid negative values for O2
          #if (O2 + dO2 < 0) dO2 <- -O
          list(c(dVol, dO2))
        })
      },
      equations = list(),
      times = Times,
      parms = c(RespRate = RespRate * Mass, PhotRate = PhotRate * Mass,
        RespTime = RespTime, RespFlow = RespFlow, O2Flow = O2Flow,
        LightTime = LightTime),
      init = c(Vol = RespVol, O2 = O2Ini),
      solver = "lsoda"
    )
    
    respire <- sim(respire)
    res <- out(respire)
    
    # O2 plot
    res$timeh <- res$time / 60
    library(ggplot2)
    library(chart)
    chart(data = res, O2 ~ timeh) +
      geom_line() +
      geom_hline(yintercept = O2Range, col = "red", lty = 2) +
      xlab("Time (h)") +
      ylab("[O2] (mg/L)") +
      theme_bw()
    
    # When do we reach O2Max or O2Min (in hours)?
    #(res$time[res$O2 > O2Range[2]][1]) / 60
    #(res$time[res$O2 < O2Range[1]][1]) / 60
  })
}

shinyApp(ui, server)
