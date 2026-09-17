library(shiny)
library(ggplot2)
library(shinyWidgets)
library(plotly)

# Load machine learning assets and functions
load("model.RData")
source("preprocess.R")
source("inference.R")
source("health_score.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #1a1e24; color: #ffffff; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .well { background-color: #242933 !important; border: 1px solid #3b4252 !important; }
      .irs-line { background: #4c566a !important; }
      .indicator-card { background: #242933; border-radius: 8px; padding: 15px; text-align: center; border: 1px solid #3b4252; height: 280px; }
      .thermometer-container { width: 30px; height: 180px; background: #3b4252; margin: 0 auto; border-radius: 15px; position: relative; border: 3px solid #4c566a; }
      .thermometer-fill { width: 100%; position: absolute; bottom: 0; border-radius: 10px; transition: height 0.5s ease-in-out; }
      .vibe-container { width: 40px; height: 180px; background: #3b4252; margin: 0 auto; border-radius: 4px; position: relative; overflow: hidden; }
      .vibe-fill { width: 100%; position: absolute; bottom: 0; background: linear-gradient(to top, #a3be8c, #ebcb8b, #bf616a); transition: height 0.2s ease; }
      @keyframes vibrate {
      0% { transform: rotate(0deg); }
      20% { transform: rotate(30deg); }
      40% { transform: rotate(--30deg); }
      60% { transform: rotate(45deg); }
      80% { transform: rotate(-45deg); }
      100% { transform: rotate(0deg); }
      }
     
      .status-box { padding: 15px; border-radius: 8px; font-weight: bold; font-size: 24px; text-align: center; margin-bottom: 20px; }
      .status-healthy { background-color: #434c5e; border-left: 8px solid #a3be8c; color: #a3be8c; }
      .status-warn { background-color: #434c5e; border-left: 8px solid #bf616a; color: #bf616a; }
      .table { color: #ffffff !important; }
      .table th { background-color: #3b4252 !important; }
    
      @keyframes blink {
      0%   { opacity: 1.0; background-color: #bf616a; color: #ffffff; }
     49%  { opacity: 1.0; background-color: #bf616a; color: #ffffff; }
     50%  { opacity: 0.1; background-color: transparent; color: transparent; }
     99%  { opacity: 0.1; background-color: transparent; color: transparent; }
     100% { opacity: 1.0; background-color: #bf616a; color: #ffffff; }
     }

    .blink-alert {
    animation: blink 0.8s infinite steps(1);
    }
    "))
  ),
  
  titlePanel(h1("Oracle AeroGuard Control Panel", style="font-weight: 700; color: #88c0d0; padding-bottom: 10px;")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("temp", "Temperature (°F)", 50, 150, 80),
      sliderInput("vib", "Vibration Intensity", 0, 10, 2, step=0.1),
      sliderInput("press", "Pressure (psi)", 20, 80, 50),
      sliderInput("rpm", "RPM", 1000, 5000, 2500),
      br(),
      actionButton("start", "Start Real-Time Monitoring", class="btn-info btn-block", style="font-weight: bold;")
    ),
    
    mainPanel(
      fluidRow(
        column(12, uiOutput("statusHeader"))
      ),
      
      fluidRow(
        column(2, div(class="indicator-card", h4("Aircraft Health"), br(), uiOutput("healthBar"))),
        column(3, div(class="indicator-card", style="padding:0;", plotlyOutput("rpmGauge", height="270px"))),
        column(3, div(class="indicator-card", style="padding:0;", plotlyOutput("pressGauge", height="270px"))),
        column(2, div(class="indicator-card", h4("Temperature"), uiOutput("thermometer"))),
        column(2, div(class="indicator-card", h4("Vibration"), uiOutput("vibrationMeter")))
      ),
      br(),
      
      # Plotly Accelerated Real-Time Line Charts
      fluidRow(
        column(6, plotlyOutput("tempPlot", height="230px")),
        column(6, plotlyOutput("vibPlot", height="230px"))
      ),
      br(),
      fluidRow(
        column(6, plotlyOutput("pressPlot", height="230px")),
        column(6, plotlyOutput("rpmPlot", height="230px"))
      ),
      br(),
      
      fluidRow(
        column(12, h3("Live Stream Log History", style="color: #88c0d0;"), tableOutput("history"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    running = FALSE,
    time = c(), temp = c(), vib = c(), press = c(), rpm = c(), health = c(),
    pred_status = "0",
    current_health = 100,
    history = data.frame(Time=character(), Temp=numeric(), Vibration=numeric(), Pressure=numeric(), RPM=numeric(), Health=numeric())
  )
  
  observeEvent(input$start, {
    rv$running <- TRUE
  })
  
  observe({
    req(rv$running)
    invalidateLater(500, session)
    
    isolate({
      # 1. Read Slider Inputs and Apply Minor Jitter
      curr_temp  <- as.numeric(input$temp) + runif(1, -0.5, 0.5)
      curr_vib   <- max(0, min(10, as.numeric(input$vib) + runif(1, -0.05, 0.05)))
      curr_press <- as.numeric(input$press) + runif(1, -0.5, 0.5)
      curr_rpm   <- as.numeric(input$rpm) + sample(-10:10, 1)
      
      # 2. Structure Machine Learning Input
      new_data <- data.frame(
        temperature = curr_temp, 
        vibration = curr_vib, 
        pressure = curr_press, 
        rpm = curr_rpm
      )
      
      # 3. Model Prediction Evaluation
      pred <- predict_local(model, preprocess(new_data))
      rv$pred_status <- trimws(as.character(pred[1]))
      
      # 4. Engine Core Health Metric Calculation
      health_val <- health_score(curr_temp, curr_vib)
      rv$current_health <- round(max(0, min(100, health_val)))
      
      # 5. Build Log History Records
      row_entry <- data.frame(
        Time = format(Sys.time(), "%H:%M:%S"),
        Temp = round(curr_temp, 1),
        Vibration = round(curr_vib, 2),
        Pressure = round(curr_press, 1),
        RPM = round(curr_rpm),
        Health = rv$current_health
      )
      
      rv$history <- rbind(row_entry, rv$history)
      if (nrow(rv$history) > 8) { rv$history <- head(rv$history, 8) }
      write.table(row_entry, "logs.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
      
      # Step Timeline Ticks Safely
      next_tick <- if (length(rv$time) == 0) 1 else max(rv$time) + 1
      
      rv$time   <- c(rv$time, next_tick)
      rv$temp   <- c(rv$temp, curr_temp)
      rv$vib    <- c(rv$vib, curr_vib)
      rv$press  <- c(rv$press, curr_press)
      rv$rpm    <- c(rv$rpm, curr_rpm)
      rv$health <- c(rv$health, rv$current_health)
      
      # Dynamic Sliding Window Viewport Limit
      if (length(rv$time) > 25) {
        rv$time   <- tail(rv$time, 25)
        rv$temp   <- tail(rv$temp, 25)
        rv$vib    <- tail(rv$vib, 25)
        rv$press  <- tail(rv$press, 25)
        rv$rpm    <- tail(rv$rpm, 25)
        rv$health <- tail(rv$health, 25)
      }
    })
  })
  
  # UI Alert Dynamic Header Rendering
  output$statusHeader <- renderUI({
    req(rv$running)
    if (rv$pred_status == "1") {
      div(class="status-box status-warn blink-alert", "⚠️ MAINTENANCE REQUIRED:FAULT DETECTION ALERT")
    } else {
      div(class="status-box status-healthy", paste("SYSTEM ONLINE - AIRCRAFT HEALTH:", rv$current_health, "%"))
    }
  })
  
  # Custom Component Render Blocks
  output$healthBar <- renderUI({
    req(rv$running, length(rv$health) > 0)
    color <- if(rv$current_health > 70) "#a3be8c" else if(rv$current_health > 40) "#ebcb8b" else "#bf616a"
    div(
      div(style=sprintf("width:100%%; height:140px; background:#3b4252; border-radius:10px; position:relative; border:2px solid %s;", color),
          div(style=sprintf("position:absolute; bottom:0; width:100%%; height:%s%%; background:%s; transition: height 0.5s; border-radius:8px;", rv$current_health, color))
      ),
      br(), h3(paste0(rv$current_health, "%"), style="margin:0; font-weight:bold;")
    )
  })
  
  output$thermometer <- renderUI({
    req(rv$running, length(rv$temp) > 0)
    val <- tail(rv$temp, 1)
    pct <- max(0, min(100, ((val - 50) / 100) * 100))
    color <- if(val > 120) "#bf616a" else if(val > 95) "#ebcb8b" else "#81a1c1"
    div(
      div(class="thermometer-container",
          div(class="thermometer-fill", style=sprintf("height: %s%%; background: %s;", pct, color))
      ),
      div(style="padding-top:10px; font-weight:bold;", paste(round(val,1), "°F"))
    )
  })
  
  output$vibrationMeter <- renderUI({
    req(rv$running, length(rv$vib) > 0)
    val <- tail(rv$vib, 1)
    pct <- (val / 10) * 100
    anim_speed<-if(val>0.1) paste0(max(0.00001,0.0008/val),"s")else "0s"
    div(
      div(class="vibe-container",
          div(class="vibe-fill", style=sprintf("height: %s%%;", pct)),
          div(style=sprintf("position:absolute; bottom:%s%%; width:100%%; border-top:3px solid #ffffff; box-shadow: 0 0 8px #fff;transform-origin:center;animation: vibrate %s infinite linear;", pct,anim_speed))
      ),
      div(style="padding-top:10px; font-weight:bold;", paste(round(val,2), "Hz"))
    )
  })
  
  output$rpmGauge <- renderPlotly({
    val <- if(rv$running && length(rv$rpm) > 0) tail(rv$rpm, 1) else 1000
    plot_ly(
      type = "indicator", mode = "gauge+number", value = val, title = list(text = "Engine RPM", font = list(size = 15, color="#fff")),
      number = list(font = list(color = "#fff")),
      gauge = list(
        axis = list(range = list(1000, 5000), tickwidth = 1, tickcolor = "#fff"),
        bar = list(color = "#88c0d0"), bgcolor = "#3b4252",
        steps = list(list(range = list(1000, 3500), color = "#434c5e"), list(range = list(3500, 5000), color = "#bf616a"))
      )
    ) %>% layout(margin = list(l=20,r=20,b=10,t=40), paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$pressGauge <- renderPlotly({
    val <- if(rv$running && length(rv$press) > 0) tail(rv$press, 1) else 20
    plot_ly(
      type = "indicator", mode = "gauge+number", value = val, title = list(text = "Pressure (psi)", font = list(size = 15, color="#fff")),
      number = list(font = list(color = "#fff")),
      gauge = list(
        axis = list(range = list(20, 80), tickcolor = "#fff"),
        bar = list(color = "#b48ead"), bgcolor = "#3b4252",
        steps = list(list(range = list(20, 65), color = "#434c5e"), list(range = list(65, 80), color = "#bf616a"))
      )
    ) %>% layout(margin = list(l=20,r=20,b=10,t=40), paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  # --- Smooth Accelerating High-Performance Plotly Trend Charts ---
  output$tempPlot <- renderPlotly({
    req(length(rv$time) > 0)
    plot_ly(data.frame(x=rv$time, y=rv$temp), x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#81a1c1', width = 3), marker = list(color = '#88c0d0')) %>%
      layout(title = list(text = "Temperature Trend Trace", font = list(color = "#88c0d0", size = 14)),
             paper_bgcolor = "#242933", plot_bgcolor = "#242933",
             xaxis = list(title = "Timeline Ticks", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             yaxis = list(title = "°F", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             margin = list(l=50, r=20, t=40, b=40))
  })
  
  output$vibPlot <- renderPlotly({
    req(length(rv$time) > 0)
    plot_ly(data.frame(x=rv$time, y=rv$vib), x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#bf616a', width = 3), marker = list(color = '#bf616a')) %>%
      layout(title = list(text = "Structural Vibration Harmonics", font = list(color = "#88c0d0", size = 14)),
             paper_bgcolor = "#242933", plot_bgcolor = "#242933",
             xaxis = list(title = "Timeline Ticks", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             yaxis = list(title = "Hz", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             margin = list(l=50, r=20, t=40, b=40))
  })
  
  output$pressPlot <- renderPlotly({
    req(length(rv$time) > 0)
    plot_ly(data.frame(x=rv$time, y=rv$press), x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#b48ead', width = 3), marker = list(color = '#b48ead')) %>%
      layout(title = list(text = "Manifold Fluid Pressure Logs", font = list(color = "#88c0d0", size = 14)),
             paper_bgcolor = "#242933", plot_bgcolor = "#242933",
             xaxis = list(title = "Timeline Ticks", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             yaxis = list(title = "psi", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             margin = list(l=50, r=20, t=40, b=40))
  })
  
  output$rpmPlot <- renderPlotly({
    req(length(rv$time) > 0)
    plot_ly(data.frame(x=rv$time, y=rv$rpm), x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#a3be8c', width = 3), marker = list(color = '#a3be8c')) %>%
      layout(title = list(text = "Rotational Speed Engine Signatures (RPM)", font = list(color = "#88c0d0", size = 14)),
             paper_bgcolor = "#242933", plot_bgcolor = "#242933",
             xaxis = list(title = "Timeline Ticks", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             yaxis = list(title = "RPM", gridcolor = "#3b4252", tickfont = list(color = "#d8dee9"), titlefont = list(color = "#d8dee9")),
             margin = list(l=50, r=20, t=40, b=40))
  })
  
  output$history <- renderTable({
    req(nrow(rv$history) > 0)
    rv$history
  }, striped = TRUE, hover = TRUE, spacing = "m")
}

shinyApp(ui, server)
