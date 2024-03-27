

# Shiny app to estimate crop water use from measurements at AZMet stations


# Libraries
library(azmetr)
library(dplyr)
library(htmltools)
library(lubridate)
library(purrr)
library(shiny)
library(stringr)
library(tidyr)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station, specify the annual crop, and set the planting and end dates for the period of interest. Then, click or tap 'ESTIMATE WATER USE'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stationNames[order(stationNames$stationName), ]$stationName,
          selected = stationNames[order(stationNames$stationName), ]$stationName[1]
        ),
        
        selectInput(
          inputId = "annualCrop", 
          label = "Annual Crop",
          choices = cropGrowingSeasonLengths[order(cropGrowingSeasonLengths$crop), ]$crop,
          selected = cropGrowingSeasonLengths[order(cropGrowingSeasonLengths$crop), ]$crop[1]
        ),
        
        dateInput(
          inputId = "plantingDate",
          label = "Planting Date",
          value = Sys.Date() - 7,
          min = databaseStart,
          max = Sys.Date() - 2,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        dateInput(
          inputId = "endDate",
          label = "End Date",
          value = Sys.Date() - 1,
          min = databaseStart + 1,
          max = Sys.Date() - 1,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "estimateWaterUse", 
          label = "Estimate Water Use",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableTitle"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableSubtitle"))
      ),
      
      br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "summaryText"))
      ),
      
      br(), br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, tableOutput(outputId = "tableHelpText"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, tableOutput(outputId = "dataTablePreview"))
      ), 
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableCaption"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, uiOutput(outputId = "downloadButtonTSV"))
      ),
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableFooter"))
      ),
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # AZMet data ELT
  dAZMetDataELT <- eventReactive(input$estimateWaterUse, {
    validate(
      need(expr = input$plantingDate < input$endDate, message = FALSE)
    )
    
    idPreview <- showNotification(
      ui = "Estimating water use . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idPreview",
      type = "message"
    )
    
    on.exit(removeNotification(id = idPreview), add = TRUE)
    
    fxnAZMetDataELT(
      azmetStation = input$azmetStation, 
      timeStep = "Daily", 
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  # Calculate accumulation values, crop water use estimates
  dCalculateETc <- eventReactive(dAZMetDataELT(), {
    fxnCalculateETc(
      dAZMetDataELT = dAZMetDataELT(),
      annualCrop = input$annualCrop,
      growingSeasonLength = growingSeasonLength()
    )
  })
  
  # Format AZMet data for HTML table preview
  dAZMetDataPreview <- eventReactive(dCalculateETc(), {
    fxnAZMetDataPreview(
      dCalculateETc = dCalculateETc(),
      timeStep = "Daily")
  })
  
  # Total growing season length in days for specified crop based on FAO model, `cropGrowingSeasonLengths` loaded into environment by `scr01SetupApp.R`
  growingSeasonLength <- reactive({
    cropGrowingSeasonLengths$growingSeasonLength[
      which(cropGrowingSeasonLengths$crop == input$annualCrop)
    ]
  })
  
  # Build summary text
  summaryText <- eventReactive(dCalculateETc(), {
    fxnSummaryText(
      azmetStation = input$azmetStation,
      annualCrop = input$annualCrop,
      plantingDate = input$plantingDate,
      endDate = input$endDate,
      dCalculateETc = dCalculateETc()
    )
  })
  
  # Build table caption
  tableCaption <- eventReactive(dCalculateETc(), {
    fxnTableCaption()
  })
  
  # Build table footer
  tableFooter <- eventReactive(dCalculateETc(), {
    fxnTableFooter(
      annualCrop = input$annualCrop,
      plantingDate = input$plantingDate,
      endDate = input$endDate,
      growingSeasonLength = growingSeasonLength()
    )
  })
  
  # Build table footer help text
  tableFooterHelpText <- eventReactive(dCalculateETc(), {
    fxnTableFooterHelpText()
  })
  
  # Build table help text
  tableHelpText <- eventReactive(dCalculateETc(), {
    fxnTableHelpText()
  })
  
  # Build table subtitle
  tableSubtitle <- eventReactive(dCalculateETc(), {
    fxnTableSubtitle(plantingDate = input$plantingDate, endDate = input$endDate)
  })
  
  # Build table title
  tableTitle <- eventReactive(input$estimateWaterUse, {
    validate(
      need(
        input$plantingDate < input$endDate, 
        "Please select a 'Planting Date' that is earlier than the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    fxnTableTitle(azmetStation = input$azmetStation, annualCrop = input$annualCrop)
  })
  
  # Outputs -----
  
  output$dataTablePreview <- renderTable(
    expr = dAZMetDataPreview(), 
    striped = TRUE, 
    hover = TRUE, 
    bordered = FALSE, 
    spacing = "xs", 
    width = "auto", 
    align = "c", 
    rownames = FALSE, 
    colnames = TRUE, 
    digits = NULL, 
    na = "na"
  )
  
  output$downloadButtonTSV <- renderUI({
    req(dAZMetDataPreview())
    downloadButton(
      outputId = "downloadTSV", 
      label = "Download .tsv", 
      class = "btn btn-default btn-blue", 
      type = "button"
    )
  })
  
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste0(
        "AZMet ", input$azmetStation, " ", input$annualCrop, " water use estimates ", input$plantingDate, " to ", input$endDate, ".tsv"
      )
    },
    content = function(file) {
      vroom::vroom_write(x = dAZMetDataPreview(), file = file, delim = "\t") # Figure out whether to select here or in `fxnCalculateETc.R`
    }
  )
  
  output$summaryText <- renderUI({
    summaryText()
  })
  
  output$tableCaption <- renderUI({
    tableCaption()
  })
  
  output$tableFooter <- renderUI({
    tableFooter()
  })
  
  output$tableFooterHelpText <- renderUI({
    tableFooterHelpText()
  })
  
  output$tableHelpText <- renderUI({
    tableHelpText()
  })
  
  output$tableSubtitle <- renderUI({
    tableSubtitle()
  })
  
  output$tableTitle <- renderUI({
    tableTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
