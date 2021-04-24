# Load packages ----
library(dplyr)
library(plyr)
library(reshape2)
library(tidyverse)
library(data.table)
library(haven)
library(gt)
library(glue)
library(shiny)
library(DT)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Tabla", "Selecciona una tabla", choices = c("Recortada2","Recortada3"))
    ),
    column(4,
           selectInput("RFC","Selecciona un RFC",choices = c("Todos los RFC",unique(as.character(RFC$ctarfc))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

# Server logic
server <- function(input, output) {

  tablaInput <- reactive({
    switch(input$Tabla,
           "Recortada2" = Recortada2,
           "Recortada3" = Recortada3)
  })
  
  RFCInput <- reactive({input$RFC
  })

  output$table <- renderDataTable({datatable({
    tabla <- tablaInput()
    if (input$RFC != "Todos los RFC") {
      tabla <- tabla[tabla$ctarfc == input$RFC,]
    }
    tabla
  })})

}

# Run the app
shinyApp(ui, server)
