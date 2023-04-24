#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize = 100 * 1024^2)
library(shiny)
if(!require(pacman)){
  install.packages(pacman)
}
pacman::p_load(conflicted,tidyverse,wrappedtools, CERTAINdm)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("CERTAIN Export Manager"),
  checkboxInput(inputId = 'dropcode',label = 'Drop ...Code columns?',
               value = TRUE),
  fileInput(inputId = 'ct_file',label = 'Select CERTAIN export file',
            accept='.xlsx', width=800),
  numericInput("n rows preview", "Rows", value = 5, min = 1, step = 1),
  # tableOutput("head"),
  textOutput('sheetlist'),
  verbatimTextOutput('sheets'),
  uiOutput('sheetselector'),
  actionButton('importbutton',label = 'Import selected'),
  # reactiveVal('certaindatalist'),
  textOutput('Imported'),
  tableOutput('Sheet1')
#,
  # textOutput()
)


server <- function(input, output, session) {
  certain_sheets <- reactive({
    req(input$ct_file)
     # sheetnames <- 
       show_excelsheets(input$ct_file$datapath) 
     # paste(seq_along(sheetnames), sheetnames, sep = ': ', collapse='\n')
  })
  
    hl_sheets <- reactive({
      req(input$ct_file)
      paste('Data sheets available in',input$ct_file$name,':')
    })
    
    output$sheetlist <- renderText({
      hl_sheets()})
    output$sheets <- renderText({
    certain_sheets()
    })
    # 
    sheetpicked <- reactive({
      req(input$ct_file)
checkboxGroupInput(inputId = 'sheetpicker',
                   label = 'Select sheets to import:',
                   choices = certain_sheets(),
                   selected = certain_sheets())
            # textInput(inputId = 'sheetpicker', 
      #         label = 'Select numbers for sheets to import',
      #         width = 300,
      #         placeholder = 'e.g. 1:5,9,12:20')
      })
    output$sheetselector <- 
      renderUI({
        sheetpicked()
      })
    
    certaindatalist <- eventReactive(input$importbutton,{
      import_excelsheets(file = input$ct_file$datapath,
                         sheets = NULL,#sheetpicked(),
                         skip_codecols = input$dropcode)
      })

    output$Imported <- renderText({
      paste(certaindatalist() |> names())})
    
    output$Sheet1 <- renderTable({
      head(certaindatalist()[[1]], input$n)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
