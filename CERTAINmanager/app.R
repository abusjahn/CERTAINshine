if(!require(pacman)) {
  install.packages(pacman)
}
pacman::p_load(
  conflicted,
  shiny, shinycssloaders,shinydashboard,dashboardthemes,shinydashboardPlus,shinyjs,
  readxl,
  tidyverse,
  CERTAINdm,
  wrappedtools,
  ggbeeswarm)
conflicts_prefer(shinydashboard::box)
conflicts_prefer(shinydashboardPlus::dashboardPage)
conflicts_prefer(shinydashboardPlus::dashboardHeader)
conflicts_prefer(shinydashboardPlus::dashboardSidebar)

options(shiny.maxRequestSize = 100 * 1024^2)

ui <- dashboardPage(
  header = dashboardHeader(
    title="CERTAIN Export Manager",
    titleWidth = 400),
  
  sidebar = dashboardSidebar(disable = T,
                             minified = F),
  
  body = dashboardBody(
    theme=shinyDashboardThemes('blue_gradient'),
    tags$style(type="text/css", ".content-wrapper {background-color: #e8ecec}"),    # fluidRow(
    tabItem(
      # tabBox(
      #   width=12,
      #   height="800px",
      tabName='t1',
      tabsetPanel(
        type = 'tabs',
        tabPanel('Import/Export',
                 checkboxInput(inputId = 'dropcode',label = 'Drop ...Code columns?',
                               value = TRUE),
                 checkboxInput(inputId = 'dropempty',label = 'Drop empty columns?',
                               value = TRUE),
                 fileInput(inputId = "file", accept = ".xlsx", 
                           label = 'Select CERTAIN export file', width=400)),
        tabPanel('Sheets',
                 textOutput('sheetlist') ,
                 dataTableOutput('sheets') %>% 
                   withSpinner(type = 6,color="#Fdc5c1")
        ),
        tabPanel('Summary',
                 fluidRow(
                   column(
                     width=3,
                     uiOutput('sheetpicker')),
                   column(
                     width=3,
                     uiOutput('typepicker'))),
                 fluidRow(textOutput('columnlist')),
                 # div(
                 dataTableOutput('sheetsummary') %>% 
                   withSpinner(type = 6,color="#0dc5c1")
                 # , style = "font-size: 80%")
        ),
        tabPanel('Figures',
                 fluidRow(
                   column(
                     width=3,
                     uiOutput('sheetpicker2')),
                   column(
                     width=5,
                     uiOutput('columnpicker'))),
                 fluidRow(
                   plotOutput('colexplore') %>% 
                     withSpinner(type = 6,color="#0dFFc1")
                 )
        )
      )#, width=9
    )
  ),
  controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
  skin='midnight'
)

# )
# tags$head(tags$style(HTML('.main-header {background-color: #FF00FF;}')))

server <- function(input, output) {
  
  ct_datalist <- reactive({
    file <- input$file
    if(is.null(file)) {
      return(NULL)
    }
    import_excelsheets(file$datapath,
                       skip_codecols = input$dropcode,
                       skip_empty_cols = input$dropempty)
  })
  ct_summary <- reactive({
    req(ct_datalist())
    sheet_summary(ct_datalist())# |> names()
  })
  
  hl_sheets <- reactive({
    req(ct_datalist())
    paste('Data sheets available in',input$file$name,':')
  })
  
  output$sheetlist <- renderText({
    hl_sheets()
  })
  
  output$sheets <- renderDataTable(
    tibble(Sheet=ct_datalist() |> names(),
           Columns=map_int(ct_datalist(),ncol),
           Rows=map_int(ct_datalist(),nrow)), 
    # extensions='FixedHeader',
    options = list(
      editable=FALSE,
      info=FALSE,
      fixedHeader=TRUE,
      pageLength=10,
      lengthMenu = list(c(5, 10, 15, 20, -1),
                        c('5', '10', '15', '20', 'all')),
      padding='0%',
      columnDefs = list(
        list(width = '50%', targets = 0), # first column
        list(width = '25%', targets = 1:2)
      ))
  )
  
  sheetpicked <- reactive({
    req(ct_datalist())
    selectInput(inputId = 'ct_sheetpicker',
                label = 'Select sheet to show:',
                choices = ct_datalist() |> names())
  })
  sheetpicked2 <- reactive({
    req(ct_datalist())
    selectInput(inputId = 'ct_sheetpicker2',
                label = 'Select sheet to show:',
                choices = ct_datalist() |> names())
  })
  typepicked <- reactive({
    req(ct_datalist(),
        sheetpicked())
    selectInput(inputId = 'ct_typepicker',
                label = 'Select data type to show:',
                choices = c("Categories", "Numbers", "Dates"),
                selected = "Categories")
  })
  columnpicked <- reactive({
    req(sheetpicked2())
    varSelectInput(inputId = 'ct_colpicked',
                   label = 'Select Column:',
                   width = '500px',
                   data = ct_datalist() |> pluck(input$ct_sheetpicker2))
  })
  
  splitLayout(
    output$sheetpicker <- 
      renderUI({
        sheetpicked()
      }),
    output$sheetpicker2 <- 
      renderUI({
        sheetpicked2()
      }),
    
    output$typepicker <- 
      renderUI({
        typepicked()
      }),
    output$columnpicker <- 
      renderUI({
        columnpicked()
      }),
    
    hl_columns <- reactive({
      req(sheetpicked(),typepicked())
      paste('Column information from',input$ct_sheetpicker,':')
    }),
    output$columnlist <- renderText({
      hl_columns()
    })
  )
  # ct_summary <- reactive({
  #   req(sheetpicked())
  #   sheet_summary(ct_datalist()) |> pluck(input$ct_sheetpicker)# |> names()
  # })
  output$sheetsummary <- renderDataTable(
    ct_summary() |> pluck(input$ct_sheetpicker)|> pluck(input$ct_typepicker), 
    options = list(pageLength=10,
                   lengthMenu = list(c(5, 10, 15, 20, -1),
                                     c('5', '10', '15', '20', 'all')),
                   columnDefs = list(
                     list(width = '40%', targets = 0), # first column
                     list(width = '10%', targets = 2:3)
                     #   list(width = '10%', targets = 2:3)   
                   )
    )
  )
  
  output$colexplore <- renderPlot(
    {
      Sys.sleep(3)
      plotdata <- reactiveValues(pdata=ct_datalist() |> pluck(input$ct_sheetpicker2))
      if(!is.numeric(plotdata$pdata[[input$ct_colpicked]])){
        plottmp1 <- ggplot(plotdata$pdata,aes(!!input$ct_colpicked))+
          geom_bar()
      }
      if(is.numeric(plotdata$pdata[[input$ct_colpicked]])){
        plottmp1 <- ggplot(plotdata$pdata,aes(!!input$ct_colpicked))+
          geom_histogram()
      }
      print(plottmp1)
    }
  )
  #   ct_columns <- reactive({
  #   req(sheetpicked())
  #   ct_datalist() |> pluck(input$ct_sheetpicker)
  # })
  # output$columns <- renderDataTable({
  #   ct_columns()
  # })
}

shinyApp(ui = ui, server = server)
