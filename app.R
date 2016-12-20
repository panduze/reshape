# Data Reshapping Tool
# Author: Pierre Anduze

# This app as for an aim to provide a friendly-user interface of the "gather" function (tidyr package).
## app.R ##

library(shiny)
library(shinydashboard)
library(tidyr)


ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(title = "Data reshapping Tool"),
  
  # SIDEBAR
  dashboardSidebar(
    fileInput("file1", "Upload a dataset",
                                   accept=c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
    selectInput("cols", "Columns:", choices = NULL, multiple = TRUE),
    textInput("key", "Insert new Key column name:"),
    textInput("value", "Insert new data name:")
    ),
    
  # BODY
  dashboardBody(
    
    box(title = "Reshapped dataset:",
        width = 11,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        dataTableOutput("contents2")),
    
    downloadButton("downloadData","Download reshapped dataset")
  )
)


server <- function(input, output, session) {
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE, sep = ";", dec = ".")
    updateSelectInput(session, "cols", choices = names(data))
    data
  })
  
  key <- reactive({
    if(input$key == "") "Key"
    else input$key
  })
  
  value <- reactive({
    if(input$value == "") "Value"
    else input$value
  })
  
  dataTidy <- reactive({
    validate(need(input$file1 != "", "Please upload a data set on the left panel."))
    tidyr::gather_(myData(), key_col = key(),
                 value_col = value(),
                 gather_cols = input$cols)
  })
                                                                      
  output$contents2 <- renderDataTable({
  dataTidy()
  }, options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                    pageLength = 15, scrollX = TRUE, scrollY = "450px")) 
  
 
  output$downloadData <- downloadHandler(
    filename = function() {paste(Sys.Date(), "_Tidy_", input$file1, sep="")},
    content = function(file) { write.table(dataTidy(), file, sep = ";", dec =".", row.names = FALSE)}
  )
}

shinyApp(ui = ui, server = server)

 
