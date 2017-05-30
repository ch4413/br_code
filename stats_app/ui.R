ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv')),
        tags$hr(),
        uiOutput("column_school"),
        uiOutput("column_class"),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        shiny::dataTableOutput('contents')
      )
    )
  )
)
