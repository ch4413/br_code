options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(theme = "bootstrap.css",
img(src="https://media.licdn.com/media/AAEAAQAAAAAAAAhXAAAAJGFkNGYxOGJkLTM0MTYtNDAwMS1iYWVjLThkZmJhZTdjYjJhMQ.png", width = "40%", height = "40%", align = "right"),
  #shiny::titlePanel("Bedrock Data Platform"),
  h1(" "),
  h1(" "),
  h1(" "),
  tabsetPanel(
    tabPanel("Data Viewer", fluidPage(
      sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv')),
        tags$hr()
      ),
      mainPanel(
        shiny::dataTableOutput('data_view')
      )
    )
  )
),
tabPanel("T Test", fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("column_school"),
      uiOutput("column_class"),
      uiOutput("column_test"),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      shiny::dataTableOutput('contents')
    )
  )
)
),
tabPanel("Progess", fluidPage(
  sidebarLayout(
    sidebarPanel(
      #uiOutput("column_school"),
      #uiOutput("column_class"),
      #uiOutput("column_test"),
      #downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      shiny::dataTableOutput('progd')
    )
  )
)
),
tabPanel("Means", fluidPage(
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      shiny::dataTableOutput('meansd')
    )
  )
)
)

)
)
