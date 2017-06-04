options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(theme = "bootstrap.css",
img(src="https://media.licdn.com/media/AAEAAQAAAAAAAAhXAAAAJGFkNGYxOGJkLTM0MTYtNDAwMS1iYWVjLThkZmJhZTdjYjJhMQ.png", width = "40%", height = "40%", align = "right"),
  #shiny::titlePanel("Bedrock Data Platform"),
  h2("Data Analysis Platform"),
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
tabPanel("t Test", fluidPage(
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
tabPanel("Mean Score Report", fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("grp.var1"),
      uiOutput("grp.var2"),
      uiOutput("grp.var3"),
      downloadButton('downloadGroup', 'Download')
    ),
    mainPanel(
      shiny::dataTableOutput('meansd')
    )
  )
)
),
tabPanel("Progess Report", fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("prog_school"),
      uiOutput("prog_class"),
      uiOutput("prog_test"),
      downloadButton('downloadProgress', 'Download')
    ),
    mainPanel(
      shiny::dataTableOutput('progd')
    )
  )
)
)
)
)
