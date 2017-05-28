###############################################################
##         R-Shiny App: Bedrock Learning Demo App            ##
###############################################################
##        Alex Hughes          ##           28/07/2016       ##
###############################################################
# A Program to load data from csv (MySQL in future) and allow #
# custom subsets to be exported through options.              #
# Options to export vertical and horizontal data structures   #
# In future, will be able to release plots                    #
# In future, will allow more scope for custom reports         #
###############################################################

# options(shiny.maxRequestSize=30*1024^2)  - setting edit

#######################
## 0. Pre-Processing ##
#######################
#Dummy dataset load
#dset <- read.csv('/opt/BIOSTAT/home_ext/hughesa6/alex/OASIS_16072016a.csv', header=T)

#Load required libraries
library(shiny)
library(xlsx)
library(DT)
library(readr)

#########################
## 1. User Interface   ##
#########################
#Current options
# INPUTS
# Load File, load a csv into the application
# Select Column  - DUMMY TEST drop down
# Select School  - dropdown on load. Default ALL
# Select Class   - dropdown on School select. Default ALL
# Select Display - radio. Still to program, transposes data display
# Select Test    - checkbox on csv load (subset on school, not class)

# OUTPUTS
# Data_Table     - Partial display (1:50 fixed) of input options (to fix)
# Download       - Produce xlsx file on click, full version of screen display

ui <- shinyUI(
  mainPanel(
    tabsetPanel(
    tabPanel("Data Filter", fluidPage(

  titlePanel("CSV Splitter"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload csv file", accept="text/csv"),
      uiOutput("column_ui"),
      uiOutput("column_school"),
      uiOutput("column_class"),
      uiOutput("display"),
	    uiOutput("test_date"),
      uiOutput("column_test"),
      downloadButton("download")
    ),

    mainPanel(
      tableOutput("data_table")
    )
  )
)),
tabPanel("Statistics Reports",
         fluidPage(
           titlePanel("Statistics Reports"),

           sidebarLayout(
             sidebarPanel(
               fileInput("file_2", "Upload csv file", accept="text/csv"),
               downloadButton("download_test")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Data", fluidPage(
                   dataTableOutput("data_t2")
                 )
                 ),
                 tabPanel("Statistics", fluidPage(
                   h1("test")
                 )
                 )
             )
           )
             )
           )
         )
           )
)
)

#################
## 2. Server   ##
#################
# Elements of note
# data() - reactive dataset which is loaded, all routines use this
# output$data_table is the object that is displayed, note [1:50]
# data_table() is the table that is exported, currently matches
#              output object, need to work on a better way to program it.

server <- shinyServer(function(input, output) {

  data <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      return(read.csv(input$file$datapath, header=TRUE))
    }
  })

  output$data_t2 <- renderDataTable({

    inFile <- input$file_2

    if (is.null(inFile))
      return(data.frame( Values = "No data available"))

    read_delim(inFile$datapath, delim = ";")
  })

  output$column_ui <- renderUI({
    selectInput("column", "Select a column to split by unique values", unique(names(data())))
  })

  output$column_school <- renderUI({
    if (is.null(input$file)) {
      selectInput("school", "Select a School", unique(as.character(data()[["school2"]])))
    } else {
      selectInput("school", "Select a School", c("ALL", unique(as.character(data()[["school2"]]))))
    }
  })

  output$display <- renderUI({
    radioButtons("display", "Dataset display", c("Vertical", "Horizontal") )
  })

  output$column_class <- renderUI({
    if (is.null(input$file)) {
      selectInput("class", "Select a Class", unique(as.character(data()[["class2"]])))
    } else
      if (input$school=="ALL") {
        selectInput("class", "Select a Class", "ALL")
      }

    else {
      selectInput("class", "Select a Class", c("ALL", unique(as.character(data()[data()[["school2"]] == input$school , ][["class2"]]))))
    }
  })

  output$test_date <- renderUI({
    checkboxGroupInput("date_active", label="Set Date Range", c("check!"), c("check!"))
#    dateRangeInput("date", label="Date Range", start="2014-01-01", end=Sys.Date())
  })

  output$column_test <- renderUI({
    if (is.null(input$file)) {
      checkboxGroupInput("test", "Select a Test", sort(unique(as.character(data()[["post_title.1"]]))),
                         sort(unique(as.character(data()[["post_title.1"]]))) )

    } else
      if (input$school=="ALL") {
        checkboxGroupInput("test", "Select a Test", sort(unique(as.character(data()[["post_title.1"]]))),
                           sort(unique(as.character(data()[["post_title.1"]]))) )
      } else
      {
        checkboxGroupInput("test", "Select a Test", sort(unique(as.character(data()[data()[["school2"]] == input$school , ][["post_title.1"]]))),
                           sort(unique(as.character(data()[data()[["school2"]] == input$school , ][["post_title.1"]]))) )
      }
  })

  #output$column_test <- renderUI({
  #
  #  checkboxGroupInput("test", "Select a Test", unique(as.character(data()[["post_title.1"]])),
  #                                              unique(as.character(data()[["post_title.1"]])) )
  #})

  #Transposing function - no exit codes yet!
  transpose <- function(dset) {
    cut <- dset[ , c("user_id", "post_title.1", "percentage") ]
    #remove duplicates
    min <- cut[!duplicated(cut[ , c("post_title.1", "user_id"  )]  ) , ]
    min_t <- reshape(min, timevar="post_title.1", idvar="user_id", direction="wide")
    #create classmap
    map <- unique(dset[ , c("display_name", "user_id", "class2", "acc_ethnicity",
                            "acc_pp", "acc_sen", "acc_misc", "acc_gender") ])
    dupchk <- map[duplicated(map[ , 2]) , ]    #This should be empty!!! If not data issues, check code and data
    mapped <- merge(map, min_t , by="user_id")
    names(mapped) <- gsub("percentage.", "", names(mapped) )
    names(mapped) <- gsub("acc_"       , "", names(mapped) )
    s_index  <- dim(map)[2] + 1
    e_index  <- length(names(mapped))
    test_list <- sort(names(mapped[s_index:e_index]))
   #test_list <- sort(param_list[start:length(param_list)])
    s_mapped <- mapped[ order(mapped$class2, mapped$display_name) , ]
  }

  output$data_table <- renderTable({
    if (is.null(input$display)){
         NULL
       } else
    if (input$display=="Vertical") {
        if (is.null(input$file)) {
          ds <- data()[data()[["post_title.1"]] %in% input$test, ][1:50, ]
        } else

        if (input$school=="ALL" && input$class=="ALL") {
          ds <- data()[data()[["post_title.1"]] %in% input$test, ][1:50 , ]
		  names(ds) <- gsub("acc_"  , "", names(ds))
		  ds
        } else

        #      if (input$school=="ALL" && input$class!="ALL") {
        #        data()[data()[["class2"    ]] == input$class]
        #      } else

        if (input$school!="ALL" && input$class=="ALL") {
          ds <- data()[data()[["school2"  ]] == input$school &
                   data()[["post_title.1"]] %in% input$test, ][1:50 ,  ]
		  names(ds) <- gsub("acc_"  , "", names(ds))
		  ds
        } else

        if (input$school!="ALL" && input$class!="ALL") {
          ds <- data()[data()[["school2"  ]] == input$school &
                  data()[["class2"   ]] == input$class  &
                  data()[["post_title.1"]] %in% input$test , ][1:50 , ]
		  names(ds) <- gsub("acc_"  , "", names(ds))
		  ds
            #print(input$test)
         }
    }  else

    if (input$display=="Horizontal") {
        if (is.null(input$file)) {
           data()
        } else

        if (input$school=="ALL" && input$class=="ALL") {
          ds <- data()[data()[["post_title.1"]] %in% input$test, ]
          transpose(ds)[1:25 , ]
        } else

        if (input$school!="ALL" && input$class=="ALL") {
          ds <- data()[data()[["school2"  ]] == input$school &
                       data()[["post_title.1"]] %in% input$test, ]
          transpose(ds)[1:25 , ]
        } else

        if (input$school!="ALL" && input$class!="ALL") {
          ds <- data()[data()[["school2"  ]] == input$school &
                       data()[["class2"   ]] == input$class  &
                       data()[["post_title.1"]] %in% input$test , ]
          transpose(ds)[1:25 , ]
           #print(input$test)
        }

    }
  })

  data_table <- reactive({
    if (input$display=="Vertical") {
       if (is.null(input$file)) {
           data()[data()[["post_title.1"]] %in% input$test, ]
       } else

       if (input$school=="ALL" && input$class=="ALL") {
          data()[data()[["post_title.1"]] %in% input$test, ]
       } else

        #    if (input$school=="ALL" && input$class!="ALL") {
        #      data()[data()[["class2"    ]] == input$class]
        #    } else

       if (input$school!="ALL" && input$class=="ALL") {
            data()[data()[["school2"  ]] == input$school &
                   data()[["post_title.1"]] %in% input$test, ]
       } else

       if (input$school!="ALL" && input$class!="ALL") {
           data()[data()[["school2"  ]] == input$school &
                  data()[["class2"   ]] == input$class  &
                  data()[["post_title.1"]] %in% input$test , ]
            #print(input$test)
       }

  } else

  if (input$display=="Horizontal") {
      if (is.null(input$file)) {
        data()
      } else

      if (input$school=="ALL" && input$class=="ALL") {
         ds <- data()[data()[["post_title.1"]] %in% input$test, ]
         transpose(ds)
      } else

      if (input$school!="ALL" && input$class=="ALL") {
          ds <- data()[data()[["school2"  ]] == input$school &
                       data()[["post_title.1"]] %in% input$test, ]
          transpose(ds)
       } else

       if (input$school!="ALL" && input$class!="ALL") {
           ds <- data()[data()[["school2"  ]] == input$school &
                        data()[["class2"   ]] == input$class  &
                        data()[["post_title.1"]] %in% input$test , ]
           transpose(ds)
             #print(input$test)
        }

    }

  })


  output$download <- downloadHandler(
    filename = "result.xlsx",
    #dset <- data_table,
    content = function(file) {
      write.xlsx(data_table(), file, "check")
    }
  )


})

############################
## 3. Run the application ##
############################
shinyApp(ui = ui, server = server)

