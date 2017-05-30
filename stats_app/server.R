library(dplyr)
library(readr)

server <- function(input, output) {

  data <- reactive({

    inFile <- input$file1

    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    dat_a <- read_data(inFile$datapath)
    data_ac <- add_test_cols(dat_a)
    data_ac
  })

  getData <- reactive({

    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    data_acf <- filt_sct(data(), sch = input$school)
    data_acf
  })

  output$column_school <- renderUI({
    if (is.null(input$file)) {
      selectizeInput("school",
                     "Select a School",
                     c("All", unique(as.character(data()[["school"]]))), selected = input$school, multiple = TRUE)
    } else {
      selectizeInput("school",
                     "Select a School",
                     c("All", unique(as.character(data()[["school"]]))), selected = input$school, multiple = TRUE)
    }
  })



  output$contents <- shiny::renderDataTable({

    getData()


  })

  output$downloadData <- downloadHandler(

    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },

    content = function(file) {

      write.csv(getData(), file)

    })

  # 1
  read_data <- function(filepath) {
    readr::read_delim(file = filepath, delim = ";")
  }
  # 2
  add_test_cols <- function(tibble1) {
    tibble1 %>%
      mutate(type = stringr::str_extract(post_title_1, ": .*"),
             test = stringr::str_extract(post_title_1, ".*:")) %>%
      mutate(type = stringr::str_extract(type, "[A-Z].*"))

  }
  # Filter data as appropriate

  filt_sct <- function(d, sch = "All", classn = "All",testn = "All") {
    if (any(sch != "All")) {
      d <- d %>%
        filter(school %in% sch)
    }
    if (any(classn != "All")) {
      d <- d %>%
        filter(class %in% classn)
    }
    if (any(testn != "All")) {
      d <- d %>%
        filter(test %in% testn)
    }
    d %>%
      select(user_id, school, class, test, type, percentage)
  }

}
