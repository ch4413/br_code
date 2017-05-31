library(dplyr)
library(readr)

options(shiny.maxRequestSize=30*1024^2)

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

    data_acf <- filt_sct(data(), sch = input$school, classn = input$class, testn = input$test)
    data_acfp <- get_pairs(data = data_acf)
    t_results <- paired_t(data_acfp$percentage.x, data_acfp$percentage.y)
    tdresults <- combTestStats(data_acfp$percentage.x, data_acfp$percentage.y)
    tdresults
  })
  
  progressData <- reactive({
    
    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))
    
    data_acf <- filt_sct(data(), sch = input$school, classn = input$class, testn = input$test)
    data_acfp <- get_pairs(data = data_acf)
    data_prog <- prog_measure(group1 = data_acfp$percentage.x, group2 = data_acfp$percentage.y)
    data_prog
    
  })
  
  meansData <- reactive({
    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))
    
    grouped_means <- mean_group(data(), grp.var1 = "school", grp.var2= "acc_gender")
    
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
  
  output$column_class <- renderUI({
    if (any(input$school %in% "All")) {
      selectizeInput("class",
                     "Select a Class",
                     c("All", unique(as.character(data()[["class"]]))), multiple = TRUE)
    }
    else {
      selectizeInput("class",
                     "Select a Class",
                     c("All", unique(as.character(data()[data()[["school"]] %in% input$school, ][["class"]]))),
                     multiple = TRUE)
    }
  })
  
  output$column_test <- renderUI({
    if (is.null(input$file)) {
      selectizeInput("test",
                     "Select a test",
                     c("All", unique(as.character(data()[["test"]]))), selected = input$test, multiple = TRUE)
    } else {
      selectizeInput("test",
                     "Select a test",
                     c("All", unique(as.character(data()[["test"]]))), selected = input$test, multiple = TRUE)
    }
  })

  output$progd <- shiny::renderDataTable({

    progressData()

  })
  
  output$meansd <- shiny::renderDataTable({
    
    meansData()
    
  })
  
  output$contents <- shiny::renderDataTable({
    
    getData()
    
  })
  
  output$data_view <- shiny::renderDataTable({
    
    data()
    
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
  
  filter_type <- function(tibble1, type_v) {
    tibble1 %>%
      filter(type %in% type_v)
  }
  
  get_pairs <- function(data) {
    filt_pre <- filter_type(data, type_v = "Pre Test")
    filt_post <- filter_type(data, type_v = "Post Test")
    
    d <- dplyr::inner_join(filt_pre, filt_post, by = c("user_id", "school", "class", "test"))
    e <- d[!duplicated(d), ]
    e
  }
  
  paired_t <- function(group1, group2) {
    
    tt <- stats::t.test(group1, group2, paired = TRUE, conf.level = 0.95)
    Metric <- names(unlist(tt))
    dplyr::tibble(Metric, Value = unlist(tt))
    
  }
  
  cohenTibble <- function(group1, group2) {
    
    cohD <- lsr::cohensD(group1, group2)
    tibble(Metric = "Cohen's d", Value = cohD)
    
  }
  
  combTestStats <- function(group1, group2) {
    
    rbind(paired_t(group1, group2), cohenTibble(group1, group2))
    
  }
  
  prog_measure <- function(group1, group2, ceiling = 100, threshold = 0.05) {
    
    diffP <- group2 - group1
    labels <- c("Regressed", "No Change", "Progressed")
    RNOP <- cut(diffP, breaks = c(-Inf, -1e-5, 1e-5, Inf),
                labels = labels)
    tibble(Value =  labels, Count = summary(RNOP))
    
  }
  
  mean_group <- function(tibble1, grp.var1 = "school", grp.var2 = "school", grp.var3 = "school") {
    
    dots <- list(~mean(percentage))
    
    tibble1 %>%
      group_by_(grp.var1, grp.var2,grp.var3) %>%
      summarise_(total = ~n(), .dots = dots)
  }

}
