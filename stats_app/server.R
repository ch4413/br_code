library(dplyr)
library(readr)

options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {

  data <- reactive({

    inFile <- input$file1

    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    data_a <- read_data(inFile$datapath)
    ### Remove All Post Tests aggregate
    data_ab <- data_a %>%
      dplyr::filter(name != "All Post Tests")

    data_ac <- add_test_cols(data_ab)
    data_ac<- match_pp(data_ac)
    data_ac %>%
      select(-acc_pp)
    })



  getData <- reactive({

    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    data_acf <- filt_sct(data(), sch = input$school, classn = input$class, testn = input$test)
    data_acfp <- get_pairs(data = data_acf)
    tdresults <- combTestStats(data_acfp$percentage.y, data_acfp$percentage.x)
    tdresults
  })

  progressData <- reactive({

    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    data_acf <- filt_sct(data(), sch = input$schoolpg, classn = input$classpg, testn = input$testpg)
    data_acfp <- get_pairs(data = data_acf)
    data_prog <- prog_measure(group1 = data_acfp$percentage.x, group2 = data_acfp$percentage.y)
    data_prog

  })

  meansData <- reactive({
    if (is.null(input$file1))
      return(dplyr::tibble(Data = "No data Available"))

    paired <- get_pairs(data())

    d1 <- paired[, 1:28]
    d2 <- cbind(paired[,29:dim(paired)[2]], paired[, c("user_id", "school", "class", "test")])

    d1 <- d1 %>%
      dplyr::select(user_id, school, class, test, everything())

    d2 <- d2 %>%
      dplyr::select(user_id, school, class, test, everything())

    names <- data() %>%
      dplyr::select(user_id, school, class, test, everything()) %>%
      names()

    names(d1) <- names
    names(d2) <- names

    dd <- rbind(d1, d2)

    grouped_means <- mean_group(dd,
                                grp.var1 = input$group1,
                                grp.var2 = input$group2,
                                grp.var3 = input$group3,
                                grp.var4 = input$group4,
                                grp.var5 = input$group5
                                )

  })

  output$grp.var1 <- renderUI({
    if (is.null(input$file)) {
      selectInput("group1",
                     "Select a value to group by",
                    names(data()))
    } else {
      selectInput("group1",
                     "Select a value to group by",
                     names(data()))
    }
  })

  output$grp.var2 <- renderUI({
    if (is.null(input$file)) {
      selectInput("group2",
                  "Select a value to group by",
                  names(data()))
    } else {
      selectInput("group2",
                  "Select a value to group by",
                  names(data()))
    }
  })

  output$grp.var3 <- renderUI({
    if (is.null(input$file)) {
      selectInput("group3",
                  "Select a value to group by",
                  names(data()))
    } else {
      selectInput("group3",
                  "Select a value to group by",
                  names(data()))
    }
  })

  output$grp.var4 <- renderUI({
    if (is.null(input$file)) {
      selectInput("group4",
                  "Select a value to group by",
                  names(data()))
    } else {
      selectInput("group4",
                  "Select a value to group by",
                  names(data()))
    }
  })

  output$grp.var5 <- renderUI({
    if (is.null(input$file)) {
      selectInput("group5",
                  "Select a value to group by",
                  names(data()))
    } else {
      selectInput("group5",
                  "Select a value to group by",
                  names(data()))
    }
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

  ########## Progress filters ###############

  output$prog_school <- renderUI({
    if (is.null(input$file)) {
      selectizeInput("schoolpg",
                     "Select a School",
                     c("All", unique(as.character(data()[["school"]]))), selected = input$schoolpg, multiple = TRUE)
    } else {
      selectizeInput("schoolpg",
                     "Select a School",
                     c("All", unique(as.character(data()[["school"]]))), selected = input$schoolpg, multiple = TRUE)
    }
  })

  output$prog_class <- renderUI({
    if (any(input$schoolpg %in% "All")) {
      selectizeInput("classpg",
                     "Select a Class",
                     c("All", unique(as.character(data()[["class"]]))), multiple = TRUE)
    }
    else {
      selectizeInput("classpg",
                     "Select a Class",
                     c("All", unique(as.character(data()[data()[["school"]] %in% input$schoolpg, ][["class"]]))),
                     multiple = TRUE)
    }
  })

  output$prog_test <- renderUI({
    if (is.null(input$file)) {
      selectizeInput("testpg",
                     "Select a test",
                     c("All", unique(as.character(data()[["test"]]))), selected = input$test, multiple = TRUE)
    } else {
      selectizeInput("testpg",
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
      metrics <- tibble(Metric = c("School", "Class", "Test"),
                        Value = c(
                          input_return(input$school),
                          input_return(input$class),
                          input_return(input$test)
                        ))

      write.csv(rbind(getData(), metrics), file)
      })

  output$downloadProgress <- downloadHandler(

    filename = function() {
      paste("data_progress-", Sys.Date(), ".csv", sep="")
    },


    content = function(file) {
      metrics <- tibble(Value = c("School", "Class", "Test"),
                        Count = c(
                          input_return(input$schoolpg),
                          input_return(input$classpg),
                          input_return(input$testpg)
                        ))

      write.csv(rbind(progressData(), metrics), file)

    })

  output$downloadGroup <- downloadHandler(

    filename = function() {
      paste("data_means-", Sys.Date(), ".csv", sep="")
    },

    content = function(file) {

      write.csv(meansData(), file)

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

  mean_group <- function(tibble1,
                         grp.var1 = "school", grp.var2 = "school",
                         grp.var3 = "school", grp.var4 = "school",
                         grp.var5 = "school") {

    dots <- list(~mean(percentage))

    tibble1 %>%
      group_by_(grp.var1, grp.var2,grp.var3) %>%
      summarise_(total = ~n(), .dots = dots)
  }

  input_return <- function(input) {
    if(is.null(input)) {
      "All"
    }
    else {
      as.character(paste0(input, collapse = ", "))
    }
  }

  match_pp <- function(data) {
    look_up <- dplyr::tibble( acc_pp = c(NA, "Yes", "No", "n", "F", "T", "Y", "N", "PP", "no", "y", "yes", "NULL"),
                              pp = c("No", "Yes", "No", "No","No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "No"))

    data %>%
      dplyr::left_join(y = look_up)

  }

}
