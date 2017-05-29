### T test Pipeline


# 1 Load in data
# 2 Add columns for test and test_type
# 3 Filter data as appropriate
# 4 split data into pre, post, join and remove duplicates
# 5 Apply paired t-test and give formatted results

### Cohen's D

# 6 Calculate D using paired data

# 7 Results from 5 & 6
# 8 Save in .csv file

# Load in data

read_data <- function(filepath) {
  readr::read_delim(file = filepath, delim = ";")
}

#setwd("/Users/chughes/Downloads/")

dat_a <- read_data(file.choose())

# Add columns for test and test_type

add_test_cols <- function(tibble1) {
  tibble1 %>%
    mutate(type = stringr::str_extract(post_title_1, ": .*"),
           test = stringr::str_extract(post_title_1, ".*:")) %>%
    mutate(type = stringr::str_extract(type, "[A-Z].*"))

}

data_ac <- add_test_cols(dat_a)

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

data_acf <- filt_sct(data_ac, testn = "Block 7 Topic 1:")

# split data into pre, post, join and remove duplicates

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

data_acfp <- get_pairs(data = data_acf)

# Apply paired t-test and give formatted results

paired_t <- function(group1, group2) {

  tt <- stats::t.test(group1, group2, paired = TRUE, conf.level = 0.95)
  Metric <- names(unlist(tt))
  dplyr::tibble(Metric, Value = unlist(tt))

}

t_results <- paired_t(data_acfp$percentage.x, data_acfp$percentage.y)

# Export data frame along with filters that were applied


### Just the commands

# 1 Load in data
dat_a <- read_data(file.choose())

# 2 Add columns for test and test_type
data_ac <- add_test_cols(dat_a)

# 3 Filter data as appropriate
data_acf <- filt_sct(data_ac, testn = "Block 7 Topic 1:")

# 4 split data into pre, post, join and remove duplicates
data_acfp <- get_pairs(data = data_acf)

# 5 Apply paired t-test and give formatted results
paired_t(data_acfp$percentage.x, data_acfp$percentage.y)

### Cohen's D

# 6 Calculate D using paired data

cohenTibble <- function(group1, group2) {

  cohD <- lsr::cohensD(group1, group2)
  tibble(Metric = "Cohen's d", Value = cohD)

}

cohenTibble(group1 = data_acfp$percentage.x, group2 = data_acfp$percentage.y)

# 7 Results from 5 & 6

combTestStats <- function(group1, group2) {

  rbind(paired_t(group1, group2), cohenTibble(group1, group2))

}

combTestStats(data_acfp$percentage.x, data_acfp$percentage.y)

# 8 Save in .csv file

save_csv <- function(data, filepath) {
  readr::write_csv(x = data, path = filepath)
}


### Everything rolled into one function

run_t_tests <- function(filepath,
                        filters = list(school = "All", class = "All", testn = "All")) {
  settings <- tibble(Metric = names(unlist(filters)), Value = unlist(filters))

  dat_a <- read_data(filepath)
  data_ac <- add_test_cols(dat_a)
  data_acf <- filt_sct(data_ac,
                       sch = filters$school, classn = filters$class, testn = filters$testn)
  data_acfp <- get_pairs(data = data_acf)

  tdresults <- combTestStats(data_acfp$percentage.x, data_acfp$percentage.y)

  save_csv(data = rbind(tdresults, settings),
           filepath = paste0("ttest_", as.integer(lubridate::now()), ".csv"))
}

run_t_tests(file.choose(), filters = list(school = "All", class = "All", testn = "Block 7 Topic 2:"))
