# Paired T-test (p values) for data. (Hope I've got this right)

#' Output paired T test table
#'
#' @param group1 The first group of paired data.
#' @param group2 The second group of paired data.
#'
#' @return tibble containing all values for t test
#'
#' @examples
#' outputTs(sleep$extra[1:10], sleep$extra[11:20])
#'


test_f <- function(d, sch = "All", classn = "All",testn = "All") {
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

data_type <- data %>%
  mutate(type = stringr::str_extract(post_title.1, ": .*"),
         test = stringr::str_extract(post_title.1, ".*:")) %>%
  mutate(type = stringr::str_extract(type, "[A-Z].*"))

filter_d <- test_f(data_type, testn = "Block 7 Topic 1:")
# Split into two data sets

filt_pre <- filter_d %>%
  filter(type == "Pre Test")

filt_post <- filter_d %>%
  filter(type == "Post Test")

d <- inner_join(filt_pre, filt_post, by = c("user_id", "school", "class", "test"))
e <- d[!duplicated(d), ]

outputTs <- function(group1, group2) {

  tt <- stats::t.test(group1, group2, paired = TRUE, conf.level = 0.99)
  Metric <- names(unlist(tt))
  dplyr::tibble(Metric, Value = unlist(tt))

}

outputTs(e$percentage.x, e$percentage.y)

# Cohen's D (effect size) for all pupils and sub groups in group.
# install.packages("lsr")

#' Cohen's D (effect size) as tibble
#'
#' @param group1 The first group of paired data.
#' @param group2 The second group of paired data.
#'
#' @return tibble containing test name and value
#'
#' @examples
#' cohenTibble(sleep$extra[1:10], sleep$extra[11:20])
cohenTibble <- function(group1, group2) {

  cohD <- lsr::cohensD(group1, group2)
  tibble(Metric = "Cohen's d", Value = cohD)

}

cohenTibble(s1, s2)

#' Combine test statistics
#'
#'  Combines the results of \code{cohenTibble} and \code{outputTs} into
#'  one tibble
#'
#' @param group1 The first group of paired data.
#' @param group2 The second group of paired data.
#'
#' @return tibble of test statistics
#'
#' @examples
#' combTestStats(sleep$extra[1:10], sleep$extra[11:20])
combTestStats <- function(group1, group2) {

  rbind(outputTs(group1, group2), cohenTibble(group1, group2))

}

# Average per test score all pupils and sub groups. Same post-test.

sum_data2 <- data %>%
  select(school, class, name, post_title.1, acc_gender, percentage) %>%
  group_by(school, class, post_title.1) %>%
  summarise(MEAN = mean(percentage), total = n())

##

mean_tests <- function(data_input, group = c("school", "class", "post_title.1")) {

data_input %>%
  select_(school, class, name, post_title.1, acc_gender, percentage) %>%
  group_by_(group) %>%
  summarise_(MEAN = mean(percentage), total = n())
}
vv <- mean_tests(data)

test <- data %>%
  mutate(type = stringr::str_extract(post_title.1, ": .*")) %>%
  mutate(type = stringr::str_extract(type, "[A-Z].*"))

filt_pp <- test %>%
  select(school, class, name, Type, post_title.1, acc_gender, percentage) %>%
  group_by(school, class, post_title.1, Type) %>%
  summarise(MEAN = mean(percentage), total = n())

#Number of topic completions (sum for each post-test.


#Number of pupils who have made progress, stayed the same, regressed.

#' Progress, no change, regressed
#'
#'  Takes in two vectors of paired test scores and gives a table
#'  counting the number that have a positive, negative or no change
#'
#' @param group1 The first group of paired data.
#' @param group2 The second group of paired data.
#' @param ceiling Maximum mark
#' @param threshold Exclusion threshold
#'
#' @return tibble of progress measure
#'
#' @examples
#' progressMeasure(sleep$extra[1:10], sleep$extra[11:20])
#'
progressMeasure <- function(group1, group2, ceiling, threshold) {

  diffP <- group2 - group1
  labels <- c("Regressed", "No Change", "Progressed")
  RNOP <- cut(diffP, breaks = c(-Inf, -1e-5, 1e-5, Inf),
      labels = labels)
  tibble(Value =  labels, Count = summary(RNOP))

}
