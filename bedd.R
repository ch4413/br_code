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
outputTs <- function(group1, group2) {

  tt <- stats::t.test(group1, group2, paired = TRUE, conf.level = 0.99)
  Metric <- names(unlist(tt))
  dplyr::tibble(Metric, Value = unlist(tt))

}

s1 <- sleep$extra[1:10]
s2 <- sleep$extra[11:20]

outputTs(s1, s2)

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

test <- data %>%
  mutate(Type = stringr::str_extract(post_title.1, ": .*")) %>%
  mutate(Type = stringr::str_extract(Type, "[A-Z].*"))

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
