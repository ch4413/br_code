### Cohen's D

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

cohenTibble(group1 = data_acfp$percentage.x, group2 = data_acfp$percentage.y)

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

combTestStats(data_acfp$percentage.x, data_acfp$percentage.y)
